use anyhow::{Context, Result};
use async_graphql::{dynamic::*, Name, Value};
use futures_core::stream::Stream;
use graphql_parser::schema::{
    EnumType, InputObjectType, InterfaceType, ObjectType, ScalarType, UnionType,
};
use indexmap::IndexMap;
use std::{borrow::Borrow, collections::HashMap, pin::Pin};
use tracing::trace;

use crate::mock_graph::MockGraph;

const MOCK_DIRECTIVES: &[&str] = &["word", "listLength", "null", "values", "select", "count"];

fn parser_value_to_query_value<'a>(
    v: impl Borrow<graphql_parser::query::Value<'a, &'a str>>,
) -> Value {
    match v.borrow() {
        graphql_parser::query::Value::Variable(_) => todo!(),
        graphql_parser::query::Value::Int(number) => Value::from(number.as_i64().unwrap()),
        graphql_parser::query::Value::Float(f) => Value::from(*f),
        graphql_parser::query::Value::String(s) => Value::from(s),
        graphql_parser::query::Value::Boolean(b) => Value::from(*b),
        graphql_parser::query::Value::Null => Value::Null,
        graphql_parser::query::Value::Enum(e) => Value::Enum(Name::new(e)),
        graphql_parser::query::Value::List(values) => Value::List(
            // V could be any value type, possibly need to recurse
            values.iter().map(parser_value_to_query_value).collect(),
        ),
        graphql_parser::query::Value::Object(btree_map) => {
            Value::Object(IndexMap::from_iter(
                btree_map
                    .iter()
                    // V could be any value type, possibly need to recurse
                    .map(|(k, v)| (Name::new(k), parser_value_to_query_value(v))),
            ))
        }
    }
}

fn map_type_to_typeref<'a>(ty: &graphql_parser::query::Type<'a, &'a str>) -> TypeRef {
    match *ty {
        graphql_parser::query::Type::NamedType(name) => TypeRef::named(name),
        graphql_parser::query::Type::ListType(ref inner) => {
            TypeRef::List(Box::new(map_type_to_typeref(inner)))
        }
        graphql_parser::query::Type::NonNullType(ref inner) => {
            TypeRef::NonNull(Box::new(map_type_to_typeref(inner)))
        }
    }
}

/// Trait for types that can have directives applied to them
pub trait ApplyDirective {
    /// Apply a directive to this type
    fn apply_directive(self, directive: async_graphql::dynamic::Directive) -> Self;
}

pub trait ApplyDirectives: ApplyDirective {
    fn apply_directives<'a>(
        self,
        directives: &Vec<graphql_parser::schema::Directive<'a, &'a str>>,
    ) -> Self
    where
        Self: Sized,
    {
        let result = directives
            .iter()
            .filter(|d| !MOCK_DIRECTIVES.contains(&d.name))
            // Exclude key directives from all types, Objects will handle it separately
            .filter(|d| d.name != "key")
            .fold(self, |s, source_directive| {
                let directive = source_directive.arguments.iter().fold(
                    Directive::new(source_directive.name),
                    |d, (k, v)| {
                        let v = parser_value_to_query_value(v);
                        d.argument(*k, v)
                    },
                );
                s.apply_directive(directive)
            });

        result
    }
}

macro_rules! impl_apply_directives {
    ($($t:ty),*) => {
        $(
            impl ApplyDirective for $t {
                fn apply_directive(self, directive: async_graphql::dynamic::Directive) -> Self {
                    self.directive(directive)
                }
            }

            impl ApplyDirectives for $t {}
        )*
    };
}

// Implement for types that have a directive method
impl_apply_directives!(
    Scalar,
    Interface,
    Union,
    Enum,
    InputObject,
    Object,
    EnumItem,
    InputValue,
    Field,
    InterfaceField
);

pub trait ApplyArgument {
    fn apply_argument(self, directive: async_graphql::dynamic::InputValue) -> Self;
}

pub trait ApplyArguments: ApplyArgument {
    fn apply_arguments<'a>(
        self,
        arguments: &Vec<graphql_parser::schema::InputValue<'a, &'a str>>,
    ) -> Self
    where
        Self: Sized,
    {
        let result = arguments.iter().fold(self, |f, source_argument| {
            let mut argument = InputValue::new(
                source_argument.name,
                map_type_to_typeref(&source_argument.value_type),
            );
            if let Some(description) = source_argument.description.as_ref() {
                argument = argument.description(description);
            }
            if let Some(default_value) = source_argument.default_value.as_ref() {
                argument = argument.default_value(parser_value_to_query_value(default_value));
            }
            let argument = argument.apply_directives(&source_argument.directives);
            f.apply_argument(argument)
        });

        result
    }
}

macro_rules! impl_apply_arguments {
    ($($t:ty),*) => {
        $(
            impl ApplyArgument for $t {
                fn apply_argument(self, argument: async_graphql::dynamic::InputValue) -> Self {
                    self.argument(argument)
                }
            }

            impl ApplyArguments for $t {}
        )*
    };
}

// Implement for types that have a directive method
impl_apply_arguments!(Field, InterfaceField);

pub fn register_scalar<'a>(
    schema: SchemaBuilder,
    source_scalar: &'a ScalarType<&'a str>,
) -> Result<SchemaBuilder> {
    let mut s = Scalar::new(source_scalar.name);
    if let Some(description) = source_scalar.description.as_ref() {
        s = s.description(description);
    }

    let s = s.apply_directives(&source_scalar.directives);

    Ok(schema.register(s))
}

pub fn register_interface<'a>(
    schema: SchemaBuilder,
    source_interface: &'a InterfaceType<&'a str>,
) -> Result<SchemaBuilder> {
    let mut i = Interface::new(source_interface.name);
    if let Some(description) = source_interface.description.as_ref() {
        i = i.description(description);
    }

    let i = i.apply_directives(&source_interface.directives);

    let i = source_interface
        .implements_interfaces
        .iter()
        .fold(i, |i, interface| i.implement(*interface));

    let i = source_interface.fields.iter().fold(i, |i, source_field| {
        let field_type = map_type_to_typeref(&source_field.field_type);

        let mut field = InterfaceField::new(source_field.name, field_type);

        if let Some(description) = source_field.description.as_ref() {
            field = field.description(description);
        }

        let field = field.apply_arguments(&source_field.arguments);

        let field = field.apply_directives(&source_field.directives);

        i.field(field)
    });

    Ok(schema.register(i))
}

pub fn register_union<'a>(
    schema: SchemaBuilder,
    source_union: &'a UnionType<&'a str>,
) -> Result<SchemaBuilder> {
    let mut u = Union::new(source_union.name);
    if let Some(description) = source_union.description.as_ref() {
        u = u.description(description);
    }

    let u = u.apply_directives(&source_union.directives);

    let u = source_union
        .types
        .iter()
        .fold(u, |u, ut| u.possible_type(*ut));

    Ok(schema.register(u))
}

pub fn register_enum<'a>(
    schema: SchemaBuilder,
    source_enum: &'a EnumType<&'a str>,
) -> Result<SchemaBuilder> {
    let mut en = Enum::new(source_enum.name);
    if let Some(description) = source_enum.description.as_ref() {
        en = en.description(description);
    }

    let en = en.apply_directives(&source_enum.directives);

    let en = source_enum.values.iter().fold(en, |en, value| {
        let mut item = EnumItem::new(value.name);
        if let Some(description) = value.description.as_ref() {
            item = item.description(description);
        }
        let item = item.apply_directives(&value.directives);

        en.item(item)
    });

    Ok(schema.register(en))
}

pub fn register_input_object<'a>(
    schema: SchemaBuilder,
    source_input_object: &'a InputObjectType<&'a str>,
) -> Result<SchemaBuilder> {
    let mut i = InputObject::new(source_input_object.name);
    if let Some(description) = source_input_object.description.as_ref() {
        i = i.description(description);
    }

    let i = i.apply_directives(&source_input_object.directives);

    let i = source_input_object
        .fields
        .iter()
        .fold(i, |i, source_field| {
            let field_type = map_type_to_typeref(&source_field.value_type);
            let mut input_value = InputValue::new(source_field.name, field_type);
            if let Some(description) = source_field.description.as_ref() {
                input_value = input_value.description(description);
            }
            if let Some(default_value) = source_field.default_value.as_ref() {
                input_value = input_value.default_value(parser_value_to_query_value(default_value));
            }
            i.field(input_value)
        });

    Ok(schema.register(i))
}

pub fn register_subscription_object<'a>(
    schema: SchemaBuilder,
    source_object: ObjectType<'a, &'a str>,
) -> Result<SchemaBuilder> {
    let mut o = Subscription::new(source_object.name);

    if let Some(description) = source_object.description.as_ref() {
        o = o.description(description);
    }

    // Subscriptions can't have key directives. Or any directives?

    let o = source_object
        .fields
        .into_iter()
        .try_fold(o, |object, source_field| {
            let field_type = map_type_to_typeref(&source_field.field_type);

            let source_object_name = source_object.name.to_string();
            let field_name = source_field.name.to_string();

            let field =
                SubscriptionField::new(field_name.clone(), field_type.clone(), move |ctx| {
                    let source_object_name = source_object_name.clone();
                    let field_name = field_name.clone();

                    let parent_map = ctx.parent_value.as_value().and_then(|v| match v {
                        Value::Object(index_map) => Some(index_map),
                        _ => None,
                    });

                    if let Some(parent_map) = parent_map {
                        let field_name = &Name::new(field_name.clone());

                        if let Some(existing_value) = parent_map.get(field_name) {
                            return SubscriptionFieldFuture::new(async move {
                                let stream: Pin<
                                    Box<
                                        dyn Stream<
                                                Item = async_graphql::Result<
                                                    Value,
                                                    async_graphql::Error,
                                                >,
                                            > + Send,
                                    >,
                                > = Box::pin(async_stream::stream! {
                                    yield Ok(existing_value.to_owned());
                                });
                                Ok(stream)
                            });
                        }
                    }

                    // ctx.data::<Mocker>()
                    //     .unwrap()
                    //     .get_obj_stream(source_object_name, field_name)


                    SubscriptionFieldFuture::new(async move {
                        let stream = async_stream::stream! {
                            let res = ctx.data::<MockGraph>().unwrap().resolve_field(&source_object_name, &field_name).unwrap();
                            yield Ok(res);
                        };

                        Ok(stream)
                    })

                });

            // Subscription Fields can't have directives

            anyhow::Ok(object.field(field))
        })?;

    Ok(schema.register(o))
}

pub fn register_object<'a>(
    schema: SchemaBuilder,
    source_object: ObjectType<'a, &'a str>,
) -> Result<SchemaBuilder> {
    let mut o = Object::new(source_object.name);
    if let Some(description) = source_object.description.as_ref() {
        o = o.description(description);
    }

    // Apply all directives other than 'key'
    let o = o.apply_directives(&source_object.directives);

    // handle the 'key' directive
    let o = source_object
        .directives
        .into_iter()
        .filter(|d| d.name == "key")
        .try_fold(o, |o, source_directive| {
            let arguments: HashMap<String, graphql_parser::query::Value<'_, &str>> =
                source_directive
                    .arguments
                    .iter()
                    .map(|(k, v)| (k.to_string(), v.clone()))
                    .collect();

            let key_fields = arguments
                .get("fields")
                .and_then(|fields| match fields {
                    graphql_parser::query::Value::String(s) => Some(s),
                    _ => None,
                })
                .context(format!(
                    "Key not found on SourceObject: {}",
                    source_object.name
                ))?;

            let resolvable = arguments
                .get("resolvable")
                .and_then(|resolvable| match resolvable {
                    graphql_parser::query::Value::Boolean(b) => Some(*b),
                    _ => None,
                })
                .unwrap_or(true);

            if resolvable {
                anyhow::Ok(o.key(key_fields))
            } else {
                anyhow::Ok(o.unresolvable(key_fields))
            }
        })?;

    let o = source_object
        .implements_interfaces
        .iter()
        .fold(o, |o, interface| o.implement(*interface));

    let o = source_object
        .fields
        .into_iter()
        .fold(o, |object, source_field| {
            let field_type = map_type_to_typeref(&source_field.field_type);

            let source_object_name = source_object.name.to_string();
            let field_name = source_field.name.to_string();

            let field = Field::new(field_name.clone(), field_type.clone(), move |ctx| {
                let source_object_name = source_object_name.clone();
                trace!(
                    "Resolve Field: {source_object_name}.{field_name} - Args: {:?}",
                    ctx.args.as_index_map()
                );
                let field_name = field_name.clone();

                if let Some(Value::Object(parent_map)) = ctx.parent_value.as_value() {
                    let field_name = &Name::new(field_name.clone());

                    if let Some(existing_value) = parent_map.get(field_name) {
                        trace!("Parent Exists: {field_name} = {:?}", existing_value);
                        return FieldFuture::new(async move {
                            Ok(Some::<FieldValue>(existing_value.to_owned().into()))
                        });
                    }
                }

                FieldFuture::new(async move {
                    Ok(ctx
                        .data::<MockGraph>()
                        .unwrap()
                        .resolve_field(&source_object_name, &field_name))
                })
            });

            let field = match source_field.description.as_ref() {
                Some(description) => field.description(description),
                None => field,
            };

            let field = field.apply_arguments(&source_field.arguments);

            let field = field.apply_directives(&source_field.directives);

            object.field(field)
        });

    let res = schema.register(o);

    Ok(res)
}
