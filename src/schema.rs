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

pub fn register_scalar<'a>(
    schema: SchemaBuilder,
    source_scalar: &'a ScalarType<&'a str>,
) -> Result<SchemaBuilder> {
    let mut s = Scalar::new(source_scalar.name);
    if let Some(description) = source_scalar.description.as_ref() {
        s = s.description(description);
    }

    let s = source_scalar
        .directives
        .iter()
        .try_fold(s, |s, source_directive| {
            let directive = source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            );
            anyhow::Ok(s.directive(directive))
        })?;

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

    let i = source_interface
        .directives
        .iter()
        .try_fold(i, |i, source_directive| {
            let directive = source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            );
            anyhow::Ok(i.directive(directive))
        })?;

    let i = source_interface
        .implements_interfaces
        .iter()
        .try_fold(i, |i, interface| anyhow::Ok(i.implement(*interface)))?;

    let i = source_interface
        .fields
        .iter()
        .try_fold(i, |i, source_field| {
            let field_type = map_type_to_typeref(&source_field.field_type);

            let field = InterfaceField::new(source_field.name, field_type);

            let field = source_field.arguments.clone().into_iter().try_fold(
                field,
                |f, source_argument| {
                    let argument = InputValue::new(
                        source_argument.name,
                        map_type_to_typeref(&source_argument.value_type),
                    );
                    let argument = source_argument
                        .directives
                        .into_iter()
                        .filter(|d| !MOCK_DIRECTIVES.contains(&d.name))
                        .try_fold(argument, |a, source_directive| {
                            let d = source_directive.arguments.into_iter().fold(
                                Directive::new(source_directive.name),
                                |d, (k, v)| {
                                    let v = parser_value_to_query_value(v);
                                    d.argument(k, v)
                                },
                            );
                            anyhow::Ok(a.directive(d))
                        })?;
                    anyhow::Ok(f.argument(argument))
                },
            )?;

            anyhow::Ok(i.field(field))
        })?;

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

    let u = source_union
        .directives
        .iter()
        .try_fold(u, |u, source_directive| {
            let directive = source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            );
            anyhow::Ok(u.directive(directive))
        })?;

    let u = source_union
        .types
        .iter()
        .try_fold(u, |u, ut| anyhow::Ok(u.possible_type(*ut)))?;

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

    let en = source_enum
        .directives
        .iter()
        .try_fold(en, |en, source_directive| {
            let directive = source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            );
            anyhow::Ok(en.directive(directive))
        })?;

    let en = source_enum.values.iter().try_fold(en, |en, value| {
        let mut item = EnumItem::new(value.name);
        if let Some(description) = value.description.as_ref() {
            item = item.description(description);
        }

        anyhow::Ok(en.item(item))
    })?;

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

    let i = source_input_object
        .directives
        .iter()
        .try_fold(i, |i, source_directive| {
            let directive = source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            );
            anyhow::Ok(i.directive(directive))
        })?;

    let i = source_input_object.fields.iter().try_fold(i, |i, field| {
        let field_type = map_type_to_typeref(&field.value_type);
        anyhow::Ok(i.field(InputValue::new(field.name, field_type)))
    })?;

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

    let o = source_object
        .directives
        .into_iter()
        .filter(|d| !MOCK_DIRECTIVES.contains(&d.name))
        .try_fold(o, |o, source_directive| {
            if source_directive.name == "key" {
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

                let o = if resolvable {
                    o.key(key_fields)
                } else {
                    o.unresolvable(key_fields)
                };

                return anyhow::Ok(o);
            }

            let directive = source_directive.arguments.into_iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(k.to_string(), v)
                },
            );
            Ok(o.directive(directive))
        })?;

    let o = source_object
        .implements_interfaces
        .iter()
        .try_fold(o, |o, interface| anyhow::Ok(o.implement(*interface)))?;

    let o = source_object
        .fields
        .into_iter()
        .try_fold(o, |object, source_field| {
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

            let field =
                source_field
                    .arguments
                    .into_iter()
                    .try_fold(field, |f, source_argument| {
                        let argument = InputValue::new(
                            source_argument.name,
                            map_type_to_typeref(&source_argument.value_type),
                        );
                        let argument = source_argument
                            .directives
                            .into_iter()
                            .filter(|d| !MOCK_DIRECTIVES.contains(&d.name))
                            .try_fold(argument, |a, source_directive| {
                                let d = source_directive.arguments.into_iter().fold(
                                    Directive::new(source_directive.name),
                                    |d, (k, v)| {
                                        let v = parser_value_to_query_value(v);
                                        d.argument(k, v)
                                    },
                                );
                                anyhow::Ok(a.directive(d))
                            })?;
                        anyhow::Ok(f.argument(argument))
                    })?;

            let field = source_field
                .directives
                .into_iter()
                .filter(|d| !MOCK_DIRECTIVES.contains(&d.name))
                .try_fold(field, |f, source_directive| {
                    let d = source_directive.arguments.into_iter().fold(
                        Directive::new(source_directive.name),
                        |d, (k, v)| {
                            let v = parser_value_to_query_value(v);
                            d.argument(k, v)
                        },
                    );
                    anyhow::Ok(f.directive(d))
                })?;

            anyhow::Ok(object.field(field))
        })?;

    let res = schema.register(o);

    Ok(res)
}
