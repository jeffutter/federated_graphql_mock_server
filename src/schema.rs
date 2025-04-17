use async_graphql::{dynamic::*, Name, Value};
use core::panic;
use fake::{
    faker::lorem::en::Words,
    rand::{self, seq::IndexedRandom},
    uuid::UUIDv4,
    Fake,
};
use futures_core::stream::Stream;
use graphql_parser::schema::{
    EnumType, InputObjectType, InterfaceType, ObjectType, ScalarType, UnionType,
};
use indexmap::IndexMap;
use std::{borrow::Borrow, collections::HashMap, pin::Pin, sync::Arc};

const MOCK_DIRECTIVES: &[&str] = &["word", "listLength", "null", "values"];

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

pub fn map_field_to_mock_field<'a>(
    field: &graphql_parser::schema::Field<'a, &'a str>,
) -> MockField {
    use graphql_parser::query::Type;

    let null = field.directives.iter().find(|d| d.name == "null");

    let null_probability: f64 = null
        .and_then(|d| d.arguments.iter().find(|(n, _)| *n == "probability"))
        .and_then(|(_, v)| match v {
            graphql_parser::query::Value::Int(number) => number.as_i64().map(|x| x as f64),
            graphql_parser::query::Value::Float(f) => Some(*f),
            graphql_parser::query::Value::Null => Some(0.0),
            _ => panic!(),
        })
        .unwrap_or(0.05);

    match &field.field_type {
        Type::NamedType(_) => MockField::Nullable(
            1.0 - null_probability,
            map_type_to_mock_contents(&field.field_type, &field.directives),
        ),
        Type::ListType(inner) => MockField::Nullable(
            1.0 - null_probability,
            map_type_to_mock_contents(&Type::ListType(inner.clone()), &field.directives),
        ),
        Type::NonNullType(inner) => {
            MockField::NonNull(map_type_to_mock_contents(inner, &field.directives))
        }
    }
}

fn map_type_to_mock_contents<'a>(
    ty: &graphql_parser::schema::Type<'a, &'a str>,
    directives: &Vec<graphql_parser::schema::Directive<'a, &'a str>>,
) -> MockContents {
    use graphql_parser::query::Type;

    match ty {
        Type::NamedType(name) => match *name {
            "String" => {
                if let Some(values) = directives.iter().find(|d| d.name == "values") {
                    let selection: Vec<String> = values
                        .arguments
                        .iter()
                        .find(|(n, _)| *n == "select")
                        .map(|(_, v)| match v {
                            graphql_parser::query::Value::List(list) => list
                                .iter()
                                .map(|v| match v {
                                    graphql_parser::query::Value::String(string) => string.clone(),
                                    _ => panic!(),
                                })
                                .collect::<Vec<_>>(),
                            _ => panic!(),
                        })
                        .unwrap();

                    return MockContents::Type(Box::new(MockType::SelectString { selection }));
                };

                let words = directives.iter().find(|d| d.name == "words");

                let word_count: Option<usize> = words
                    .and_then(|d| d.arguments.iter().find(|(n, _)| *n == "count"))
                    .map(|(_, v)| match v {
                        graphql_parser::query::Value::Int(number) => {
                            number.as_i64().unwrap().try_into().unwrap()
                        }
                        graphql_parser::query::Value::Null => 0,
                        _ => panic!(),
                    });
                let min: Option<usize> = words
                    .and_then(|d| d.arguments.iter().find(|(n, _)| *n == "min"))
                    .map(|(_, v)| match v {
                        graphql_parser::query::Value::Int(number) => {
                            number.as_i64().unwrap().try_into().unwrap()
                        }
                        graphql_parser::query::Value::Null => 0,
                        _ => panic!(),
                    });
                let max: Option<usize> = words
                    .and_then(|d| d.arguments.iter().find(|(n, _)| *n == "max"))
                    .map(|(_, v)| match v {
                        graphql_parser::query::Value::Int(number) => {
                            number.as_i64().unwrap().try_into().unwrap()
                        }
                        graphql_parser::query::Value::Null => 0,
                        _ => panic!(),
                    });

                let (min, max) = match (word_count, min, max) {
                    (Some(n), _, _) => (n, n),
                    (None, Some(min), Some(max)) => (min, max),
                    (None, None, Some(max)) => (1, max),
                    _ => (1, 1),
                };

                if max < min {
                    panic!("Max: {max} is less than Min: {min}");
                }

                MockContents::Type(Box::new(MockType::RandomString { min, max }))
            }
            "Int" => MockContents::Type(Box::new(MockType::Int)),
            "Float" => MockContents::Type(Box::new(MockType::Float)),
            "Boolean" => MockContents::Type(Box::new(MockType::Boolean)),
            "ID" => {
                if let Some(values) = directives.iter().find(|d| d.name == "values") {
                    let selection: Vec<String> = values
                        .arguments
                        .iter()
                        .find(|(n, _)| *n == "select")
                        .map(|(_, v)| match v {
                            graphql_parser::query::Value::List(list) => list
                                .iter()
                                .map(|v| match v {
                                    graphql_parser::query::Value::String(string) => string.clone(),
                                    _ => panic!(),
                                })
                                .collect::<Vec<_>>(),
                            _ => panic!(),
                        })
                        .unwrap();

                    return MockContents::Type(Box::new(MockType::SelectID { selection }));
                };
                MockContents::Type(Box::new(MockType::ID))
            }
            s => MockContents::Type(Box::new(MockType::Ref(s.to_string()))),
        },
        Type::ListType(ref inner) => {
            let list_length = directives.iter().find(|d| d.name == "listLength");
            let min: usize = list_length
                .and_then(|d| d.arguments.iter().find(|(n, _)| *n == "min"))
                .map(|(_, v)| match v {
                    graphql_parser::query::Value::Int(number) => {
                        number.as_i64().unwrap().try_into().unwrap()
                    }
                    graphql_parser::query::Value::Null => 0,
                    _ => panic!(),
                })
                .unwrap_or(0);
            let max: usize = list_length
                .and_then(|d| d.arguments.iter().find(|(n, _)| *n == "max"))
                .map(|(_, v)| match v {
                    graphql_parser::query::Value::Int(number) => {
                        number.as_i64().unwrap().try_into().unwrap()
                    }
                    graphql_parser::query::Value::Null => 10,
                    _ => panic!(),
                })
                .unwrap_or(0);

            if max < min {
                panic!("Max: {max} is less than Min: {min}");
            }

            MockContents::List(Box::new(MockFieldList {
                contents: map_type_to_mock_contents(inner, directives),
                min,
                max,
            }))
        }
        Type::NonNullType(ref inner) => MockContents::Field(Box::new(MockField::NonNull(
            map_type_to_mock_contents(inner, directives),
        ))),
    }
}

pub struct MockerBuilder {
    data: HashMap<String, HashMap<String, MockField>>,
    enums: HashMap<String, Vec<String>>,
}

impl MockerBuilder {
    fn new() -> Self {
        Self {
            data: HashMap::new(),
            enums: HashMap::new(),
        }
    }
    pub fn insert_obj(&mut self, obj_name: String, field_name: String, mock_fn: MockField) {
        self.data
            .entry(obj_name)
            .and_modify(|hm| {
                hm.insert(field_name.clone(), mock_fn.clone());
            })
            .or_insert_with(|| {
                let mut hm = HashMap::new();
                hm.insert(field_name.clone(), mock_fn.clone());
                hm
            });
    }

    pub fn insert_enum(&mut self, enum_name: String, values: Vec<String>) {
        self.enums.insert(enum_name, values);
    }

    pub fn build(self) -> Mocker {
        Mocker {
            data: Arc::new(self.data),
            enums: Arc::new(self.enums),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Mocker {
    data: Arc<HashMap<String, HashMap<String, MockField>>>,
    enums: Arc<HashMap<String, Vec<String>>>,
}

impl Mocker {
    pub fn builder() -> MockerBuilder {
        MockerBuilder::new()
    }

    fn _get_enum(&self, enum_name: String) -> async_graphql::Result<Option<Value>> {
        match self.enums.get(&enum_name) {
            Some(en) => {
                let v = en.choose(&mut fake::rand::rng()).unwrap();
                Ok(Some(Value::from(v)))
            }
            None => Ok(None),
        }
    }

    fn get_obj(&self, obj_name: String, field_name: String) -> FieldFuture<'_> {
        FieldFuture::new(async move { self._get_obj(&obj_name, &field_name) })
    }

    fn get_obj_stream(&self, obj_name: String, field_name: String) -> SubscriptionFieldFuture<'_> {
        SubscriptionFieldFuture::new(async move {
            let stream = async_stream::stream! {
                yield Ok(self._get_obj(&obj_name, &field_name).unwrap().unwrap());
            };

            Ok(stream)
        })
    }

    fn _get_obj(&self, obj_name: &str, field_name: &str) -> async_graphql::Result<Option<Value>> {
        let field = self
            .data
            .get(obj_name)
            .and_then(|hm| hm.get(field_name))
            .cloned();

        match field {
            Some(mock_field) => self._resolve_mock_field(mock_field),
            None => todo!("Obj: {obj_name}, Field: {field_name}"),
        }
    }

    fn _resolve_mock_contents(
        &self,
        mock_contents: MockContents,
    ) -> async_graphql::Result<Option<Value>> {
        match mock_contents {
            MockContents::Field(mock_field) => self._resolve_mock_field(*mock_field),
            MockContents::List(mock_field_list) => {
                let MockFieldList { contents, min, max } = *mock_field_list;

                let n = fake::rand::random_range(min..=max);
                let mut out = Vec::with_capacity(n);

                for _ in 0..n {
                    out.push(
                        self._resolve_mock_contents(contents.clone())
                            .unwrap()
                            .unwrap(),
                    );
                }

                Ok(Some(Value::from(out)))
            }
            MockContents::Type(ty) => match *ty {
                MockType::SelectString { selection } => Ok(Some(Value::from(
                    selection.choose(&mut rand::rng()).unwrap(),
                ))),
                MockType::RandomString { min, max } => Ok(Some(Value::from(
                    Words(min..max + 1).fake::<Vec<String>>().join(" "),
                ))),
                MockType::Int => Ok(Some(Value::from((1..1000).fake::<i32>()))),
                MockType::Float => Ok(Some(Value::from((1.0..1000.0).fake::<f64>()))),
                MockType::Boolean => Ok(Some(Value::from(true))),
                MockType::ID => Ok(Some(Value::from(UUIDv4.fake::<String>()))),
                MockType::SelectID { selection } => Ok(Some(Value::from(
                    selection.choose(&mut rand::rng()).unwrap(),
                ))),
                MockType::Ref(obj_name) => {
                    // Ref might be to an enum type
                    if let Some(val) = self._get_enum(obj_name.clone()).unwrap() {
                        return Ok(Some(val));
                    }

                    // let mut res: IndexMap<Name, Value> = IndexMap::new();
                    // let keys = self
                    //     .data
                    //     .get(&obj_name)
                    //     .map(|hm| hm.keys())
                    //     .unwrap_or_default();
                    //
                    // for k in keys {
                    //     if let Some(v) = self._get_obj(&obj_name, k).unwrap() {
                    //         res.insert(Name::new(k), v.clone());
                    //     }
                    // }

                    // Ok(Some(Value::from(res)))

                    // Return an empty object, the resolvers on the fields will fill the object out
                    Ok(Some(Value::from(IndexMap::new())))
                }
            },
        }
    }

    fn _resolve_mock_field(&self, mock_field: MockField) -> async_graphql::Result<Option<Value>> {
        match mock_field {
            MockField::NonNull(mock_contents) => self._resolve_mock_contents(mock_contents),
            MockField::Nullable(null_probability, mock_contents) => {
                if !rand::random_bool(null_probability) {
                    return Ok(None);
                }

                self._resolve_mock_contents(mock_contents)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum MockField {
    NonNull(MockContents),
    Nullable(f64, MockContents),
}

#[derive(Clone, Debug)]
pub enum MockContents {
    Field(Box<MockField>),
    Type(Box<MockType>),
    List(Box<MockFieldList>),
}

#[derive(Clone, Debug)]
pub enum MockType {
    SelectString { selection: Vec<String> },
    RandomString { min: usize, max: usize },
    Int,
    Float,
    Boolean,
    ID,
    SelectID { selection: Vec<String> },
    Ref(String),
}

#[derive(Clone, Debug)]
pub struct MockFieldList {
    contents: MockContents,
    min: usize,
    max: usize,
}

pub fn register_scalar<'a>(
    schema: SchemaBuilder,
    source_scalar: &'a ScalarType<&'a str>,
) -> SchemaBuilder {
    let mut s = Scalar::new(source_scalar.name);
    if let Some(description) = source_scalar.description.as_ref() {
        s = s.description(description);
    }

    let s = source_scalar
        .directives
        .iter()
        .fold(s, |s, source_directive| {
            s.directive(source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            ))
        });

    schema.register(s)
}

pub fn register_interface<'a>(
    schema: SchemaBuilder,
    source_interface: &'a InterfaceType<&'a str>,
) -> SchemaBuilder {
    let mut i = Interface::new(source_interface.name);
    if let Some(description) = source_interface.description.as_ref() {
        i = i.description(description);
    }

    let i = source_interface
        .directives
        .iter()
        .fold(i, |i, source_directive| {
            i.directive(source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            ))
        });

    let i = source_interface
        .implements_interfaces
        .iter()
        .fold(i, |i, interface| i.implement(*interface));

    schema.register(source_interface.fields.iter().fold(i, |i, source_field| {
        let field_type = map_type_to_typeref(&source_field.field_type);
        i.field(InterfaceField::new(source_field.name, field_type))
    }))
}

pub fn register_union<'a>(
    schema: SchemaBuilder,
    source_union: &'a UnionType<&'a str>,
) -> SchemaBuilder {
    let mut u = Union::new(source_union.name);
    if let Some(description) = source_union.description.as_ref() {
        u = u.description(description);
    }

    let u = source_union
        .directives
        .iter()
        .fold(u, |u, source_directive| {
            u.directive(source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            ))
        });

    schema.register(
        source_union
            .types
            .iter()
            .fold(u, |u, ut| u.possible_type(*ut)),
    )
}

pub fn register_enum<'a>(
    schema: SchemaBuilder,
    source_enum: &'a EnumType<&'a str>,
) -> SchemaBuilder {
    let mut en = Enum::new(source_enum.name);
    if let Some(description) = source_enum.description.as_ref() {
        en = en.description(description);
    }

    let en = source_enum
        .directives
        .iter()
        .fold(en, |en, source_directive| {
            en.directive(source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            ))
        });

    schema.register(
        source_enum
            .values
            .iter()
            .fold(en, |en, value| en.item(EnumItem::new(value.name))),
    )
}

pub fn register_input_object<'a>(
    schema: SchemaBuilder,
    source_input_object: &'a InputObjectType<&'a str>,
) -> SchemaBuilder {
    let mut i = InputObject::new(source_input_object.name);
    if let Some(description) = source_input_object.description.as_ref() {
        i = i.description(description);
    }

    let i = source_input_object
        .directives
        .iter()
        .fold(i, |i, source_directive| {
            i.directive(source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            ))
        });

    schema.register(source_input_object.fields.iter().fold(i, |i, field| {
        let field_type = map_type_to_typeref(&field.value_type);
        i.field(InputValue::new(field.name, field_type))
    }))
}

pub fn register_subscription_object<'a>(
    schema: SchemaBuilder,
    source_object: ObjectType<'a, &'a str>,
) -> SchemaBuilder {
    let mut o = Subscription::new(source_object.name);

    if let Some(description) = source_object.description.as_ref() {
        o = o.description(description);
    }

    // Subscriptions can't have key directives. Or any directives?

    schema.register(
        source_object
            .fields
            .into_iter()
            .fold(o, |object, source_field| {
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

                        ctx.data::<Mocker>()
                            .unwrap()
                            .get_obj_stream(source_object_name, field_name)
                    });

                // Subscription Fields can't have directives

                object.field(field)
            }),
    )
}

pub fn register_object<'a>(
    schema: SchemaBuilder,
    source_object: ObjectType<'a, &'a str>,
) -> SchemaBuilder {
    let mut o = Object::new(source_object.name);
    if let Some(description) = source_object.description.as_ref() {
        o = o.description(description);
    }

    let o = source_object
        .directives
        .into_iter()
        .filter(|d| !MOCK_DIRECTIVES.contains(&d.name))
        .fold(o, |o, source_directive| {
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
                        graphql_parser::query::Value::String(ref s) => Some(s),
                        _ => None,
                    })
                    .unwrap();

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

                return o;
            }

            o.directive(source_directive.arguments.into_iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(k.to_string(), v)
                },
            ))
        });

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

            // println!(
            //     "Directives ({}-{}): {:#?}",
            //     source_object.name, source_field.name, source_field.directives
            // );

            let field = Field::new(field_name.clone(), field_type.clone(), move |ctx| {
                let source_object_name = source_object_name.clone();
                let field_name = field_name.clone();

                let parent_map = ctx.parent_value.as_value().and_then(|v| match v {
                    Value::Object(index_map) => Some(index_map),
                    _ => None,
                });

                if let Some(parent_map) = parent_map {
                    let field_name = &Name::new(field_name.clone());

                    if let Some(existing_value) = parent_map.get(field_name) {
                        return FieldFuture::new(async move {
                            Ok(Some::<FieldValue>(existing_value.to_owned().into()))
                        });
                    }
                }

                ctx.data::<Mocker>()
                    .unwrap()
                    .get_obj(source_object_name, field_name)
            });

            let field = source_field
                .directives
                .into_iter()
                .filter(|d| !MOCK_DIRECTIVES.contains(&d.name))
                .fold(field, |f, source_directive| {
                    let d = source_directive.arguments.into_iter().fold(
                        Directive::new(source_directive.name),
                        |d, (k, v)| {
                            let v = parser_value_to_query_value(v);
                            d.argument(k, v)
                        },
                    );
                    f.directive(d)
                });

            object.field(field)
        });

    println!("Registering: {:?}", o);
    schema.register(o)
}
