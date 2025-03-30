use async_graphql::{dynamic::*, extensions::Tracing, http::GraphiQLSource, Name, Value};
use async_graphql_axum::GraphQL;
use axum::{
    response::{self, IntoResponse},
    routing::{get, post},
    Router,
};
use clap::Parser;
use core::panic;
use fake::{
    faker::lorem::en::Words,
    rand::{self, seq::IndexedRandom},
    uuid::UUIDv4,
    Fake,
};
use graphql_parser::schema::{
    Definition, Document, EnumType, InputObjectType, InterfaceType, ObjectType, ScalarType,
    TypeDefinition, UnionType,
};
use indexmap::IndexMap;
use notify::Watcher;
use std::{collections::HashMap, path::PathBuf, sync::Arc};
use tokio::{
    net::TcpListener,
    select,
    sync::{mpsc, Mutex},
};
use tower_service::Service;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Layer};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Commandline Args
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, default_value_t = false)]
    pretty_traces: bool,

    #[arg(long, env = "PORT", default_value_t = 8080)]
    port: usize,

    #[arg(short, long)]
    mock_schema: PathBuf,
}

async fn graphiql() -> impl IntoResponse {
    response::Html(GraphiQLSource::build().endpoint("/").finish())
}

/// Setup tracing and logging
fn setup_tracing(args: &Args) {
    let default_filter = EnvFilter::builder()
        .with_default_directive(LevelFilter::INFO.into())
        .from_env_lossy();
    if args.pretty_traces {
        tracing_subscriber::registry()
            .with(tracing_forest::ForestLayer::default().with_filter(default_filter))
            .init();
    } else {
        tracing_subscriber::registry()
            .with(tracing_subscriber::fmt::layer().with_filter(default_filter))
            .init();
    };
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

fn map_field_to_mock_field<'a>(field: &graphql_parser::schema::Field<'a, &'a str>) -> MockField {
    use graphql_parser::query::Type;

    match &field.field_type {
        Type::NamedType(_) => MockField::Nullable(map_type_to_mock_contents(
            &field.field_type,
            &field.directives,
        )),
        Type::ListType(inner) => MockField::Nullable(map_type_to_mock_contents(
            &Type::ListType(inner.clone()),
            &field.directives,
        )),
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

                MockContents::Type(Box::new(MockType::String { min, max }))
            }
            "Int" => MockContents::Type(Box::new(MockType::Int)),
            "Float" => MockContents::Type(Box::new(MockType::Float)),
            "Boolean" => MockContents::Type(Box::new(MockType::Boolean)),
            "ID" => MockContents::Type(Box::new(MockType::ID)),
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

struct MockerBuilder {
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
    fn insert_obj(&mut self, obj_name: String, field_name: String, mock_fn: MockField) {
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
    fn insert_enum(&mut self, enum_name: String, values: Vec<String>) {
        self.enums.insert(enum_name, values);
    }

    fn build(self) -> Mocker {
        Mocker {
            data: Arc::new(self.data),
            enums: Arc::new(self.enums),
        }
    }
}

#[derive(Clone, Debug)]
struct Mocker {
    data: Arc<HashMap<String, HashMap<String, MockField>>>,
    enums: Arc<HashMap<String, Vec<String>>>,
}

impl Mocker {
    fn builder() -> MockerBuilder {
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

    fn _get_obj(&self, obj_name: &str, field_name: &str) -> async_graphql::Result<Option<Value>> {
        let field = self
            .data
            .get(obj_name)
            .and_then(|hm| hm.get(field_name))
            .cloned();

        match field {
            Some(mock_field) => self._resolve_mock_field(mock_field),
            None => todo!(),
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
                MockType::String { min, max } => Ok(Some(Value::from(
                    Words(min..max + 1).fake::<Vec<String>>().join(" "),
                ))),
                MockType::Int => Ok(Some(Value::from((1..1000).fake::<i32>()))),
                MockType::Float => Ok(Some(Value::from((1.0..1000.0).fake::<f64>()))),
                MockType::Boolean => Ok(Some(Value::from(true))),
                MockType::ID => Ok(Some(Value::from(UUIDv4.fake::<String>()))),
                MockType::Ref(obj_name) => {
                    // Ref might be to an enum type
                    if let Some(val) = self._get_enum(obj_name.clone()).unwrap() {
                        return Ok(Some(val));
                    }

                    let mut res: IndexMap<Name, Value> = IndexMap::new();
                    let keys = self
                        .data
                        .get(&obj_name)
                        .map(|hm| hm.keys())
                        .unwrap_or_default();

                    for k in keys {
                        if let Some(v) = self._get_obj(&obj_name, k).unwrap() {
                            res.insert(Name::new(k), v.clone());
                        }
                    }

                    Ok(Some(Value::from(res)))
                }
            },
        }
    }

    fn _resolve_mock_field(&self, mock_field: MockField) -> async_graphql::Result<Option<Value>> {
        match mock_field {
            MockField::NonNull(mock_contents) => self._resolve_mock_contents(mock_contents),
            MockField::Nullable(mock_contents) => {
                if !rand::random_bool(9.0 / 10.0) {
                    return Ok(None);
                }

                self._resolve_mock_contents(mock_contents)
            }
        }
    }
}

#[derive(Clone, Debug)]
enum MockField {
    NonNull(MockContents),
    Nullable(MockContents),
}

#[derive(Clone, Debug)]
enum MockContents {
    Field(Box<MockField>),
    Type(Box<MockType>),
    List(Box<MockFieldList>),
}

#[derive(Clone, Debug)]
enum MockType {
    String { min: usize, max: usize },
    Int,
    Float,
    Boolean,
    ID,
    Ref(String),
}

#[derive(Clone, Debug)]
struct MockFieldList {
    contents: MockContents,
    min: usize,
    max: usize,
}

fn register_scalar<'a>(
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

fn register_interface<'a>(
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

    schema.register(source_interface.fields.iter().fold(i, |i, field| {
        let field_type = map_type_to_typeref(&field.field_type);
        i.field(InterfaceField::new(source_interface.name, field_type))
    }))
}

fn register_union<'a>(
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

fn register_enum<'a>(schema: SchemaBuilder, source_enum: &'a EnumType<&'a str>) -> SchemaBuilder {
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

fn register_input_object<'a>(
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

fn register_object<'a>(
    schema: SchemaBuilder,
    source_object: &'a ObjectType<&'a str>,
) -> SchemaBuilder {
    let mut o = Object::new(source_object.name);
    if let Some(description) = source_object.description.as_ref() {
        o = o.description(description);
    }

    let o = source_object
        .directives
        .iter()
        .fold(o, |o, source_directive| {
            o.directive(source_directive.arguments.iter().fold(
                Directive::new(source_directive.name),
                |d, (k, v)| {
                    let v = parser_value_to_query_value(v);
                    d.argument(*k, v)
                },
            ))
        });

    let o = source_object
        .implements_interfaces
        .iter()
        .fold(o, |o, interface| o.implement(*interface));

    schema.register(source_object.fields.iter().fold(o, |object, source_field| {
        let field_type = map_type_to_typeref(&source_field.field_type);

        let source_object_name = source_object.name.to_string();
        let field_name = source_field.name.to_string();

        // println!(
        //     "Directives ({}-{}): {:#?}",
        //     source_object.name, source_field.name, source_field.directives
        // );

        let field = Field::new(field_name.clone(), field_type, move |ctx| {
            let source_object_name = source_object_name.clone();
            let field_name = field_name.clone();

            ctx.data::<Mocker>()
                .unwrap()
                .get_obj(source_object_name, field_name)
        });

        let field = source_field
            .directives
            .iter()
            .fold(field, |f, source_directive| {
                let d = source_directive.arguments.iter().fold(
                    Directive::new(source_directive.name),
                    |d, (k, v)| {
                        let v = parser_value_to_query_value(v);
                        d.argument(*k, v)
                    },
                );
                f.directive(d)
            });

        object.field(field)
    }))
}

fn parser_value_to_query_value<'a>(v: &'a graphql_parser::query::Value<&'a str>) -> Value {
    match v {
        graphql_parser::query::Value::Variable(_) => todo!(),
        graphql_parser::query::Value::Int(number) => Value::from(number.as_i64().unwrap()),
        graphql_parser::query::Value::Float(f) => Value::from(*f),
        graphql_parser::query::Value::String(s) => Value::from(s),
        graphql_parser::query::Value::Boolean(b) => Value::from(*b),
        graphql_parser::query::Value::Null => Value::Null,
        graphql_parser::query::Value::Enum(e) => Value::Enum(Name::new(e)),
        graphql_parser::query::Value::List(values) => Value::List(
            // V could be any value type, probably need to recurse
            values
                .iter()
                .map(|v| parser_value_to_query_value(v))
                .collect(),
        ),
        graphql_parser::query::Value::Object(btree_map) => {
            Value::Object(IndexMap::from_iter(
                btree_map
                    .iter()
                    // V could be any value type, probably need to recurse
                    .map(|(k, v)| (Name::new(k), parser_value_to_query_value(v))),
            ))
        }
    }
}

fn build_handler(path: &PathBuf) -> anyhow::Result<GraphQL<Schema>> {
    let sdl_str = std::fs::read_to_string(path)?;
    let sdl: Document<&str> = graphql_parser::parse_schema(&sdl_str)?;

    let root_query_name = sdl
        .definitions
        .iter()
        .find_map(|d| match d {
            Definition::SchemaDefinition(sd) => sd.query,
            _ => None,
        })
        .expect("No root query found");

    let mutation_query_name = sdl.definitions.iter().find_map(|d| match d {
        Definition::SchemaDefinition(sd) => sd.mutation,
        _ => None,
    });

    let subscription_query_name = sdl.definitions.iter().find_map(|d| match d {
        Definition::SchemaDefinition(sd) => sd.subscription,
        _ => None,
    });

    let mut mock_builder = Mocker::builder();

    for source_enum in sdl
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::TypeDefinition(TypeDefinition::Enum(en)) => Some(en),
            _ => None,
        })
    {
        mock_builder.insert_enum(
            source_enum.name.to_string(),
            source_enum
                .values
                .iter()
                .map(|s| s.name.to_string())
                .collect(),
        );
    }

    for source_object in sdl
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::TypeDefinition(TypeDefinition::Object(source_object)) => {
                Some(source_object)
            }
            _ => None,
        })
    {
        for field in &source_object.fields {
            mock_builder.insert_obj(
                source_object.name.to_string(),
                field.name.to_string(),
                map_field_to_mock_field(field),
            );
        }
    }

    let mocker = mock_builder.build();
    // println!("Mocker: {:#?}", mocker);

    let schema = sdl
        .definitions
        .clone()
        .iter()
        .fold(
            Schema::build(
                root_query_name,
                mutation_query_name,
                subscription_query_name,
            )
            .data(mocker),
            |schema, definition| match definition {
                Definition::SchemaDefinition(schema_definition) => schema,
                Definition::TypeDefinition(source_type_definition) => {
                    match source_type_definition {
                        TypeDefinition::Scalar(source_scalar) => {
                            register_scalar(schema, source_scalar)
                        }
                        TypeDefinition::Object(source_object) => {
                            register_object(schema, source_object)
                        }
                        TypeDefinition::Interface(source_interface) => {
                            register_interface(schema, source_interface)
                        }
                        TypeDefinition::Union(source_union) => register_union(schema, source_union),
                        TypeDefinition::Enum(source_enum) => register_enum(schema, source_enum),
                        TypeDefinition::InputObject(source_input_object) => {
                            register_input_object(schema, source_input_object)
                        }
                    }
                }
                Definition::TypeExtension(type_extension) => unimplemented!(),
                Definition::DirectiveDefinition(source_directive_definition) => schema,
            },
        )
        .enable_federation()
        .extension(Tracing)
        .finish()?;
    // println!("BUILT SCHEMA: {}", schema.sdl());

    Ok(GraphQL::new(schema))
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    setup_tracing(&args);

    let addr = format!("0.0.0.0:{}", args.port);

    let handler = Arc::new(Mutex::new(build_handler(&args.mock_schema)?));

    let app = Router::new().route(
        "/",
        get(graphiql).post_service(post({
            let handler = handler.clone();
            move |req| async move {
                let mut h = handler.lock().await;
                h.call(req).await
            }
        })),
    );

    let handler = handler.clone();
    let path = args.mock_schema.clone();
    tokio::spawn(async move {
        let (tx, mut rx) = mpsc::channel::<notify::Result<notify::Event>>(100);
        let handle = tokio::runtime::Handle::current();

        let mut watcher = notify::recommended_watcher(move |res| {
            handle.block_on(async {
                tx.send(res).await.unwrap();
            })
        })
        .unwrap();

        watcher
            .watch(path.clone().as_path(), notify::RecursiveMode::NonRecursive)
            .unwrap();

        loop {
            select! {
                Some(res) = rx.recv() => {
                    match res {
                        Ok(event) => {
                            match event.kind {
                                notify::EventKind::Modify(notify::event::ModifyKind::Data(_)) => {
                                    println!("File Change Detected");
                                    match build_handler(&path) {
                                        Ok(new_handler) => {
                                            let mut handler_guard = handler.lock().await;
                                            *handler_guard = new_handler;
                                            println!("Schema Reloaded");
                                        },
                                        Err(e) => println!("Constructing Schema Failed: {:?}", e),
                                    }
                                },
                                event => println!("Unhandled File Event: {:?}", event),
                            }
                        },
                        Err(e) => {
                            panic!("File Watcher Error: {}", e);
                        },
                    }

                }
            }
        }
    });

    println!("GraphiQL IDE: http://{}", addr);

    axum::serve(TcpListener::bind(addr).await?, app).await?;

    Ok(())
}
