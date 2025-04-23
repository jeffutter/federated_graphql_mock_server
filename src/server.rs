use crate::schema::{self, Mocker};
use async_graphql::{dynamic::*, extensions::Tracing, http::GraphiQLSource, Value};
use async_graphql_axum::GraphQL;
use axum::{
    response::{self, IntoResponse},
    routing::{get, post},
    Router,
};
use core::panic;
use graphql_parser::schema::{Definition, Document, TypeDefinition};
use notify::Watcher;
use std::{path::PathBuf, sync::Arc};
use tokio::{
    net::TcpListener,
    select,
    sync::{mpsc, Mutex},
};
use tower_http::trace::TraceLayer;
use tower_service::Service;
use tracing::{trace, Instrument};

async fn graphiql() -> impl IntoResponse {
    response::Html(GraphiQLSource::build().endpoint("/").finish())
}

pub async fn start_server(
    addr: String,
    schema_path: PathBuf,
    output_path: Option<PathBuf>,
) -> anyhow::Result<()> {
    let handler = Arc::new(Mutex::new(build_handler(&schema_path, &output_path)?));

    let app = Router::new()
        .route(
            "/",
            get(graphiql).post_service(post({
                let handler = handler.clone();
                move |req| {
                    async move {
                        let mut h = handler.lock().await;
                        h.call(req).await
                    }
                    .in_current_span()
                }
            })),
        )
        .layer(TraceLayer::new_for_http());

    let handler = handler.clone();
    let path = schema_path.clone();
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
                                    match build_handler(&path, &output_path) {
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

fn build_handler(path: &PathBuf, output: &Option<PathBuf>) -> anyhow::Result<GraphQL<Schema>> {
    let sdl_str = std::fs::read_to_string(path)?;
    let sdl: Document<&str> = graphql_parser::parse_schema(&sdl_str)?;

    let root_query_name = sdl
        .definitions
        .iter()
        .find_map(|d| match d {
            Definition::SchemaDefinition(sd) => sd.query,
            _ => None,
        })
        .unwrap_or("Query");

    let mutation_query_name = sdl.definitions.iter().find_map(|d| match d {
        Definition::SchemaDefinition(sd) => sd.mutation,
        _ => None,
    });

    let subscription_query_name = sdl.definitions.iter().find_map(|d| match d {
        Definition::SchemaDefinition(sd) => sd.subscription,
        _ => None,
    });

    let mut mock_builder = schema::Mocker::builder();

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
            let source_obj_name = if source_object.name == root_query_name {
                "Query"
            } else {
                source_object.name
            };
            mock_builder.insert_obj(
                source_obj_name.to_string(),
                field.name.to_string(),
                schema::map_field_to_mock_field(field),
            );
        }
    }

    for source_interface in sdl
        .definitions
        .iter()
        .filter_map(|definition| match definition {
            Definition::TypeDefinition(TypeDefinition::Interface(source_interface)) => {
                Some(source_interface)
            }
            _ => None,
        })
    {
        let implementers = sdl
            .definitions
            .iter()
            .filter_map(|definition| match definition {
                Definition::TypeDefinition(TypeDefinition::Object(obj)) => {
                    if obj
                        .implements_interfaces
                        .iter()
                        .any(|o_i| o_i == &source_interface.name.to_string())
                    {
                        Some(obj.name.to_string())
                    } else {
                        None
                    }
                }
                _ => None,
            });
    }

    let mocker = mock_builder.build();
    // println!("Mocker: {:#?}", mocker);

    let schema = sdl
        .definitions
        .clone()
        .iter()
        .fold(
            Schema::build("Query", mutation_query_name, subscription_query_name).data(mocker),
            |schema, definition| match definition {
                Definition::SchemaDefinition(_schema_definition) => schema,
                Definition::TypeDefinition(source_type_definition) => {
                    match source_type_definition {
                        TypeDefinition::Scalar(source_scalar) => {
                            schema::register_scalar(schema, source_scalar)
                        }
                        TypeDefinition::Object(source_object) => {
                            let source_object = if source_object.name == root_query_name {
                                let mut source_object = source_object.clone();
                                source_object.name = "Query";
                                source_object
                            } else {
                                source_object.clone()
                            };
                            //TODO: Subscriptions probably don't actually work. Likely need to
                            // register an entirely different tree of objects for subscriptions
                            if Some(source_object.name) == subscription_query_name {
                                return schema::register_subscription_object(schema, source_object);
                            }
                            schema::register_object(schema, source_object)
                        }
                        TypeDefinition::Interface(source_interface) => {
                            schema::register_interface(schema, source_interface)
                        }
                        TypeDefinition::Union(source_union) => {
                            schema::register_union(schema, source_union)
                        }
                        TypeDefinition::Enum(source_enum) => {
                            schema::register_enum(schema, source_enum)
                        }
                        TypeDefinition::InputObject(source_input_object) => {
                            schema::register_input_object(schema, source_input_object)
                        }
                    }
                }
                Definition::TypeExtension(_type_extension) => unimplemented!(),
                Definition::DirectiveDefinition(_source_directive_definition) => schema,
            },
        )
        .enable_federation()
        .extension(Tracing)
        .entity_resolver(|ctx| {
            // trace!("Entity Resolver Args: {:?}", ctx.args.as_index_map());

            FieldFuture::new(
                async move {
                    let representations = ctx.args.try_get("representations")?.list()?;
                    trace!(
                        "Entity Resolver Representations: {:#?}",
                        representations.as_values_slice()
                    );

                    let values = representations.iter().map(|item| {
                        let item = item.object().unwrap();
                        let typename = item
                            .try_get("__typename")
                            .and_then(|value| value.string())
                            .unwrap();

                        let res = ctx.data::<Mocker>().unwrap().get_whole_obj(typename);

                        trace!("Value: {:?}", res);

                        res.with_type(typename.to_string())

                        // FieldValue::from(Value::from(item.as_index_map().to_owned()))
                        //     .with_type(typename.to_string())
                    });

                    Ok(Some(FieldValue::list(values)))
                }
                .in_current_span(),
            )
        })
        .finish()?;

    let sdl = schema.sdl_with_options(async_graphql::SDLExportOptions::new().federation());
    // let sdl = schema.sdl();
    // println!("BUILT SCHEMA: {}", sdl);
    if let Some(output) = output {
        std::fs::write(output, sdl).unwrap();
    }

    Ok(GraphQL::new(schema))
}
