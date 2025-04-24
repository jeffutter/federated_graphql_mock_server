use crate::schema;
use async_graphql::{dynamic::*, extensions::Tracing, http::GraphiQLSource};
use async_graphql_axum::GraphQL;
use axum::{
    response::{self, IntoResponse},
    routing::{get, post},
    Router,
};
use core::panic;
use federated_graphql_mock_server::mock_graph::MockGraph;
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

    _build_handler(sdl_str).map(|(sdl, graphql)| {
        if let Some(output) = output {
            std::fs::write(output, sdl).unwrap();
        }
        graphql
    })
}

fn _build_handler(sdl_str: String) -> anyhow::Result<(String, GraphQL<Schema>)> {
    let sdl: Document<&str> = graphql_parser::parse_schema(&sdl_str)?;
    let mock_graph = MockGraph::builder().process_document(&sdl).build();

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

    let schema = sdl
        .definitions
        .clone()
        .iter()
        .fold(
            Schema::build("Query", mutation_query_name, subscription_query_name).data(mock_graph),
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

                        let res = ctx
                            .data::<MockGraph>()
                            .unwrap()
                            .resolve_obj(typename)
                            .unwrap();

                        trace!("Value: {:?}", res);

                        res.with_type(typename.to_string())
                    });

                    Ok(Some(FieldValue::list(values)))
                }
                .in_current_span(),
            )
        })
        .finish()?;

    let sdl = schema.sdl_with_options(async_graphql::SDLExportOptions::new().federation());

    Ok((sdl, GraphQL::new(schema)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{json, Value};

    #[tokio::test]
    async fn test_entity_query() {
        let schema = r#"
        type Query {
          user: User
        }

        type User @key(fields: "id") {
          id: ID!
          a: A!
          b: B
        }

        interface A {
          id: ID!
        }

        type AA implements A {
          id: ID!
          name: String
        }

        type AB implements A {
          id: ID!
          name: String
        }

        type B {
          id: ID!
        }
        "#;

        // Build the handler with our test schema
        let (_, mut graphql) = _build_handler(schema.to_string()).expect("Failed to build handler");

        // Create an entity query that requests User with its A and B fields
        let query = r#"
          query($representations: [_Any!]!) {
            _entities(representations: $representations) {
              ... on User {
                id
                a {
                  id
                  __typename
                }
                b {
                  id
                }
              }
            }
          }
        "#;

        // Create the variables with a representation for a User entity
        let request = json!({
            "query": query,
            "variables": {
                "representations": [
                    {
                        "__typename": "User",
                        "id": "user-123"
                    }
                ]
            }
        });

        // Create the GraphQL request
        // let request = GraphQLRequest::new(query.to_string(), Some("".to_string()), Some(variables));
        let http_request = axum::http::Request::builder()
            .method("POST")
            .uri("/")
            .header("content-type", "application/json")
            .body(serde_json::to_string(&request).unwrap())
            .unwrap();

        // Execute the query
        let response = graphql.call(http_request).await.unwrap();
        let body = axum::body::to_bytes(response.into_body(), 2048)
            .await
            .unwrap();
        let json: Value = serde_json::from_slice(&body).unwrap();

        // Verify the response contains the expected structure
        assert!(json.get("data").is_some());
        let entities = json["data"]["_entities"].as_array().unwrap();
        assert_eq!(entities.len(), 1);

        let user = &entities[0];
        assert!(user.get("id").is_some());
        assert!(user.get("a").is_some());
        assert!(user["a"].get("id").is_some());

        // B might be null sometimes due to the nullable field, so check conditionally
        if let Some(b) = user.get("b") {
            if !b.is_null() {
                assert!(b.get("id").is_some());
            }
        }
    }
}
