use async_graphql::http::GraphiQLSource;
use axum::{
    response::{self, IntoResponse},
    routing::{get, post},
    Router,
};
use std::{collections::HashMap, path::PathBuf};
use tokio::net::TcpListener;
use tower_http::trace::TraceLayer;
use tower_service::Service;
use tracing::Instrument;

use crate::schema_handler::{Schema, SchemaHandler};
use crate::supergraph_config::SupergraphConfig;

async fn subgraph_graphiql(endpoint: &str) -> impl IntoResponse {
    response::Html(GraphiQLSource::build().endpoint(endpoint).finish())
}

/// Start the GraphQL server with the given schema files
pub async fn start_server(
    addr: String,
    schema_paths: HashMap<String, PathBuf>,
    output_path: Option<PathBuf>,
) -> anyhow::Result<()> {
    let schema_handler = Schema::new(&schema_paths.clone().into_iter().collect::<Vec<_>>()).await?;
    let schema_handler = schema_handler.run();
    let write_handle = output_path
        .map(|path| {
            let supergraph_config = SupergraphConfig::new(&addr, &path, schema_handler.clone())?;
            let handle = supergraph_config.run();
            anyhow::Ok(handle)
        })
        .transpose()?;

    // Build the router with routes for each subgraph
    let router = build_router(schema_paths.keys(), schema_handler.clone());

    // Print server information
    print_server_info(&addr, &schema_paths);

    // Start the server
    axum::serve(TcpListener::bind(addr).await?, router)
        .with_graceful_shutdown(async move {
            tokio::signal::ctrl_c()
                .await
                .expect("failed to listen to ctrl-c");
        })
        .await?;

    if let Some(write_handle) = write_handle {
        write_handle.close().await?;
    }
    schema_handler.close().await?;

    Ok(())
}

/// Build the Axum router with routes for each subgraph
fn build_router<I, K>(subgraph_names: I, schema_handler: SchemaHandler) -> Router
where
    I: IntoIterator<Item = K>,
    K: AsRef<str>,
{
    subgraph_names
        .into_iter()
        .fold(Router::new(), move |router, subgraph_name| {
            let subgraph_name = subgraph_name.as_ref().to_string();
            let endpoint = format!("/{}", subgraph_name);
            let endpoint_clone = endpoint.clone();
            let schema_handler = schema_handler.clone();

            router.route(
                &endpoint,
                get(|| async move { subgraph_graphiql(&endpoint_clone).await }).post_service(post(
                    {
                        move |req| {
                            async move {
                                let mut handler = schema_handler.get(&subgraph_name).await.unwrap();
                                handler.call(req).await
                            }
                            .in_current_span()
                        }
                    },
                )),
            )
        })
        .layer(TraceLayer::new_for_http())
}

/// Print server information to the console
fn print_server_info(addr: &str, schema_paths: &HashMap<String, PathBuf>) {
    println!("GraphiQL IDE: http://{}", addr);
    println!("Available subgraphs:");
    for subgraph_name in schema_paths.keys() {
        println!("  - /{}", subgraph_name);
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

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

        let mut tempfile = tempfile::NamedTempFile::new().unwrap();
        tempfile.write_all(schema.as_bytes()).unwrap();

        let schema_name = String::from("TestSchema");
        let schema_handler = Schema::new(&[(schema_name.clone(), tempfile.path().to_path_buf())])
            .await
            .unwrap()
            .handle();

        let mut router = build_router(vec![&schema_name], schema_handler);

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
        let http_request = axum::http::Request::builder()
            .method("POST")
            .uri(format!("/{schema_name}"))
            .header("content-type", "application/json")
            .body(serde_json::to_string(&request).unwrap())
            .unwrap();

        // Execute the query
        let response = router.call(http_request).await.unwrap();
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

    #[tokio::test]
    async fn test_nonstandard_root_type() {
        let schema = r#"
        schema @link(
            url: "https://specs.apollo.dev/federation/v2.7"
            import: ["@key"]
        ) {
          query: QueryType
        }

        type QueryType {
          user: User
        }

        type User @key(fields: "id") {
          id: ID!
        }
        "#;

        let mut tempfile = tempfile::NamedTempFile::new().unwrap();
        tempfile.write_all(schema.as_bytes()).unwrap();

        let schema_name = String::from("TestSchema");
        let schema_handler = Schema::new(&[(schema_name.clone(), tempfile.path().to_path_buf())])
            .await
            .unwrap()
            .handle();

        let mut router = build_router(vec![&schema_name], schema_handler);

        // Create an entity query that requests User with its A and B fields
        let query = r#"
          query() {
            user {
              id
            }
          }
        "#;

        // Create the variables with a representation for a User entity
        let request = json!({
            "query": query,
            "variables": {}
        });

        // Create the GraphQL request
        // let request = GraphQLRequest::new(query.to_string(), Some("".to_string()), Some(variables));
        let http_request = axum::http::Request::builder()
            .method("POST")
            .uri(format!("/{schema_name}"))
            .header("content-type", "application/json")
            .body(serde_json::to_string(&request).unwrap())
            .unwrap();

        // Execute the query
        let response = router.call(http_request).await.unwrap();
        let body = axum::body::to_bytes(response.into_body(), 2048)
            .await
            .unwrap();
        let json: Value = serde_json::from_slice(&body).unwrap();

        // Verify the response contains the expected structure
        assert!(json.get("data").is_some());
        let user = json["data"]["user"].as_object().unwrap();
        assert!(user.get("id").is_some());
    }
}
