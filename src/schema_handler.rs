use std::fmt::Debug;
use std::sync::Arc;
use std::{collections::HashMap, path::PathBuf};

use anyhow::Context;
use async_graphql::{dynamic::*, extensions::Tracing};
use async_graphql_axum::GraphQL;
use futures::{Stream, StreamExt, TryStreamExt};
use graphql_parser::schema::{Definition, Document, TypeDefinition};
use tokio::{select, sync::RwLock, task::JoinHandle};
use tokio_stream::wrappers::BroadcastStream;
use tokio_util::sync::CancellationToken;
use tracing::{trace, Instrument};

use crate::schema;
use crate::{mock_graph::MockGraph, schema_watcher::SchemaWatcher};

struct SchemaInner {
    sdl: String,
    path: PathBuf,
    schema: GraphQL<async_graphql::dynamic::Schema>,
}

impl Debug for SchemaInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SchemaInner")
            .field("sdl", &self.sdl)
            .field("path", &self.path)
            .field("schema", &"xxx")
            .finish()
    }
}

#[derive(Debug)]
pub struct Schema {
    watcher: RwLock<SchemaWatcher>,
    schemas: RwLock<HashMap<String, SchemaInner>>,
    task_handle: RwLock<Option<JoinHandle<()>>>,
    tx: tokio::sync::broadcast::Sender<String>,
    cancellation_token: CancellationToken,
}

#[derive(Debug)]
pub struct SchemaHandler {
    rx: tokio::sync::broadcast::Receiver<String>,
    inner: Arc<Schema>,
    cancellation_token: CancellationToken,
}

impl Clone for SchemaHandler {
    fn clone(&self) -> Self {
        Self {
            rx: self.rx.resubscribe(),
            inner: self.inner.clone(),
            cancellation_token: self.cancellation_token.clone(),
        }
    }
}

impl Schema {
    pub async fn new(paths: &[(String, PathBuf)]) -> anyhow::Result<Arc<Self>> {
        let (tx, _rx) = tokio::sync::broadcast::channel(100);

        let handler = Self {
            watcher: RwLock::new(SchemaWatcher::new(paths)?),
            schemas: RwLock::new(HashMap::new()),
            task_handle: RwLock::new(None),
            tx,
            cancellation_token: CancellationToken::new(),
        };

        let handler = Arc::new(handler);

        for (name, path) in paths {
            handler.load_schema(name.to_string(), path).await?;
        }

        Ok(handler)
    }

    pub async fn reload_schema(self: &Arc<Self>, name: &str) -> anyhow::Result<()> {
        let path = {
            let schemas = self.schemas.read().await;
            let SchemaInner { path, .. } = schemas.get(name).context("Schema not found")?;
            path.clone()
        };
        self.load_schema(name.to_string(), &path).await?;

        Ok(())
    }

    pub async fn load_schema(self: &Arc<Self>, name: String, path: &PathBuf) -> anyhow::Result<()> {
        let (sdl, schema) = load_schema_from_path(path)?;
        self.schemas.write().await.insert(
            name.clone(),
            SchemaInner {
                sdl,
                schema: GraphQL::new(schema),
                path: path.clone(),
            },
        );
        self.tx.send(name)?;
        Ok(())
    }

    pub fn handle(self: &Arc<Self>) -> SchemaHandler {
        SchemaHandler {
            rx: self.tx.subscribe(),
            inner: Arc::clone(self),
            cancellation_token: self.cancellation_token.clone(),
        }
    }

    pub fn run(self: &Arc<Self>) -> SchemaHandler {
        let state = Arc::clone(self);

        let task = tokio::spawn(async move {
            let mut watcher = state.watcher.write().await;

            loop {
                select! {
                    Some(schema) = watcher.next()=> {
                        state.reload_schema(&schema).await.unwrap();
                    }
                    _ = state.cancellation_token.cancelled() => break
                }
            }
        });

        let state_for_handle = Arc::clone(self);

        // store task handle
        tokio::spawn(async move {
            *state_for_handle.task_handle.write().await = Some(task);
        });

        // return handle for read-only access
        self.handle()
    }
}

impl SchemaHandler {
    pub fn stream(&self) -> impl Stream<Item = anyhow::Result<String>> {
        BroadcastStream::new(self.rx.resubscribe()).err_into()
    }

    pub async fn name_and_sdl(&self) -> Vec<(String, String)> {
        self.inner
            .schemas
            .read()
            .await
            .iter()
            .map(|(name, inner)| (name.to_string(), inner.sdl.clone()))
            .collect()
    }

    pub async fn get(&self, name: &str) -> Option<GraphQL<async_graphql::dynamic::Schema>> {
        self.inner
            .schemas
            .read()
            .await
            .get(name)
            .map(|schema_inner| schema_inner.schema.clone())
    }

    pub async fn close(&self) -> anyhow::Result<()> {
        self.cancellation_token.cancel();
        if let Some(x) = self.inner.task_handle.write().await.take() {
            x.await?;
        }
        anyhow::Ok(())
    }
}

fn load_schema_from_path(
    path: &PathBuf,
) -> anyhow::Result<(String, async_graphql::dynamic::Schema)> {
    let input_sdl_str = std::fs::read_to_string(path)?;
    load_schema(input_sdl_str)
}

fn load_schema(input_sdl_str: String) -> anyhow::Result<(String, async_graphql::dynamic::Schema)> {
    let input_sdl: Document<&str> = graphql_parser::parse_schema(&input_sdl_str)?;

    let root_query_name = input_sdl
        .definitions
        .iter()
        .find_map(|d| match d {
            Definition::SchemaDefinition(sd) => sd.query,
            _ => None,
        })
        .unwrap_or("Query");

    let mutation_query_name = input_sdl.definitions.iter().find_map(|d| match d {
        Definition::SchemaDefinition(sd) => sd.mutation,
        _ => None,
    });

    let subscription_query_name = input_sdl.definitions.iter().find_map(|d| match d {
        Definition::SchemaDefinition(sd) => sd.subscription,
        _ => None,
    });

    let mock_graph = MockGraph::builder(
        root_query_name.to_string(),
        mutation_query_name.map(|x| x.to_string()),
        subscription_query_name.map(|x| x.to_string()),
    )
    .register_document(&input_sdl)
    .build();

    let schema = input_sdl
        .definitions
        .clone()
        .iter()
        .fold(
            async_graphql::dynamic::Schema::build(
                "Query",
                mutation_query_name,
                subscription_query_name,
            )
            .data(mock_graph),
            |schema, definition| match definition {
                Definition::SchemaDefinition(_schema_definition) => schema,
                Definition::TypeDefinition(source_type_definition) => {
                    match source_type_definition {
                        TypeDefinition::Scalar(source_scalar) => {
                            schema::register_scalar(schema, source_scalar)
                        }
                        TypeDefinition::Object(source_object) => {
                            // We need to force rename the root query to "Query" as async-graphql
                            // won't output the query/mutation/subscription object mapping with `federation`
                            // https://github.com/async-graphql/async-graphql/blob/75a9d14e8f45176a32bac7f458534c05cabd10cc/src/registry/export_sdl.rs#L158-L201
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

    Ok((sdl, schema))
}
