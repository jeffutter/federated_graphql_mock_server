mod fetch;
mod mock_graph;
mod new_project;
mod schema;
mod schema_loader;
mod schema_parser;
mod schema_watcher;
mod server;
mod supergraph_compose;
mod supergraph_config;

use clap::{Parser, Subcommand};
use std::path::PathBuf;
use std::process::ExitCode;
use std::{collections::HashMap, process::Termination};
use tracing::level_filters::LevelFilter;
use tracing::{error, info};
use tracing_subscriber::{
    EnvFilter, Layer, fmt::format::FmtSpan, layer::SubscriberExt, util::SubscriberInitExt,
};

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Commandline Args
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, default_value_t = false)]
    pretty_traces: bool,

    /// Print LLM-friendly usage context (commands, options, directives, examples)
    #[arg(long)]
    llm_context: bool,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Start the GraphQL server
    Serve {
        #[arg(long, env = "PORT", default_value_t = 8080)]
        port: usize,

        /// Schema files in the format subgraph=file_name.graphql or
        /// subgraph=file_name.graphql@http://url to use a real routing URL
        #[arg(short, long, value_parser = parse_key_val)]
        schemas: Vec<(String, PathBuf, Option<String>)>,

        #[arg(short, long)]
        output: PathBuf,
    },
    /// Fetch a new GraphQL schema
    Fetch {
        /// Proposal number to use for the schema
        #[arg(short, long)]
        proposal_number: u32,

        /// Path to the folder where the schema will be created
        #[arg()]
        path: PathBuf,
    },
    /// Create a new federated GraphQL project
    New {
        /// Path to the folder where the project will be created
        #[arg()]
        path: PathBuf,

        /// Names of subgraphs to create
        #[arg(short, long)]
        subgraph: Vec<String>,
    },
}

/// Parse a key-value pair in the format "key=path" or "key=path@url"
fn parse_key_val(s: &str) -> Result<(String, PathBuf, Option<String>), String> {
    let (key, rest) = s
        .split_once('=')
        .ok_or_else(|| format!("Invalid KEY=value: no `=` found in `{s}`"))?;

    // Check for @url suffix (only split on @http:// or @https://)
    let (path_str, url) =
        if let Some(idx) = rest.find("@http://").or_else(|| rest.find("@https://")) {
            (&rest[..idx], Some(rest[idx + 1..].to_string()))
        } else {
            (rest, None)
        };

    Ok((key.to_string(), PathBuf::from(path_str), url))
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
            .with(
                tracing_subscriber::fmt::layer()
                    .with_span_events(FmtSpan::ENTER)
                    .with_span_events(FmtSpan::EXIT)
                    .with_filter(default_filter),
            )
            .init();
    };
}

const LLM_CONTEXT: &str = r#"# federated_graphql_mock_server

A CLI tool for local development of Apollo Federation GraphQL subgraphs. It generates mock data
automatically, watches schema files for changes, and composes supergraphs using Apollo's Rover CLI.

## Commands

### serve — Run a local dev server with mock resolvers

```
federated_graphql_mock_server serve --schemas <subgraph>=<path.graphql> --output <dir> [--port <port>]
```

**Options:**
- `--schemas` / `-s` — Schema files as `subgraph_name=path/to/schema.graphql` (repeatable).
  Append `@URL` to route to a real subgraph instead of mocking:
  `subgraph_name=path/to/schema.graphql@http://localhost:4001/graphql`
- `--output` / `-o` — Directory for composed supergraph output
- `--port` — HTTP port (default: 8080, env: `PORT`)

The server provides a GraphiQL IDE per subgraph and watches schema files for changes, automatically
recomposing and reloading when schemas are modified.

**Examples:**
```shell
# All mocked subgraphs
federated_graphql_mock_server serve \
  -s users=schemas/users.graphql \
  -s products=schemas/products.graphql \
  -o output/ \
  --port 4000

# Mix mocked and real subgraphs — users is real, products is mocked
federated_graphql_mock_server serve \
  -s users=schemas/users.graphql@http://localhost:4001/graphql \
  -s products=schemas/products.graphql \
  -o output/
```

### fetch — Pull schemas from Apollo Studio proposals

```
federated_graphql_mock_server fetch --proposal-number <num> <path>
```

**Options:**
- `--proposal-number` / `-p` — Apollo Studio proposal number
- `<path>` — Directory where fetched schemas will be written

Requires the `APOLLO_KEY` environment variable to be set.

**Example:**
```shell
APOLLO_KEY=service:my-graph:abc123 federated_graphql_mock_server fetch -p 42 ./schemas
```

### new — Scaffold a new federated project

```
federated_graphql_mock_server new <path> --subgraph <name> [--subgraph <name>...]
```

**Options:**
- `<path>` — Directory for the new project
- `--subgraph` / `-s` — Subgraph names to create (repeatable)

Creates schema files for each subgraph with Federation v2 boilerplate and composes an initial
supergraph schema.

**Example:**
```shell
federated_graphql_mock_server new my-project -s users -s products -s reviews
```

## Custom Mock Directives

Add these directives to fields in your subgraph schemas to control mock data generation.

### @words(min: Int, max: Int)

Generate random text with a specified word count. Applies to `String` fields.

- `min` — Minimum number of words (default: 1)
- `max` — Maximum number of words (default: 3)

```graphql
type User @key(fields: "id") {
  id: ID!
  bio: String @words(min: 10, max: 20)
  username: String @words(min: 1, max: 2)
}
```

### @select(from: [String])

Pick a random value from a fixed list. Applies to `String`, `ID`, interface, and union fields.

```graphql
type Product @key(fields: "id") {
  id: ID! @select(from: ["prod-1", "prod-2", "prod-3"])
  category: String @select(from: ["Electronics", "Clothing", "Books"])
  status: String @select(from: ["active", "archived", "draft"])
}
```

For interface/union fields, `@select` chooses which concrete type to resolve:

```graphql
type Query {
  feed: [FeedItem!]! @count(min: 5, max: 10)
}

union FeedItem = Post | Comment | Share

type Post { title: String }
type Comment { body: String }
type Share { url: String }

# Restrict mock feed to only Post and Comment:
# feed: [FeedItem!]! @select(from: ["Post", "Comment"]) @count(min: 5, max: 10)
```

### @count(min: Int, max: Int)

Control the number of items in a list field.

- `min` — Minimum list length (default: 1)
- `max` — Maximum list length (default: 1)

```graphql
type User @key(fields: "id") {
  id: ID!
  tags: [String!]! @count(min: 2, max: 5)
  orders: [Order!]! @count(min: 1, max: 10)
}
```

## Schema Requirements

Schemas must use Apollo Federation v2 `extend schema @link` syntax:

```graphql
extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.0",
        import: ["@key", "@shareable"])

type Query {
  users: [User!]! @count(min: 3, max: 5)
}

type User @key(fields: "id") {
  id: ID!
  name: String @select(from: ["Alice", "Bob", "Charlie"])
  bio: String @words(min: 5, max: 15)
}
```

## Typical Workflow

```shell
# 1. Scaffold a project
federated_graphql_mock_server new my-api -s users -s products

# 2. Edit the generated schemas, add mock directives
#    e.g., my-api/users.graphql, my-api/products.graphql

# 3. Start the dev server
federated_graphql_mock_server serve \
  -s users=my-api/users.graphql \
  -s products=my-api/products.graphql \
  -o my-api/

# 4. Open GraphiQL at http://localhost:8080 and query your mocked subgraphs
# 5. Edit schemas — the server watches for changes and recomposes automatically
```

## Environment Variables

- `PORT` — Server port for `serve` command (default: 8080)
- `APOLLO_KEY` — Apollo Studio API key, required for `fetch` command
- `RUST_LOG` — Control log verbosity (e.g., `RUST_LOG=debug`)
"#;

#[tokio::main]
async fn main() -> ExitCode {
    rustls::crypto::ring::default_provider()
        .install_default()
        .expect("Failed to install rustls crypto provider");

    let args = Args::parse();

    if args.llm_context {
        print!("{}", LLM_CONTEXT);
        return ExitCode::SUCCESS;
    }

    let Some(ref command) = args.command else {
        Args::parse_from(["", "--help"]);
        return ExitCode::FAILURE;
    };

    setup_tracing(&args);

    match command {
        Commands::Serve {
            port,
            schemas,
            output,
        } => {
            let addr = format!("0.0.0.0:{}", port);
            let schema_map: HashMap<String, PathBuf> = schemas
                .iter()
                .map(|(name, path, _)| (name.clone(), path.clone()))
                .collect();
            let url_overrides: HashMap<String, String> = schemas
                .iter()
                .filter_map(|(name, _, url)| url.as_ref().map(|u| (name.clone(), u.clone())))
                .collect();

            let res = server::start_server(addr, schema_map, url_overrides, output.clone()).await;

            if let Err(e) = res {
                error!("Error starting server: {:?}", e);
                ExitCode::FAILURE.report()
            } else {
                info!("Exiting");
                ExitCode::SUCCESS.report()
            }
        }
        Commands::Fetch {
            path,
            proposal_number,
        } => {
            info!(
                "Fetching schema at {:?} with proposal number {}",
                path, proposal_number
            );
            match fetch::fetch_schema(path, *proposal_number).await {
                Ok(_) => {
                    info!("Schema fetching completed successfully");
                    ExitCode::SUCCESS.report()
                }
                Err(e) => {
                    error!("Error fetching schema: {:?}", e);
                    ExitCode::FAILURE.report()
                }
            }
        }
        Commands::New { path, subgraph } => {
            info!(
                "Creating new project at {:?} with subgraphs: {:?}",
                path, subgraph
            );
            match new_project::create_new_project(path, subgraph).await {
                Ok(_) => {
                    info!("Project created successfully");
                    ExitCode::SUCCESS.report()
                }
                Err(e) => {
                    error!("Error creating project: {:?}", e);
                    ExitCode::FAILURE.report()
                }
            }
        }
    }
}
