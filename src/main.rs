mod mock_graph;
mod schema;
mod schema_loader;
mod schema_watcher;
mod server;
mod supergraph_compose;
mod supergraph_config;

use clap::Parser;
use std::path::PathBuf;
use std::process::ExitCode;
use std::{collections::HashMap, process::Termination};
use tracing::level_filters::LevelFilter;
use tracing::{error, info};
use tracing_subscriber::{
    fmt::format::FmtSpan, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Layer,
};

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

    /// Schema files in the format subgraph=file_name.graphql
    #[arg(short, long, value_parser = parse_key_val)]
    schemas: Vec<(String, PathBuf)>,

    #[arg(short, long)]
    output: PathBuf,
}

/// Parse a key-value pair in the format "key=value"
fn parse_key_val(s: &str) -> Result<(String, PathBuf), String> {
    let (key, value) = s
        .split_once('=')
        .ok_or_else(|| format!("Invalid KEY=value: no `=` found in `{s}`"))?;
    let path = PathBuf::from(value);
    Ok((key.to_string(), path))
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

#[tokio::main]
async fn main() -> ExitCode {
    let args = Args::parse();
    setup_tracing(&args);

    let addr = format!("0.0.0.0:{}", args.port);

    let schema_map: HashMap<String, PathBuf> = args.schemas.into_iter().collect();

    let res = server::start_server(addr, schema_map, args.output).await;

    if let Err(e) = res {
        error!("Error starting server: {:?}", e);
        ExitCode::FAILURE.report()
    } else {
        info!("Exiting");
        ExitCode::SUCCESS.report()
    }
}
