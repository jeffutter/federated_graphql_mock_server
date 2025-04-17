mod schema;
mod server;

use clap::Parser;
use std::path::PathBuf;
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

    #[arg(short, long)]
    output: Option<PathBuf>,
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

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    setup_tracing(&args);

    let addr = format!("0.0.0.0:{}", args.port);

    server::start_server(addr, args.mock_schema, args.output).await?;

    Ok(())
}
