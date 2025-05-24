use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    process::Command,
    time::Duration,
};

use anyhow::Context;
use indicatif::ProgressBar;
use serde::Serialize;
use tracing::{error, info, instrument};

#[derive(Debug, Serialize)]
pub struct SupergraphConfig {
    pub federation_version: String,
    pub subgraphs: HashMap<String, SubgraphConfig>,
}

impl SupergraphConfig {
    pub fn new(federation_version: &str) -> Self {
        Self {
            federation_version: federation_version.to_string(),
            subgraphs: HashMap::new(),
        }
    }

    pub fn add_subgraph(&mut self, name: &str, routing_url: &str, file: &str) {
        let subgraph = SubgraphConfig::new(routing_url, file);
        self.subgraphs.insert(name.to_string(), subgraph);
    }
}

#[derive(Debug, Serialize)]
pub struct SubgraphConfig {
    pub routing_url: String,
    pub schema: SubgraphSchemaConfig,
}

impl SubgraphConfig {
    pub fn new(routing_url: &str, file: &str) -> Self {
        Self {
            routing_url: routing_url.to_string(),
            schema: SubgraphSchemaConfig::new(file),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct SubgraphSchemaConfig {
    pub file: String,
}

impl SubgraphSchemaConfig {
    pub fn new(file: &str) -> Self {
        Self {
            file: file.to_string(),
        }
    }
}

/// Compose a supergraph schema directly from a map of subgraph schemas
#[instrument()]
pub fn compose_from_schemas(
    schemas: &HashMap<String, PathBuf>,
    output_path: &Path,
    addr: Option<&str>,
) -> anyhow::Result<String> {
    // Create a temporary config file
    let temp_dir = tempfile::tempdir()?;
    let config_path = temp_dir.path().join("supergraph.yaml");

    let addr = match addr {
        Some(addr) => addr.to_string(),
        None => {
            let port = 8080;
            format!("0.0.0.0:{}", port)
        }
    };
    let mut supergraph_config = SupergraphConfig::new("=2.7.8");

    for (name, path) in schemas {
        let path_str = path.to_string_lossy();
        let routing_url = format!("http://{}/{}", addr, name);
        supergraph_config.add_subgraph(name, &routing_url, &path_str);
    }

    // Write config to file
    fs::write(&config_path, serde_yaml::to_string(&supergraph_config)?)?;

    // Run rover and return the schema
    run_rover_compose(&config_path, output_path)
}

/// Run rover supergraph compose with the given config path and write the output to the given path
#[instrument()]
pub fn run_rover_compose(config_path: &Path, output_path: &Path) -> anyhow::Result<String> {
    // Create the output directory if it doesn't exist
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)?;
    }

    // Run rover command
    let args = [
        "supergraph",
        "compose",
        "--config",
        &config_path.to_string_lossy(),
    ];

    info!("Running: rover {}", args.join(" "));

    // Create a spinner to show progress if requested
    let sp = ProgressBar::new_spinner();
    sp.set_message("Rover is running...");
    sp.enable_steady_tick(Duration::from_millis(100));

    let output = Command::new("rover")
        .args(args)
        .output()
        .context("Rover supergraph compose failed")?;

    // Finish the spinner if it exists
    sp.finish_and_clear();

    if !output.status.success() {
        error!(
            "Composing Supergraph Schema Failed: {:?}\n{}",
            output.status,
            String::from_utf8(output.stderr)?
        );
        anyhow::bail!("Rover supergraph compose failed");
    }

    let schema = String::from_utf8(output.stdout)?;

    // Write to output file
    fs::write(output_path, &schema)?;

    Ok(schema)
}
