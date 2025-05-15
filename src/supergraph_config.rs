use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use itertools::Itertools;
use sha2::{Digest, Sha256};
use tempfile::TempDir;

use crate::schema_loader::SchemaLoaderHandle;

pub struct SupergraphConfig {
    addr: String,
    output_path: PathBuf,
    _temp_dir: TempDir,
    temp_dir_path: PathBuf,
    schema_handler: SchemaLoaderHandle,
}

impl SupergraphConfig {
    pub fn new(
        addr: &str,
        output_path: &Path,
        schema_handler: SchemaLoaderHandle,
    ) -> anyhow::Result<Arc<Self>> {
        let temp_dir = TempDir::new()?;
        let temp_dir_path = temp_dir.path().to_path_buf();

        Ok(Arc::new(Self {
            addr: addr.to_string(),
            output_path: output_path.to_path_buf(),
            _temp_dir: temp_dir,
            temp_dir_path,
            schema_handler,
        }))
    }

    pub async fn update_supergraph_config(self: &Arc<Self>) -> anyhow::Result<()> {
        // Create the output directory if it doesn't exist
        if let Some(parent) = self.output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        // Create a map to store schema file paths for each subgraph
        let mut subgraph_schema_files = HashMap::new();
        let name_and_sdl = self.schema_handler.name_and_sdl().await;

        // Process each handler to get the SDL
        for (subgraph_name, subgraph_sdl) in name_and_sdl.iter().sorted_by_key(|(name, _)| name) {
            // Generate a hash of the processed schema content
            let mut hasher = Sha256::new();
            hasher.update(subgraph_sdl);
            let hash = format!("{:x}", hasher.finalize());

            // Create a file name with the subgraph name and hash
            let schema_file_name = format!("{}-{}.graphql", subgraph_name, hash);
            let schema_file_path = self.temp_dir_path.join(&schema_file_name);

            // Write the processed schema to the temp file
            fs::write(&schema_file_path, subgraph_sdl)?;

            // Store the relative path for the YAML config
            subgraph_schema_files.insert(subgraph_name, schema_file_path);
        }

        // Generate the supergraph.yaml content
        let mut yaml_content = String::from("federation_version: =2.7.8\nsubgraphs:\n");

        for (subgraph_name, schema_file_path) in subgraph_schema_files
            .into_iter()
            .sorted_by_key(|(name, _)| name.to_owned())
        {
            let routing_url = format!(
                "http://{}:{}/{}",
                self.addr.split(':').next().unwrap_or("localhost"),
                self.addr.split(':').nth(1).unwrap_or("8080"),
                subgraph_name
            );

            yaml_content.push_str(&format!("  {}:\n", subgraph_name));
            yaml_content.push_str(&format!("    routing_url: {}\n", routing_url));
            yaml_content.push_str("    schema:\n");
            yaml_content.push_str(&format!(
                "      file: {}\n",
                schema_file_path.to_string_lossy()
            ));
        }

        // Write the supergraph.yaml file
        fs::write(self.output_path.clone(), yaml_content)?;

        Ok(())
    }
}
