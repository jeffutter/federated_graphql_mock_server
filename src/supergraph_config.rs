use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use itertools::Itertools;
use sha2::{Digest, Sha256};
use tempfile::TempDir;
use tracing::instrument;

use crate::{schema_loader::SchemaLoaderHandle, supergraph_compose};

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

    /// Write the supergraph YAML config to `self.output_path`.
    ///
    /// This writes **only** the YAML configuration file (subgraph routing URLs
    /// and schema file paths). It does **not** invoke Rover to compose the
    /// supergraph — callers are expected to run `run_rover_compose` separately.
    #[instrument(skip(self))]
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
            let hash = hasher
                .finalize()
                .iter()
                .map(|b| format!("{:02x}", b))
                .collect::<String>();

            // Create a file name with the subgraph name and hash
            let schema_file_name = format!("{}-{}.graphql", subgraph_name, hash);
            let schema_file_path = self.temp_dir_path.join(&schema_file_name);

            // Write the processed schema to the temp file
            fs::write(&schema_file_path, subgraph_sdl)?;

            // Store the relative path for the YAML config
            subgraph_schema_files.insert(subgraph_name.to_string(), schema_file_path);
        }

        let federation_version = match self.schema_handler.max_federation_version().await {
            Some(v) => format!("={}", v),
            None => "=2.7.8".to_string(),
        };

        let mut config = supergraph_compose::SupergraphYamlConfig::new(&federation_version);
        for (name, path) in &subgraph_schema_files {
            let routing_url = format!("http://{}/{}", self.addr, name);
            config.add_subgraph(name, &routing_url, &path.to_string_lossy());
        }

        fs::write(&self.output_path, serde_yaml::to_string(&config)?)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema_loader::SchemaLoader;
    use std::io::Write;

    #[tokio::test]
    async fn test_update_supergraph_config_writes_yaml_with_subgraphs() {
        let schema1 = r#"
        type Query {
          user: User
        }
        type User @key(fields: "id") {
          id: ID!
          name: String
        }
        "#;

        let schema2 = r#"
        type Query {
          product: Product
        }
        type Product @key(fields: "id") {
          id: ID!
          title: String
        }
        "#;

        let mut tempfile1 = tempfile::NamedTempFile::new().unwrap();
        tempfile1.write_all(schema1.as_bytes()).unwrap();
        let mut tempfile2 = tempfile::NamedTempFile::new().unwrap();
        tempfile2.write_all(schema2.as_bytes()).unwrap();

        let schema_loader = SchemaLoader::new(&[
            ("users".to_string(), tempfile1.path().to_path_buf()),
            ("products".to_string(), tempfile2.path().to_path_buf()),
        ])
        .await
        .unwrap();

        let handle = schema_loader.handle();
        let output_dir = tempfile::tempdir().unwrap();
        let output_path = output_dir.path().join("supergraph.yaml");

        let config = SupergraphConfig::new("0.0.0.0:9090", &output_path, handle).unwrap();
        config.update_supergraph_config().await.unwrap();

        // The output file must be valid YAML with populated subgraphs
        let yaml_content = fs::read_to_string(&output_path).unwrap();
        let yaml: serde_yaml::Value = serde_yaml::from_str(&yaml_content).unwrap();

        let subgraphs = yaml.get("subgraphs").unwrap().as_mapping().unwrap();
        assert_eq!(subgraphs.len(), 2);
        assert!(subgraphs.contains_key(serde_yaml::Value::String("users".to_string())));
        assert!(subgraphs.contains_key(serde_yaml::Value::String("products".to_string())));

        // Each subgraph must have routing_url and schema.file
        for name in &["users", "products"] {
            let sg = subgraphs
                .get(serde_yaml::Value::String(name.to_string()))
                .unwrap();
            let routing_url = sg.get("routing_url").unwrap().as_str().unwrap();
            assert_eq!(routing_url, format!("http://0.0.0.0:9090/{}", name));

            let file = sg
                .get("schema")
                .unwrap()
                .get("file")
                .unwrap()
                .as_str()
                .unwrap();
            assert!(file.ends_with(".graphql"));
        }

        // federation_version must start with '='
        let fed_version = yaml.get("federation_version").unwrap().as_str().unwrap();
        assert!(
            fed_version.starts_with('='),
            "federation_version should start with '=', got: {}",
            fed_version
        );
    }

    #[tokio::test]
    async fn test_update_supergraph_config_detects_federation_version() {
        let schema = r#"
        schema @link(url: "https://specs.apollo.dev/federation/v2.7", import: ["@key"]) {
          query: Query
        }

        type Query {
          user: User
        }
        type User @key(fields: "id") {
          id: ID!
          name: String
        }
        "#;

        let mut tempfile = tempfile::NamedTempFile::new().unwrap();
        tempfile.write_all(schema.as_bytes()).unwrap();

        let schema_loader =
            SchemaLoader::new(&[("users".to_string(), tempfile.path().to_path_buf())])
                .await
                .unwrap();

        let handle = schema_loader.handle();
        let output_dir = tempfile::tempdir().unwrap();
        let output_path = output_dir.path().join("supergraph.yaml");

        let config = SupergraphConfig::new("0.0.0.0:9090", &output_path, handle).unwrap();
        config.update_supergraph_config().await.unwrap();

        let yaml_content = fs::read_to_string(&output_path).unwrap();
        let yaml: serde_yaml::Value = serde_yaml::from_str(&yaml_content).unwrap();

        let fed_version = yaml.get("federation_version").unwrap().as_str().unwrap();
        assert_eq!(fed_version, "=2.7.0");
    }

    #[tokio::test]
    async fn test_update_supergraph_config_defaults_federation_version() {
        // Schema without @link directive should fall back to =2.7.8
        let schema = r#"
        type Query {
          user: User
        }
        type User @key(fields: "id") {
          id: ID!
          name: String
        }
        "#;

        let mut tempfile = tempfile::NamedTempFile::new().unwrap();
        tempfile.write_all(schema.as_bytes()).unwrap();

        let schema_loader =
            SchemaLoader::new(&[("users".to_string(), tempfile.path().to_path_buf())])
                .await
                .unwrap();

        let handle = schema_loader.handle();
        let output_dir = tempfile::tempdir().unwrap();
        let output_path = output_dir.path().join("supergraph.yaml");

        let config = SupergraphConfig::new("0.0.0.0:9090", &output_path, handle).unwrap();
        config.update_supergraph_config().await.unwrap();

        let yaml_content = fs::read_to_string(&output_path).unwrap();
        let yaml: serde_yaml::Value = serde_yaml::from_str(&yaml_content).unwrap();

        let fed_version = yaml.get("federation_version").unwrap().as_str().unwrap();
        assert_eq!(fed_version, "=2.7.8");
    }

    #[tokio::test]
    async fn test_update_supergraph_config_single_subgraph() {
        let schema = r#"
        type Query {
          user: User
        }
        type User @key(fields: "id") {
          id: ID!
          name: String
        }
        "#;

        let mut tempfile = tempfile::NamedTempFile::new().unwrap();
        tempfile.write_all(schema.as_bytes()).unwrap();

        let schema_loader =
            SchemaLoader::new(&[("users".to_string(), tempfile.path().to_path_buf())])
                .await
                .unwrap();

        let handle = schema_loader.handle();
        let output_dir = tempfile::tempdir().unwrap();
        let output_path = output_dir.path().join("supergraph.yaml");

        let config = SupergraphConfig::new("0.0.0.0:9090", &output_path, handle).unwrap();
        config.update_supergraph_config().await.unwrap();

        let yaml_content = fs::read_to_string(&output_path).unwrap();
        let yaml: serde_yaml::Value = serde_yaml::from_str(&yaml_content).unwrap();

        let subgraphs = yaml.get("subgraphs").unwrap().as_mapping().unwrap();
        assert_eq!(subgraphs.len(), 1);
        assert!(subgraphs.contains_key(serde_yaml::Value::String("users".to_string())));
    }

    #[tokio::test]
    async fn test_update_supergraph_config_schema_files_exist_on_disk() {
        let schema = r#"
        type Query {
          user: User
        }
        type User @key(fields: "id") {
          id: ID!
          name: String
        }
        "#;

        let mut tempfile = tempfile::NamedTempFile::new().unwrap();
        tempfile.write_all(schema.as_bytes()).unwrap();

        let schema_loader =
            SchemaLoader::new(&[("users".to_string(), tempfile.path().to_path_buf())])
                .await
                .unwrap();

        let handle = schema_loader.handle();
        let output_dir = tempfile::tempdir().unwrap();
        let output_path = output_dir.path().join("supergraph.yaml");

        let config = SupergraphConfig::new("0.0.0.0:9090", &output_path, handle).unwrap();
        config.update_supergraph_config().await.unwrap();

        let yaml_content = fs::read_to_string(&output_path).unwrap();
        let yaml: serde_yaml::Value = serde_yaml::from_str(&yaml_content).unwrap();

        let subgraphs = yaml.get("subgraphs").unwrap().as_mapping().unwrap();
        for (_name, sg) in subgraphs {
            let file_path = sg
                .get("schema")
                .unwrap()
                .get("file")
                .unwrap()
                .as_str()
                .unwrap();
            assert!(
                Path::new(file_path).exists(),
                "Schema file referenced in YAML does not exist: {}",
                file_path
            );
            // The file should contain valid GraphQL SDL
            let contents = fs::read_to_string(file_path).unwrap();
            assert!(
                contents.contains("Query"),
                "Schema file should contain SDL, got: {}",
                contents
            );
        }
    }

    #[tokio::test]
    async fn test_update_supergraph_config_picks_max_federation_version() {
        // Two schemas with different federation versions — should pick the higher one
        let schema_v25 = r#"
        schema @link(url: "https://specs.apollo.dev/federation/v2.5", import: ["@key"]) {
          query: Query
        }
        type Query {
          user: User
        }
        type User @key(fields: "id") {
          id: ID!
        }
        "#;

        let schema_v27 = r#"
        schema @link(url: "https://specs.apollo.dev/federation/v2.7", import: ["@key"]) {
          query: Query
        }
        type Query {
          product: Product
        }
        type Product @key(fields: "id") {
          id: ID!
        }
        "#;

        let mut tf1 = tempfile::NamedTempFile::new().unwrap();
        tf1.write_all(schema_v25.as_bytes()).unwrap();
        let mut tf2 = tempfile::NamedTempFile::new().unwrap();
        tf2.write_all(schema_v27.as_bytes()).unwrap();

        let schema_loader = SchemaLoader::new(&[
            ("users".to_string(), tf1.path().to_path_buf()),
            ("products".to_string(), tf2.path().to_path_buf()),
        ])
        .await
        .unwrap();

        let handle = schema_loader.handle();
        let output_dir = tempfile::tempdir().unwrap();
        let output_path = output_dir.path().join("supergraph.yaml");

        let config = SupergraphConfig::new("0.0.0.0:9090", &output_path, handle).unwrap();
        config.update_supergraph_config().await.unwrap();

        let yaml_content = fs::read_to_string(&output_path).unwrap();
        let yaml: serde_yaml::Value = serde_yaml::from_str(&yaml_content).unwrap();

        let fed_version = yaml.get("federation_version").unwrap().as_str().unwrap();
        assert_eq!(fed_version, "=2.7.0");
    }
}
