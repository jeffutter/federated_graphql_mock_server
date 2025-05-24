use anyhow::{Context, Result};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use tracing::info;

use crate::supergraph_compose::{self};

/// Create a new federated GraphQL project with the specified subgraphs
pub async fn create_new_project(path: &Path, subgraphs: &[String]) -> Result<()> {
    // Create the project directory if it doesn't exist
    fs::create_dir_all(path).context("Failed to create project directory")?;

    // Create a map to store subgraph paths
    let mut schema_map = HashMap::new();

    if subgraphs.is_empty() {
        info!("No subgraphs provided. Skipping schema generation.");
        return Ok(());
    }

    // Create each subgraph schema file
    for subgraph in subgraphs {
        let schema_path = path.join(format!("{}.graphql", subgraph));
        let schema_content = generate_basic_schema(subgraph);

        fs::write(&schema_path, schema_content)
            .with_context(|| format!("Failed to write schema file for subgraph '{}'", subgraph))?;

        info!(
            "Created schema for subgraph '{}' at {:?}",
            subgraph, schema_path
        );
        schema_map.insert(subgraph.clone(), schema_path);
    }

    // Create the supergraph schema
    let supergraph_path = path.join("supergraph.graphql");

    // Use the SupergraphCompose utility to generate the supergraph schema
    let _ = supergraph_compose::compose_from_schemas(&schema_map, &supergraph_path, None)
        .context("Failed to compose supergraph schema")?;

    info!("Created supergraph schema at {:?}", supergraph_path);

    Ok(())
}

/// Generate a basic schema for a subgraph
fn generate_basic_schema(subgraph_name: &str) -> String {
    format!(
        r#"extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.0",
        import: ["@key", "@shareable"])

type Query {{
  {subgraph_name}Info: String
}}

type {entity_name} @key(fields: "id") {{
  id: ID!
  name: String
  description: String
}}
"#,
        subgraph_name = subgraph_name.to_lowercase(),
        entity_name = capitalize_first(subgraph_name)
    )
}

/// Capitalize the first letter of a string
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}
