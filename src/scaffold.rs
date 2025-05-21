use anyhow::{anyhow, Context, Result};
use clap::{crate_name, crate_version};
use graphql_client::{GraphQLQuery, Response};
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use tracing::info;

use crate::schema_loader::SchemaLoader;
use crate::supergraph_compose::SupergraphCompose;
use crate::supergraph_config::SupergraphConfig;

// Define the GraphQL query structure
#[derive(GraphQLQuery)]
#[graphql(
    schema_path = "src/graphql/schema.graphql",
    query_path = "src/graphql/variant_query.graphql",
    response_derives = "Debug"
)]
pub struct VariantQuery;

pub async fn scaffold_schema(path: &PathBuf, proposal_number: u32) -> Result<()> {
    let apollo_key = std::env::var("APOLLO_KEY").context("Missing ENV Var APOLLO_KEY")?;
    // Create directory if it doesn't exist
    fs::create_dir_all(path).context("Failed to create output directory")?;

    // Construct the variant reference with the proposal number
    let variant_ref = format!("pi-staging@p-{}", proposal_number);

    info!("Fetching schema for variant: {}", variant_ref);

    // Set up the request
    let variables = variant_query::Variables { variant_ref };

    let client = reqwest::Client::new();
    let request_body = VariantQuery::build_query(variables);

    // Make the request to Apollo's API
    let res = client
        .post("https://api.apollographql.com/api/graphql")
        .json(&request_body)
        .header("apollographql-client-name", crate_name!())
        .header("apollographql-client-version", crate_version!())
        .header("X-API-KEY", apollo_key)
        .send()
        .await
        .context("Failed to send request to Apollo API")?;

    let response_body: Response<variant_query::ResponseData> = res
        .json()
        .await
        .context("Failed to parse response from Apollo API")?;

    // Check for errors in the response
    if let Some(errors) = response_body.errors {
        return Err(anyhow!("GraphQL errors: {:?}", errors));
    }

    // Extract the data
    let data = response_body
        .data
        .ok_or_else(|| anyhow!("No data in response"))?;

    // Handle the variant response
    match data.variant {
        Some(variant_query::VariantQueryVariant::GraphVariant(variant)) => {
            let mut count = 0;

            let mut schemas = vec![];

            for subgraph in variant.subgraphs.unwrap_or(vec![]) {
                let subgraph_name = subgraph.name;
                let sdl = subgraph.active_partial_schema.sdl;

                // Create file path
                let file_path = path.join(format!("{}.graphql", subgraph_name));

                info!(
                    "Writing schema for subgraph '{}' to {:?}",
                    subgraph_name, file_path
                );

                schemas.push((subgraph_name.clone(), file_path.clone()));

                // Write the SDL to a file
                let mut file = File::create(&file_path)
                    .with_context(|| format!("Failed to create file at {:?}", file_path))?;

                file.write_all(sdl.as_bytes())
                    .with_context(|| format!("Failed to write to file at {:?}", file_path))?;

                count += 1;
            }

            let port = 8080;
            let addr = format!("0.0.0.0:{}", port);
            let output_path = path.join("supergraph.yaml");

            let schema_loader = SchemaLoader::new(&schemas)
                .await
                .context("SchemaLoader Failed")?;
            let schema_loader_handle = schema_loader.handle();
            let config_writer =
                SupergraphConfig::new(&addr, &output_path, schema_loader_handle.clone())
                    .context("SupergraphConfig Failed")?;

            let mut graphql_path = output_path.clone();
            graphql_path.set_extension("graphql");

            let compose = SupergraphCompose::new(&output_path, &graphql_path)
                .context("SupergraphCompose Failed")?;

            config_writer.update_supergraph_config().await?;
            compose.compose_supergraph_schema().await?;

            info!("Successfully scaffolded {} subgraph schemas", count);
            Ok(())
        }
        Some(variant_query::VariantQueryVariant::InvalidRefFormat(error)) => {
            Err(anyhow!("Invalid variant reference: {}", error.message))
        }
        None => Err(anyhow!("No variant data returned")),
    }
}
