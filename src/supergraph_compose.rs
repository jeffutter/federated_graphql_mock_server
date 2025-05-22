use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
    time::Duration,
};

use anyhow::Context;
use indicatif::ProgressBar;
use tracing::{error, info, instrument};

pub struct SupergraphCompose {
    output_path: PathBuf,
    supergraph_path: PathBuf,
}

impl SupergraphCompose {
    pub fn new(supergraph_path: &Path, output_path: &Path) -> anyhow::Result<Arc<Self>> {
        Ok(Arc::new(Self {
            output_path: output_path.to_path_buf(),
            supergraph_path: supergraph_path.to_path_buf(),
        }))
    }

    #[instrument(skip(self))]
    pub async fn compose_supergraph_schema(self: &Arc<Self>) -> anyhow::Result<()> {
        // Create the output directory if it doesn't exist
        if let Some(parent) = self.output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let args = [
            "supergraph",
            "compose",
            "--config",
            &self.supergraph_path.to_string_lossy(),
        ];

        info!("Running: rover {}", args.join(" "));

        // Create a spinner to show progress
        let spinner = ProgressBar::new_spinner();
        spinner.set_message("Rover is running...");
        spinner.enable_steady_tick(Duration::from_millis(100));

        let x = Command::new("rover")
            .args(args)
            .output()
            .context("Rover supergraph compose failed")?;

        // Finish the spinner
        spinner.finish_and_clear();

        if !x.status.success() {
            error!(
                "Writing Supergraph Schema Failed: {:?}\n{}",
                x.status,
                String::from_utf8(x.stderr)?
            );
        }

        fs::write(self.output_path.clone(), x.stdout)?;

        Ok(())
    }
}
