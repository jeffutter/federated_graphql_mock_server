use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use futures::{Stream, StreamExt};
use tokio::{select, sync::RwLock, task::JoinHandle};
use tokio_util::sync::CancellationToken;
use tracing::error;

pub struct SupergraphComposeHandle {
    inner: Arc<SupergraphCompose>,
    cancellation_token: CancellationToken,
}

impl SupergraphComposeHandle {
    pub async fn close(&self) -> anyhow::Result<()> {
        self.cancellation_token.cancel();
        if let Some(x) = self.inner.task_handle.write().await.take() {
            x.await??;
        }
        anyhow::Ok(())
    }
}

pub struct SupergraphCompose {
    output_path: PathBuf,
    supergraph_path: PathBuf,
    cancellation_token: CancellationToken,
    task_handle: RwLock<Option<JoinHandle<anyhow::Result<()>>>>,
}

impl SupergraphCompose {
    pub fn new(supergraph_path: &Path, output_path: &Path) -> anyhow::Result<Arc<Self>> {
        Ok(Arc::new(Self {
            output_path: output_path.to_path_buf(),
            supergraph_path: supergraph_path.to_path_buf(),
            cancellation_token: CancellationToken::new(),
            task_handle: RwLock::new(None),
        }))
    }

    pub fn run(
        self: &Arc<Self>,
        mut update: impl Stream<Item = anyhow::Result<()>> + Send + Unpin + 'static,
    ) -> Arc<SupergraphComposeHandle> {
        let state = Arc::clone(self);

        let task = tokio::spawn(async move {
            loop {
                select! {
                    Some(Ok(())) = update.next() => {
                        state.compose_supergraph_schema().await?;
                    }
                    _ = state.cancellation_token.cancelled() => break
                }
            }

            anyhow::Ok(())
        });

        let state_for_handle = Arc::clone(self);

        // store task handle
        tokio::spawn(async move {
            *state_for_handle.task_handle.write().await = Some(task);
        });

        Arc::new(SupergraphComposeHandle {
            inner: Arc::clone(self),
            cancellation_token: self.cancellation_token.clone(),
        })
    }

    async fn compose_supergraph_schema(self: &Arc<Self>) -> anyhow::Result<()> {
        // Create the output directory if it doesn't exist
        if let Some(parent) = self.output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let x = Command::new("rover")
            .args([
                "supergraph",
                "compose",
                "--config",
                &self.supergraph_path.to_string_lossy(),
            ])
            .output()?;

        if !x.status.success() {
            error!(
                "Writing Supergraph Schema Failed: {:?}\n{:?}",
                x.status, x.stderr
            );
        }

        fs::write(self.output_path.clone(), x.stdout)?;

        println!("Updated supergraph schema at: {:?}", self.output_path);

        Ok(())
    }
}
