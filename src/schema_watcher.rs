use std::{
    fmt::Debug,
    path::PathBuf,
    pin::Pin,
    sync::Arc,
    task::{Context, Poll},
};

use futures::{Stream, StreamExt};
use notify::{event::ModifyKind, Event, EventKind, RecursiveMode, Watcher as _};
use tokio::{
    select,
    sync::{broadcast, RwLock},
};
use tokio_stream::wrappers::BroadcastStream;
use tokio_util::sync::CancellationToken;
use tracing::{info, warn};

type Update = String;

struct Inner {
    _watcher: Box<dyn notify::Watcher + Send + Sync>,
    _updates_rx: broadcast::Receiver<String>,
    paths: Vec<PathBuf>,
    cancellation_token: CancellationToken,
}

impl Inner {
    pub fn close(&mut self) {
        for path in self.paths.iter() {
            if let Err(e) = self._watcher.unwatch(path) {
                warn!("Error unwatching path {}: {}", path.display(), e);
            };
        }
        self.cancellation_token.cancel();
    }
}

pub struct SchemaWatcher {
    _updates_rx: broadcast::Receiver<String>,
    updates_stream: BroadcastStream<String>,
    inner: Arc<RwLock<Inner>>,
}

impl Clone for SchemaWatcher {
    fn clone(&self) -> Self {
        let updates_rx = self._updates_rx.resubscribe();
        let _updates_rx = self._updates_rx.resubscribe();
        let stream = BroadcastStream::new(updates_rx);
        Self {
            updates_stream: stream,
            _updates_rx,
            inner: self.inner.clone(),
        }
    }
}

impl Debug for SchemaWatcher {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SchemaWatcher")
            .field("_watcher", &"xxx")
            .field("updates_stream", &self.updates_stream)
            .finish()
    }
}

impl Stream for SchemaWatcher {
    type Item = anyhow::Result<Update>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.updates_stream
            .poll_next_unpin(cx)
            .map_err(|x| x.into())
    }
}

impl SchemaWatcher {
    pub async fn done(&self) {
        self.inner.read().await.cancellation_token.cancelled().await
    }

    pub fn new(paths: &[(String, PathBuf)]) -> anyhow::Result<Self> {
        info!("Starting Schema Watcher for {paths:?}");

        let (updates_tx, updates_rx) = tokio::sync::broadcast::channel::<String>(100);
        let (inner_updates_tx, mut inner_updates_rx) = tokio::sync::mpsc::channel::<String>(100);
        let cancellation_token = CancellationToken::new();

        let paths1 = paths.to_vec();
        let mut watcher =
            notify::recommended_watcher(move |res: notify::Result<notify::Event>| match &res {
                Ok(Event {
                    kind: EventKind::Modify(ModifyKind::Data(_)),
                    paths,
                    ..
                }) => {
                    if let Some((name, _path)) = paths1.iter().find(|(_name, path)| {
                        paths.iter().any(|event_path| event_path.ends_with(path))
                    }) {
                        inner_updates_tx.try_send(name.to_string()).unwrap();
                    }
                }
                Ok(Event {
                    kind: EventKind::Create(_),
                    ..
                }) => {}
                Ok(Event {
                    kind: EventKind::Modify(ModifyKind::Metadata(_)),
                    ..
                }) => {}
                Ok(Event {
                    kind: EventKind::Modify(ModifyKind::Name(_)),
                    ..
                }) => {}
                Ok(event) => {
                    warn!("Unhandled File Event: {:?}", event);
                }
                Err(e) => {
                    warn!("Error in file watcher: {}", e);
                }
            })?;

        let cancellation_token1 = cancellation_token.clone();
        tokio::spawn(async move {
            loop {
                select! {
                    _ = cancellation_token1.cancelled() => {
                        break;
                    }
                    Some(update) = inner_updates_rx.recv() => {
                        updates_tx.send(update).unwrap();
                    }
                }
            }
        });

        let mut watched_paths = Vec::with_capacity(paths.len());
        for (_, path) in paths {
            watcher.watch(path, RecursiveMode::NonRecursive)?;
            watched_paths.push(path.to_path_buf());
        }

        let updates_rx1 = updates_rx.resubscribe();
        let updates_stream = BroadcastStream::new(updates_rx.resubscribe());

        Ok(Self {
            _updates_rx: updates_rx1,
            updates_stream,
            inner: Arc::new(RwLock::new(Inner {
                _watcher: Box::new(watcher),
                _updates_rx: updates_rx,
                paths: watched_paths,
                cancellation_token,
            })),
        })
    }

    pub async fn close(&mut self) {
        self.inner.write().await.close();
    }
}
