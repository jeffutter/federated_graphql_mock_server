use std::{
    fmt::Debug,
    path::PathBuf,
    pin::Pin,
    task::{Context, Poll},
};

use futures::{Stream, StreamExt};
use notify::{event::ModifyKind, Event, EventKind, RecursiveMode, Watcher as _};
use tokio_stream::wrappers::ReceiverStream;
use tracing::{info, trace, warn};

type Update = String;

pub struct SchemaWatcher {
    _watcher: Box<dyn notify::Watcher + Send + Sync>,
    updates_stream: ReceiverStream<String>,
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
    type Item = Update;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.updates_stream.poll_next_unpin(cx)
    }
}

impl SchemaWatcher {
    pub fn new(paths: &[(String, PathBuf)]) -> anyhow::Result<Self> {
        info!("Starting Schema Watcher for {paths:?}");

        let (updates_tx, updates_rx) = tokio::sync::mpsc::channel::<String>(100);

        let paths1 = paths.to_vec();
        let mut watcher =
            notify::recommended_watcher(move |res: notify::Result<notify::Event>| match &res {
                Ok(Event {
                    kind: EventKind::Modify(ModifyKind::Data(_)),
                    paths,
                    ..
                }) => {
                    trace!("File Event: {res:?}");
                    if let Some((name, _path)) = paths1.iter().find(|(_name, path)| {
                        paths.iter().any(|event_path| event_path.ends_with(path))
                    }) {
                        trace!("Sending file update for {name}");
                        updates_tx.try_send(name.to_string()).unwrap();
                    }
                }
                Ok(event) => {
                    warn!("Unhandled File Event: {:?}", event);
                }
                Err(e) => {
                    warn!("Error in file watcher: {}", e);
                }
            })
            .unwrap();

        for (_, path) in paths {
            watcher.watch(path, RecursiveMode::NonRecursive)?;
        }

        Ok(Self {
            _watcher: Box::new(watcher),
            updates_stream: ReceiverStream::new(updates_rx),
        })
    }
}
