use std::sync::atomic::{AtomicUsize, Ordering};
use std::path::PathBuf;
use std::fs::{File, create_dir_all};

pub type ValueID = usize;

/// The data common to an entire resolve pass
pub struct Session {
    id_counter: AtomicUsize,
    debug_dir: Option<PathBuf>,
}

impl Session {
    pub fn new(debug_dir: Option<PathBuf>) -> Session {
        if let Some(p) = debug_dir.as_ref() {
            create_dir_all(p)
                .unwrap_or_else(|e| error!("Failed to create debug directory `{}`: {}", p.display(), e));
        }
        Session {
            id_counter: AtomicUsize::new(1),
            debug_dir: debug_dir,
        }
    }

    pub fn make_id(&self) -> usize {
        self.id_counter.fetch_add(1, Ordering::Relaxed)
    }

    pub fn debug_file<T: FnOnce() -> String>(&self, name: T) -> Option<File> {
        self.debug_dir.as_ref().and_then(|path| {
            let mut p = path.to_owned();
            p.push(name());
            File::create(&p)
                .map_err(|e| error!("Failed to open debug file `{}`: {}", p.display(), e))
                .ok()
        })
    }
}
