use std::sync::atomic::{AtomicUsize, Ordering};
use std::path::PathBuf;
use std::fs;

use super::{ Index };

#[derive(Clone, Default, Debug)]
pub struct Config {
    pub debug_dir: Option<PathBuf>
}

pub struct Ctxt<'a> {
    pub id_counter: AtomicUsize,
    pub debug_dir: Option<PathBuf>,
    pub index: &'a Index,
}

impl Ctxt<'_> {
    pub fn new<'a>(config: Config, index: &'a Index) -> Ctxt<'a> {
        if let Some(ref p) = config.debug_dir {
            fs::create_dir_all(p)
                .unwrap_or_else(|e| error!("Failed to create debug directory `{}`: {}", p.display(), e));
        }

        Ctxt {
            id_counter: AtomicUsize::new(1),
            debug_dir: config.debug_dir,
            index: index,
        }
    }

    pub fn make_id(&self) -> usize {
        self.id_counter.fetch_add(1, Ordering::Relaxed)
    }

    pub fn debug_file<T: FnOnce() -> String>(&self, name: T) -> Option<fs::File> {
        self.debug_dir.as_ref().and_then(|path| {
            let mut p = path.to_owned();
            p.push(name());
            fs::File::create(&p)
                .map_err(|e| error!("Failed to open debug file `{}`: {}", p.display(), e))
                .ok()
        })
    }
}

