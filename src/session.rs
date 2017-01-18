use std::sync::atomic::{AtomicUsize, Ordering};
use std::path::PathBuf;
use std::fs::{File, create_dir_all};
use protocol::{ ProtocolDef, ProtocolId };

use std::fmt::Debug;
use std::sync::RwLock;
use std::marker::PhantomData;
use std::mem::transmute;

pub struct Index<T, K> {
    items: RwLock<Vec<Option<Box<T>>>>,
    _phantom: PhantomData<K>,
}

impl<T, K: Copy + Debug + From<usize>> Index<T, K> where usize: From<K> {
    pub fn new() -> Self {
        Index {
            items: RwLock::new(Vec::new()),
            _phantom: PhantomData,
        }
    }

    pub fn create(&self) -> K {
        let mut locked = self.items.write().unwrap();
        let id = locked.len();
        locked.push(None);
        K::from(id)
    }

    pub fn define(&self, k: K, v: T) {
        let mut locked = self.items.write().unwrap();
        let pos = &mut locked[usize::from(k)];
        assert!(pos.is_none());
        *pos = Some(Box::new(v));
    }

    pub fn get<'s>(&'s self, k: K) -> &'s T {
        let locked = self.items.read().unwrap();
        if let Some(x) = locked[usize::from(k)].as_ref() {
            unsafe {
                // Items are never removed from the vec, boxed objects have stable addresses
                transmute::<&T, &'s T>(x)
            }
        } else {
            panic!("{:?} was not defined", k);
        }
    }
}

pub type ValueID = usize;

/// The data common to an entire resolve pass
pub struct Session {
    id_counter: AtomicUsize,
    debug_dir: Option<PathBuf>,
    pub protocols: Index<ProtocolDef, ProtocolId>,
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
            protocols: Index::new(),
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
