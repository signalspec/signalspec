use std::sync::atomic::{AtomicUsize, Ordering};

pub type ValueID = usize;

/// The data common to an entire resolve pass
pub struct Session {
    id_counter: AtomicUsize,
}

impl Session {
    pub fn new() -> Session {
        Session {
            id_counter: AtomicUsize::new(1),
        }
    }

    pub fn make_id(&self) -> usize {
        self.id_counter.fetch_add(1, Ordering::Relaxed)
    }
}
