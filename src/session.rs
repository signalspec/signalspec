use std::sync::atomic::{AtomicUint, Relaxed};
use arena::TypedArena;

use resolve::Scope;
use resolve::block::EventClosure;

/// The data common to an entire resolve pass
pub struct Session<'session> {
    pub closure_arena: TypedArena<EventClosure<'session>>,
    id_counter: AtomicUint,
    pub prelude: Scope<'session>,
}

impl<'session> Session<'session> {
    pub fn new() -> Session<'session> {
        Session {
            closure_arena: TypedArena::new(),
            id_counter: AtomicUint::new(1),
            prelude: Scope::new(),
        }
    }

    pub fn make_id(&self) -> uint {
        self.id_counter.fetch_add(1, Relaxed)
    }
}
