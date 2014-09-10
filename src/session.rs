use std::fmt;
use std::sync::atomics::{AtomicUint, Relaxed};
use arena::TypedArena;

use resolve::block::EventClosure;

/// The data common to an entire resolve pass
pub struct Session<'session> {
	pub closure_arena: TypedArena<EventClosure<'session>>,
	id_counter: AtomicUint,
}

impl<'session> Session<'session> {
	pub fn new() -> Session<'session> {
		Session {
			closure_arena: TypedArena::new(),
			id_counter: AtomicUint::new(1),
		}
	}

	pub fn make_id<T>(&self) -> Id<T> {
      Id(self.id_counter.fetch_add(1, Relaxed))
  }
}

#[deriving(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Id<M>(uint);
impl<M> fmt::Show for Id<M> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let Id(i) = *self;
		write!(f, "{}", i)
	}
}
