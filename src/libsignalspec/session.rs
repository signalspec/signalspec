use arena::{Arena,TypedArena};
use resolve::{Scope, EventClosure};

/// The data common to an entire resolve pass
pub struct Session<'session> {
	moduleDefArena: TypedArena<EventClosure<'session>>,
	//cells: DCellDescriptor,
}

impl<'session> Session<'session> {
	pub fn new() -> Session {
		Session {
			moduleDefArena: TypedArena::new(),
			//cells: DCellDescriptor::new(),
		}
	}
}
