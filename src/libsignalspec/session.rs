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

pub struct Context<'session> {
	session: &'session Session<'session>,
}

impl<'session> Context<'session> {
	pub fn new(session: &'session Session<'session>) -> Context<'session> {
		Context {
			session: session,
		}
	}

	pub fn child<'ctx>(&mut self) -> Context<'session> {
		Context {
			session: self.session,
		}
	}
}