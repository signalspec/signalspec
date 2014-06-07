use arena::{Arena,TypedArena};
use expr::Item;

/// The data common to an entire resolve pass
pub struct Session<'session> {
	pub arena: Arena,
	pub itemArena: TypedArena<Item<'session>>,
}

impl<'session> Session<'session> {
	pub fn new() -> Session {
		Session {
			arena: Arena::new(),
			itemArena: TypedArena::new(),
		}
	}
}
