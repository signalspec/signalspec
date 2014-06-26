use arena::TypedArena;
use expr::Item;

/// The data common to an entire resolve pass
pub struct Session<'session> {
	pub itemArena: TypedArena<Item<'session>>,
}

impl<'session> Session<'session> {
	pub fn new() -> Session {
		Session {
			itemArena: TypedArena::new(),
		}
	}
}
