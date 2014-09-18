use session::Session;
use eval;
use resolve::scope::{ValueRef, Dynamic, Item, ValueItem, ConstantItem, TupleItem, DefItem};

/// Dynamic Cell
pub type ValueID = uint;

pub struct Context<'session> {
	pub session: &'session Session<'session>,
	pub downs: Vec<(ValueID, eval::ValOp)>,
	pub ups:   Vec<(ValueID, eval::ValOp)>,
}

impl<'session> Context<'session> {
	pub fn new<'s>(session: &'s Session<'s>) -> Context<'s> {
		Context {
			session: session,
			downs: Vec::new(),
			ups: Vec::new(),
		}
	}

	pub fn child<'p>(&self) -> Context<'session> {
		Context {
			session: self.session,
			downs: Vec::new(),
			ups: Vec::new(),
		}
	}
	
	pub fn make_register(&mut self) -> ValueID {
		self.session.make_id()
	}

	pub fn down_op(&mut self, v: eval::ValOp) -> ValueRef {
		let id = self.make_register();
		self.downs.push((id, v));
		Dynamic(id)
	}

	pub fn add_up_op(&mut self, id:ValueID, op: eval::ValOp) {
		self.ups.push((id, op));
	}

	pub fn up_op(&mut self, dest: ValueID, v: |ValueID| -> eval::ValOp) -> ValueRef {
		let cell = self.make_register();
		self.add_up_op(dest, v(cell));
		Dynamic(cell)
	}
	
	fn flatten_into(&mut self, item: Item<'session>, down: &mut Vec<ValueID>, up: &mut Vec<ValueID>) {
		// TODO: check shape
		match item {
			ConstantItem(_v) => unimplemented!(),
			ValueItem(_, ref _d, ref _u) => {
				down.push(0);
				up.push(0);
			},
			TupleItem(t) => for i in t.into_iter() { self.flatten_into(i, down, up) },
			DefItem(..) => fail!("Cannot flatten non-sendable expression")
		}
	}

	pub fn flatten_to_message(&mut self, item: Item<'session>) -> (Vec<ValueID>, Vec<ValueID>) {
		let mut down = Vec::new();
		let mut up = Vec::new();
		self.flatten_into(item, &mut down, &mut up);
		(down, up)
	}
}
