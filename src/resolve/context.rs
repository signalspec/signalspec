use session::Session;
use eval;
use resolve::scope::{ValueRef, Dynamic, Item, ValueItem, ConstantItem, TupleItem, DefItem};

/// Dynamic Cell
pub type ValueID = uint;

pub struct Context<'session> {
	pub session: &'session Session<'session>,
	pub ops: eval::Ops,
}

impl<'session> Context<'session> {
	pub fn new<'s>(session: &'s Session<'s>) -> Context<'s> {
		Context {
			session: session,
			ops: eval::Ops::new(),
		}
	}

	pub fn child<'p>(&self) -> Context<'session> {
		Context {
			session: self.session,
			ops: eval::Ops::new(),
		}
	}

	pub fn into_ops(self) -> eval::Ops {
		self.ops
	}

	pub fn make_register(&mut self) -> ValueID {
		self.session.make_id()
	}

	pub fn down_op(&mut self, v: eval::ValOp) -> ValueRef {
		let id = self.make_register();
		self.ops.entry.push((id, v));
		Dynamic(id)
	}

	pub fn add_up_op(&mut self, id:ValueID, op: eval::ValOp) {
		self.ops.exit.push((id, op));
	}

	pub fn up_op(&mut self, dest: ValueID, v: |ValueID| -> eval::ValOp) -> ValueRef {
		let cell = self.make_register();
		self.add_up_op(dest, v(cell));
		Dynamic(cell)
	}
	
	fn flatten_into(&mut self, item: Item<'session>, down: &mut Vec<ValueID>, up: &mut Vec<ValueID>) {
		// TODO: check shape
		match item {
			ConstantItem(ref v) => {
				down.push(self.down_op(eval::ConstOp(v.clone())).value_id());
				up.push(self.up_op(0, |c| eval::CheckOp(c, v.clone())).value_id());
			},
			ValueItem(_, ref d, ref u) => {
				down.push(d.value_id());
				up.push(u.value_id());
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
