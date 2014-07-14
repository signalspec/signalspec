use session::Session;
use eval;
use ast::Value;
use resolve::scope::{ValueRef, Constant, Dynamic};

/// Dynamic Cell
pub type ValueID = uint;

pub struct Context<'session> {
	pub session: &'session Session<'session>,
	pub downs: Vec<eval::ValOp>,
	pub ups:   Vec<eval::ValOp>,
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

	pub fn get_const(&self, v: &ValueRef) -> Value {
		match *v {
			Constant(ref x) => x.clone(),
			_ => fail!("Constant value required"),
		}
	}

	pub fn down_op(&mut self, _v: eval::ValOp) -> ValueRef {
		Dynamic(0)
	}

	pub fn up_cell(&mut self) -> ValueID {
		return 0;
	}

	pub fn up_op(&mut self, _cell:ValueID, _o: eval::ValOp) {

	}

	pub fn up_op_cell(&mut self, _res: ValueID, v: |ValueID| -> eval::ValOp) -> ValueRef {
		let cell = 0;
		v(cell);
		Dynamic(cell)
	}
}
