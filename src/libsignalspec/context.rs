use std::any::Any;

use session::Session;
use eval;
use ast::Value;
use resolve;

#[deriving(Eq, Clone)]
pub enum ValueRef {
	Ignored,
	Constant(Value),
	Dynamic(DCell),
	Poison(&'static str),
}

/// Dynamic Cell
pub type DCell = uint;

pub struct Context<'session> {
	session: &'session Session<'session>,
	depth: uint,
	downs: ~[eval::ValOp],
	ups:   ~[eval::ValOp],
}

impl<'session> Context<'session> {
	pub fn new<'s>(session: &'s Session<'s>) -> Context<'s> {
		Context {
			session: session,
			depth: 0,
			downs: ~[],
			ups: ~[],
		}
	}

	pub fn child<'p>(&self) -> Context<'session> {
		Context {
			session: self.session,
			depth: self.depth + 1,
			downs: ~[],
			ups: ~[],
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

	pub fn up_cell(&mut self) -> DCell {
		return 0;
	}

	pub fn up_op(&mut self, cell:DCell, o: eval::ValOp) {

	}

	pub fn up_op_cell(&mut self, res: DCell, v: |DCell| -> eval::ValOp) -> ValueRef {
		let cell = 0;
		v(cell);
		Dynamic(cell)
	}
}
