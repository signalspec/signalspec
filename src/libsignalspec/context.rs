use session::Session;
use eval::{ValOp};
use ast::Value;

#[deriving(Eq, Clone)]
pub enum ValueRef {
	Ignored,
	Constant(Value),
	Dynamic(DCell),
}

/// Dynamic Cell
pub type DCell = uint;

pub struct Context<'session> {
	session: &'session Session<'session>,
	depth: uint,
	downs: ~[ValOp],
	ups:   ~[ValOp],
}

impl<'session> Context<'session> {
	pub fn new(session: &'session Session<'session>) -> Context<'session> {
		Context {
			session: session,
			depth: 0,
			downs: ~[],
			ups: ~[],
		}
	}

	pub fn child<'ctx>(&mut self) -> Context<'session> {
		Context {
			session: self.session,
			depth: self.depth + 1,
			downs: ~[],
			ups: ~[],
		}
	}
}

impl<'s> Context<'s> {
	pub fn op_ref_down(&mut self, op: ValOp) -> ValueRef {
		fail!("not implemented");
	}

	pub fn op_ref_up(&mut self, op: ValOp) -> ValueRef {
		fail!("not implemented");
	}

}
