use session::Session;
use eval;
use ast::Value;
use std::fmt;

#[deriving(PartialEq, Clone)]
pub enum ValueRef {
	Ignored,
	Constant(Value),
	Dynamic(DCell),
	Poison(&'static str),
}

impl fmt::Show for ValueRef {
	fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::FormatError> {
		match *self {
			Ignored => write!(f, "ignore"),
			Constant(ref v) => write!(f, "{}", v),
			Dynamic(c) => write!(f, "%{}", c),
			Poison(s) => write!(f, "poison: {}", s),
		}
	}
}

/// Dynamic Cell
pub type DCell = uint;

pub struct Context<'session> {
	pub session: &'session Session<'session>,
	pub depth: uint,
	pub downs: Vec<eval::ValOp>,
	pub ups:   Vec<eval::ValOp>,
}

impl<'session> Context<'session> {
	pub fn new<'s>(session: &'s Session<'s>) -> Context<'s> {
		Context {
			session: session,
			depth: 0,
			downs: Vec::new(),
			ups: Vec::new(),
		}
	}

	pub fn child<'p>(&self) -> Context<'session> {
		Context {
			session: self.session,
			depth: self.depth + 1,
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
