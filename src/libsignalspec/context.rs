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

pub trait Domain: Any {
	// TODO: this method exists as a workaround for mozilla/rust#5665
	fn as_any<'a>(&'a self) -> &'a Any;
	fn resolve_time<'s>(&self, ctx: &mut Context<'s>, params: &resolve::Params<'s>) -> resolve::Step;
}

pub struct Context<'session> {
	session: &'session Session<'session>,
	depth: uint,
	downs: ~[eval::ValOp],
	ups:   ~[eval::ValOp],
	domain: &'session Domain,
}

impl<'session> Context<'session> {
	pub fn new(session: &'session Session<'session>) -> Context<'session> {
		Context {
			session: session,
			depth: 0,
			downs: ~[],
			ups: ~[],
			domain: &default_domain,
		}
	}

	pub fn child(&mut self) -> Context<'session> {
		Context {
			session: self.session,
			depth: self.depth + 1,
			downs: ~[],
			ups: ~[],
			domain: self.domain,
		}
	}
}

impl<'s> Context<'s> {
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

struct DefaultDomain;
impl Domain for DefaultDomain {
	fn as_any<'a>(&'a self) -> &'a Any { self as &Any }
	fn resolve_time<'s>(&self, _ctx: &mut Context<'s>, _params: &resolve::Params) -> resolve::Step {
		fail!("No active clock domain. What are you timing?");
	}
}
static default_domain: DefaultDomain = DefaultDomain;
