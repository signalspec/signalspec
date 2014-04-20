use std::fmt;

use eval::BinOp;

pub struct Module {
	pub imports: Vec<UseDef>,
	pub lets: Vec<LetDef>,
	pub defs: Vec<Def>,
}

pub struct Block {
	pub lets: Vec<LetDef>,
	pub actions: Vec<Action>,
}

pub struct Def {
	pub name: ~str,
	pub params: Vec<ParamDef>,
	pub block: Block,
}

pub enum ActionTarget {
	ActionDef(Expr),
	ActionEntity(Expr, ~str)
}

pub struct Action {
	pub action: ActionTarget,
	pub positional: Vec<Expr>,
	pub body: Option<ActionBody>,
}

pub struct ActionBody {
	pub param_names: Vec<~str>,
	pub block: Block,
}

pub struct UseDef(pub ~str);
pub struct ParamDef {
	pub name: ~str,
	pub tp: TypeExpr,
	pub default: Option<Expr>
}

pub struct LetDef(pub ~str, pub Expr);

#[deriving(Eq, Clone)]
pub enum TypeExpr {
	SymbolType, // TODO: include variants?
	IntegerType, // TODO: range
	BitsType(uint),
	VectorType(uint), //TODO: element type
	NumberType,
	EntityType,
	InvalidType,
	TopType
}

#[deriving(Clone, Eq)]
pub enum Value {
	NumberValue(f64),
	IntegerValue(i64),
	SymbolValue(~str),
	VectorValue(Vec<Value>),
}

impl Value {
	pub fn get_type(&self) -> TypeExpr {
		match *self {
			NumberValue(..) => NumberType,
			IntegerValue(..) => IntegerType,
			SymbolValue(..) => SymbolType,
			VectorValue(ref n) => VectorType(n.len()),
		}
	}

	pub fn matches(&self, other: &Value) -> bool {
		*self == *other
	}
}

impl fmt::Show for Value {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			NumberValue(n) => write!(f.buf, "{}", n),
			IntegerValue(n) => write!(f.buf, "{}", n),
			SymbolValue(ref s) => write!(f.buf, "${}", *s),
			VectorValue(ref n) => write!(f.buf, "[{}]", n.to_str()),
		}
	}
}

pub enum Expr {
	ValueExpr(Value),
	IgnoreExpr,
	
	FlipExpr(~Expr, ~Expr),
	RangeExpr(~Expr, ~Expr),
	ChooseExpr(~Expr, Vec<(Expr, Expr)>),
	ConcatExpr(Vec<Expr>),

	BinExpr(~Expr, BinOp, ~Expr),

	VarExpr(~str),
	DotExpr(~Expr, ~str),
}
