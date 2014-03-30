use std::fmt;

use eval::BinOp;

pub struct Module {
	imports: ~[UseDef],
	lets: ~[LetDef],
	defs: ~[Def],
}

pub struct Block {
	lets: ~[LetDef],
	actions: ~[Action],
}

pub struct Def {
	name: ~str,
	params: ~[ParamDef],
	block: Block,
}

pub enum ActionTarget {
	ActionDef(Expr),
	ActionEntity(Expr, ~str)
}

pub struct Action {
	action: ActionTarget,
	positional: ~[Expr],
	body: Option<ActionBody>,
}

pub struct ActionBody {
	param_names: ~[~str],
	block: Block,
}

pub struct UseDef(~str);
pub struct ParamDef {
	name: ~str,
	tp: TypeExpr,
	default: Option<Expr>
}

pub struct LetDef(~str, Expr);

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
	VectorValue(~[Value]),
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
	ChooseExpr(~Expr, ~[(Expr, Expr)]),
	ConcatExpr(~[Expr]),

	BinExpr(~Expr, BinOp, ~Expr),

	VarExpr(~str),
	DotExpr(~Expr, ~str),
}
