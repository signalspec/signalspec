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

pub enum Action {
	ActionSeq(Block),
	//ActionPar(Block),
	//ActionRepeat(Block),
	ActionCall(Expr, Call),
	ActionToken(Expr, String, Call),
}

pub struct Def {
	pub name: String,
	pub params: Vec<ParamDef>,
	pub block: Block,
}
pub struct Call {
	pub positional: Vec<Expr>,
	pub body: Option<ActionBody>,
}

pub struct ActionBody {
	pub param_names: Vec<String>,
	pub block: Block,
}

pub struct UseDef(pub String);
pub struct ParamDef {
	pub name: String,
	pub tp: TypeExpr,
	pub default: Option<Expr>
}

pub struct LetDef(pub String, pub Expr);

#[deriving(PartialEq, Clone)]
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

#[deriving(PartialEq, Clone)]
pub enum Value {
	NumberValue(f64),
	IntegerValue(i64),
	SymbolValue(String),
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
			NumberValue(n) => write!(f, "{}", n),
			IntegerValue(n) => write!(f, "{}", n),
			SymbolValue(ref s) => write!(f, "#{}", *s),
			VectorValue(ref n) => write!(f, "[{}]", n.to_string()),
		}
	}
}

pub enum Expr {
	ValueExpr(Value),
	IgnoreExpr,
	
	FlipExpr(Box<Expr>, Box<Expr>),
	RangeExpr(Box<Expr>, Box<Expr>),
	ChooseExpr(Box<Expr>, Vec<(Expr, Expr)>),
	ConcatExpr(Vec<Expr>),

	BinExpr(Box<Expr>, BinOp, Box<Expr>),

	VarExpr(String),
	DotExpr(Box<Expr>, String),
}
