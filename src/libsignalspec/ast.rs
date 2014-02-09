use bitv::Bitv;

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

pub struct Action {
	entity: Expr,
	posarg: Option<Expr>,
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
	BitsType(uint),
	NumberType,
	EntityType,
	InvalidType,
	TopType
}

#[deriving(Clone, Eq)]
pub enum Value {
	NumberValue(f64),
	SymbolValue(~str),
	BitsValue(Bitv),
}

impl Value {
	pub fn get_type(&self) -> TypeExpr {
		match (*self) {
			NumberValue(..) => NumberType,
			SymbolValue(..) => SymbolType,
			BitsValue(ref n) => BitsType(n.len()),
		}
	}

	pub fn matches(&self, other: &Value) -> bool {
		*self == *other
	}
}

impl ToStr for Value {
	fn to_str(&self) -> ~str {
		match (*self) {
			NumberValue(n) => n.to_str(),
			SymbolValue(ref s) => "$" + *s,
			BitsValue(ref n) => "'b" + n.to_str(),
		}
	}
}

pub enum BinOp {
	BiAdd,
	BiMul,
	BiSub,
	BiDiv,

	BiAnd,
	BiOr,
	BiXor,
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
