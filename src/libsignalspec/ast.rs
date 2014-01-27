// Expressions are included directly in the AST
pub use expr::{
	Expr,
		ValueExpr,
		IgnoreExpr,
		FlipExpr,
		RangeExpr,
		ChooseExpr,
		ConcatExpr,
		BinExpr,
		VarExpr,
		DotExpr,
	Value,
		NumberValue,
		SymbolValue,
		BitsValue,
	Type,
		SymbolType,
		BitsType,
		NumberType,
		EntityType,
		InvalidType,
		TopType,
	BinOp,
		BiAdd,
		BiMul,
};

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
	tp: Type,
	default: Option<Expr>
}

pub struct LetDef(~str, Expr);
