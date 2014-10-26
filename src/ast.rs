use std::fmt;

use eval::BinOp;
use resolve::types;

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
    ActionRepeat(Expr, Block),
    ActionCall(Expr, Expr, Option<Block>),
    ActionToken(Expr, Option<Block>),
    ActionOn(Expr, Option<Block>),
}

pub struct Def {
    pub name: String,
    pub param: Expr,
    pub block: Block,
}

pub struct UseDef(pub String);
pub struct LetDef(pub String, pub Expr);

#[deriving(PartialEq, Clone)]
pub enum Value {
    NumberValue(f64),
    IntegerValue(i64),
    SymbolValue(String),
    VectorValue(Vec<Value>),
}

impl Value {
    pub fn get_type(&self) -> types::Type {
        match *self {
            NumberValue(..) => types::Number,
            IntegerValue(..) => types::Integer,
            SymbolValue(..) => types::Symbol,
            VectorValue(ref n) => types::Vector(n.len()),
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
            VectorValue(ref n) => write!(f, "{}", n.to_string()),
        }
    }
}

pub enum Expr {
    ValueExpr(Value),
    TupExpr(Vec<Expr>),
    IgnoreExpr,

    FlipExpr(Box<Expr>, Box<Expr>),
    RangeExpr(Box<Expr>, Box<Expr>),
    ChooseExpr(Box<Expr>, Vec<(Expr, Expr)>),
    ConcatExpr(Vec<Expr>),

    BinExpr(Box<Expr>, BinOp, Box<Expr>),

    VarExpr(String),
    DotExpr(Box<Expr>, String),
}
