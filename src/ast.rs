use std::fmt;

use eval::BinOp;
use resolve::types;

pub struct Module {
    pub imports: Vec<UseDef>,
    pub lets: Vec<LetDef>,
    pub defs: Vec<Def>,
}

#[derive(Debug)]
pub struct Block {
    pub lets: Vec<LetDef>,
    pub actions: Vec<Action>,
}

#[derive(Debug)]
pub enum Action {
    Seq(Block),
    //Par(Block),
    Repeat(Expr, Block),
    Call(Expr, Expr, Option<Block>),
    Token(Expr, Option<Block>),
    On(Expr, Option<Block>),
}

pub struct Def {
    pub name: String,
    pub param: Expr,
    pub block: Block,
}

#[derive(Debug)]
pub struct UseDef(pub String);

#[derive(Debug)]
pub struct LetDef(pub String, pub Expr);

#[derive(PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Integer(i64),
    Symbol(String),
    Vector(Vec<Value>),
}

impl Value {
    pub fn get_type(&self) -> types::Type {
        match *self {
            Value::Number(..) => types::Number,
            Value::Integer(..) => types::Integer,
            Value::Symbol(..) => types::Symbol,
            Value::Vector(ref n) => types::Vector(n.len()),
        }
    }

    pub fn matches(&self, other: &Value) -> bool {
        *self == *other
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Integer(n) => write!(f, "{}", n),
            Value::Symbol(ref s) => write!(f, "#{:?}", *s),
            Value::Vector(ref n) => write!(f, "{:?}", n),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Value(Value),
    String(String), // Produces an Item, not a Value, because it isn't fixed size
    Tup(Vec<Expr>),
    Ignore,

    Flip(Box<Expr>, Box<Expr>),
    Range(Box<Expr>, Box<Expr>),
    Choose(Box<Expr>, Vec<(Expr, Expr)>),
    Concat(Vec<Expr>),

    Bin(Box<Expr>, BinOp, Box<Expr>),

    Var(String),
    Dot(Box<Expr>, String),
}
