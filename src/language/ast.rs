use super::eval::BinOp;
pub use data::Value;

pub struct Module {
    pub entries: Vec<ModuleEntry>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub lets: Vec<LetDef>,
    pub actions: Vec<Action>,
}

#[derive(Debug, Clone)]
pub enum Action {
    Repeat(Expr, Block),
    Call(Expr, Expr, Option<Block>),
    Token(Expr),
    On(Expr, Option<Block>),
    For(Vec<(String, Expr)>, Block),
    Alt(Expr, Vec<AltArm>),
}

#[derive(Debug, Clone)]
pub struct AltArm {
    pub discriminant: Expr,
    pub block: Block,
}

pub enum ModuleEntry {
    Let(LetDef),
    Use(String),
    WithDef(Option<With>, Def),
    Protocol(Protocol),
    Test(Test),
}

pub struct Def {
    pub name: String,
    pub param: Expr,
    pub protocol: Option<Expr>,
    pub block: Block,
}

pub struct Protocol {
    pub name: String,
    pub params: Vec<String>,
    pub entries: Vec<ProtocolEntry>,
}

pub enum ProtocolEntry {
    Message(Expr),
}

pub enum With {
    Protocol { name: String, param: Expr },
    Type(Expr),
    Tup(Vec<With>)
}

#[derive(Debug, Clone)]
pub struct LetDef(pub String, pub Expr);

#[derive(Debug, Clone)]
pub enum Expr {
    Value(Value),
    String(String), // Produces an Item, not a Value, because it isn't fixed size
    Func{ args: Box<Expr>, body: Box<Expr> }, // Produces an Item
    Tup(Vec<Expr>),
    Ignore,

    Union(Vec<Expr>),
    Flip(Box<Expr>, Box<Expr>),
    Range(Box<Expr>, Box<Expr>),
    Choose(Box<Expr>, Vec<(Expr, Expr)>),
    Concat(Vec<Expr>),

    Bin(Box<Expr>, BinOp, Box<Expr>),

    Call(Box<Expr>, Box<Expr>),
    Var(String),
}

#[derive(Debug, Clone)]
pub enum Process {
    Call(String, Expr),
    Literal(ProcessLiteralDirection, Expr, Block),
    Block(Block)
}

#[derive(Debug, Copy, Clone)]
pub enum ProcessLiteralDirection {
    Up,
    Down,
    Both,
    RoundTrip,
}

#[derive(Debug)]
pub struct Test {
    pub should_fail: bool,
    pub processes: Vec<Process>,
}
