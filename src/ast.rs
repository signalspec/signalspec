use eval::BinOp;
pub use data::Value;

pub struct Module {
    pub imports: Vec<UseDef>,
    pub lets: Vec<LetDef>,
    pub defs: Vec<ModuleEntry>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub lets: Vec<LetDef>,
    pub actions: Vec<Action>,
}

#[derive(Debug, Clone)]
pub enum Action {
    Seq(Block),
    //Par(Block),
    Repeat(Expr, Block),
    Call(Expr, Expr, Option<Block>),
    Token(Expr),
    On(Expr, Option<Block>),
    For(Vec<(String, Expr)>, Block),
}

pub enum ModuleEntry {
    Signal(Def),
    Interface(Interface),
    Test(Test),
}

pub struct Def {
    pub name: String,
    pub param: Expr,
    pub interface: Option<Expr>,
    pub block: Block,
}

pub struct Interface {
    pub name: String,
    pub entries: Vec<InterfaceEntry>,
}

pub enum InterfaceEntry {
    Shape(Expr),
}

#[derive(Debug, Clone)]
pub struct UseDef(pub String);

#[derive(Debug, Clone)]
pub struct LetDef(pub String, pub Expr);

#[derive(Debug, Clone)]
pub enum Expr {
    Value(Value),
    String(String), // Produces an Item, not a Value, because it isn't fixed size
    Tup(Vec<Expr>),
    Ignore,

    Union(Vec<Expr>),
    Flip(Box<Expr>, Box<Expr>),
    Range(Box<Expr>, Box<Expr>),
    Choose(Box<Expr>, Vec<(Expr, Expr)>),
    Concat(Vec<Expr>),

    Bin(Box<Expr>, BinOp, Box<Expr>),

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
