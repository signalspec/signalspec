use eval::BinOp;
pub use data::Value;

pub struct Module {
    pub imports: Vec<UseDef>,
    pub lets: Vec<LetDef>,
    pub defs: Vec<ModuleEntry>,
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
    For(Vec<(String, Expr)>, Block),
}

pub enum ModuleEntry {
    Signal(Def),
    Interface(Interface),
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
    Shape(Expr, Vec<InterfaceEntry>),
}

#[derive(Debug)]
pub struct UseDef(pub String);

#[derive(Debug)]
pub struct LetDef(pub String, pub Expr);

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
}

#[derive(Debug)]
pub struct Process {
    pub name: String,
    pub arg: Expr,
}
