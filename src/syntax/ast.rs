pub use super::{ BinOp, Value, Spanned };

pub struct Module {
    pub entries: Vec<Spanned<ModuleEntry>>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub lets: Vec<Spanned<LetDef>>,
    pub actions: Vec<Spanned<Action>>,
}

#[derive(Debug, Clone)]
pub enum Action {
    Process(Vec<Process>),
    Repeat(Option<SpannedExpr>, Block),
    On(String, Vec<SpannedExpr>, Option<Block>),
    For(Vec<(String, SpannedExpr)>, Block),
    Alt(SpannedExpr, Vec<AltArm>),
}

#[derive(Debug, Clone)]
pub enum Process {
    Call(String, Vec<SpannedExpr>),
    Seq(ProtocolRef, Block),
    InferSeq(Block),
}

#[derive(Debug, Clone)]
pub struct AltArm {
    pub discriminant: SpannedExpr,
    pub block: Block,
}

pub enum ModuleEntry {
    Let(LetDef),
    Use(String),
    WithDef(Def),
    Protocol(Protocol),
    Test(Test),
}

pub struct Def {
    pub name: String,
    pub params: Vec<Spanned<DefParam>>,
    pub bottom: ProtocolRef,
    pub processes: Vec<Process>,
}

#[derive(Debug, Clone)]
pub enum DefParam {
    Const(SpannedExpr),
    Var { direction: SpannedExpr, value: SpannedExpr },
}

pub struct PrimitiveHeader {
    pub name: String,
    pub params: Vec<Spanned<DefParam>>,
    pub top: Option<ProtocolRef>,
    pub bottom: ProtocolRef,
}

pub struct Protocol {
    pub name: String,
    pub param: SpannedExpr,
    pub entries: Vec<ProtocolEntry>,
}

pub enum ProtocolEntry {
    Message(String, Vec<Spanned<DefParam>>),
}

#[derive(Debug, Clone)]
pub struct ProtocolRef {
    pub name: String,
    pub param: SpannedExpr,
}

#[derive(Debug, Clone)]
pub struct LetDef(pub String, pub SpannedExpr);

pub type SpannedExpr = Spanned<Expr>;

#[derive(Debug, Clone)]
pub enum Expr {
    Value(Value),
    String(String), // Produces an Item, not a Value, because it isn't fixed size
    Func{ args: Box<SpannedExpr>, body: Box<SpannedExpr> }, // Produces an Item
    Tup(Vec<SpannedExpr>),
    Ignore,

    Union(Vec<SpannedExpr>),
    Flip(Option<Box<SpannedExpr>>, Option<Box<SpannedExpr>>),
    Range(Box<SpannedExpr>, Box<SpannedExpr>),
    Choose(Box<SpannedExpr>, Vec<(SpannedExpr, SpannedExpr)>),
    Concat(Vec<(Option<usize>, SpannedExpr)>),

    Bin(Box<SpannedExpr>, BinOp, Box<SpannedExpr>),

    Call(Box<SpannedExpr>, Box<SpannedExpr>),
    Var(String),
}

#[derive(Debug)]
pub struct Test {
    pub should_fail: bool,
    pub processes: Vec<Process>,
}
