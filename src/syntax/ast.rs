pub use super::{ BinOp, Value, FileSpan, Spanned, AstNode };

/// Helper trait for iterating AST children in macro
trait AstWalk {
    fn walk<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>);
}

impl<T> AstWalk for Vec<T> where T:AstWalk {
    fn walk<'a> (&'a self, res: &mut Vec<&'a dyn AstNode>) {
        for i in self {
            i.walk(res);
        }
    }
}

impl<T> AstWalk for Option<T> where T:AstWalk {
    fn walk<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>) {
        for i in self {
            i.walk(res);
        }
    }
}

impl<A, B> AstWalk for (A, B) where A:AstWalk, B:AstWalk {
    fn walk<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>) {
        self.0.walk(res);
        self.1.walk(res);
    }
}

impl<T> AstWalk for Box<T> where T:AstWalk {
    fn walk<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>) {
        (**self).walk(res);
    }
}

// types in AST that aren't AST nodes
impl AstWalk for FileSpan { fn walk(&self, _res: &mut Vec<&dyn AstNode>) {} }
impl AstWalk for u32 { fn walk(&self, _res: &mut Vec<&dyn AstNode>) {} }
impl AstWalk for String { fn walk(&self, _res: &mut Vec<&dyn AstNode>) {} }
impl AstWalk for Value { fn walk(&self, _res: &mut Vec<&dyn AstNode>) {} }
impl AstWalk for BinOp { fn walk(&self, _res: &mut Vec<&dyn AstNode>) {} }

macro_rules! ast_node {
    (pub struct $name:ident {
        pub span: FileSpan,
        $(pub $fname:ident: $ftype:ty),*
        $(,)?
    }) => (
        #[derive(Clone)]
        pub struct $name {
            pub span: FileSpan,
            $(pub $fname: $ftype),*
        }

        impl AstWalk for $name {
            fn walk<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>) {
                res.push(self as &dyn AstNode);
            }
        }

        impl AstNode for $name {
            fn span(&self) -> FileSpan {
                self.span
            }

            fn children<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>) {
                $(AstWalk::walk(&self.$fname, res);)*
            }

            fn node_name(&self) -> &'static str { stringify!($name) }
        }

        impl $name {
            pub fn with_span(self, span: FileSpan) -> $name {
                $name { span, ..self }
            }
        }
    );

    (pub enum $name:ident {
        $($vname:ident ($ftype:ty)),*
        $(,)?
    }) => (
        #[derive(Clone)]
        pub enum $name {
            $($vname($ftype)),*
        }

        impl $name {
            fn node(&self) -> &dyn AstNode {
                match self {
                    $(
                        Self::$vname(e) => e as &dyn AstNode,
                    )*
                }
            }

            pub fn with_span(self, span: FileSpan) -> $name {
                match self {
                    $(
                        Self::$vname(e) => Self::$vname(e.with_span(span)),
                    )*
                }
            }
        }

        impl AstWalk for $name {
            fn walk<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>) {
                res.push(self as &dyn AstNode);
            }
        }

        impl AstNode for $name {
            fn span(&self) -> FileSpan {
                self.node().span()
            }

            fn children<'a>(&'a self, res: &mut Vec<&'a dyn AstNode>) {
                res.push(self.node());
            }

            fn node_name(&self) -> &'static str { stringify!($name) }
        }

        $(impl From<$ftype> for $name {
            fn from(v: $ftype) -> $name {
                Self::$vname(v)
            }
        })*
    );
}

ast_node! {
    pub struct Identifier {
        pub span: FileSpan,
        pub name: String,
    }
}

ast_node! {
    pub struct Module {
        pub span: FileSpan,
        pub entries: Vec<ModuleEntry>,
    }
}

ast_node! {
    pub enum ModuleEntry {
        Let(LetDef),
        WithDef(Def),
        Protocol(Protocol),
    }
}

ast_node! {
    pub struct Attribute {
        pub span: FileSpan,
        pub tok_at: FileSpan,
        pub name: Identifier,
        pub args: ExprTup,
    }
}

ast_node!{
    pub struct LetDef {
        pub span: FileSpan,
        pub tok_let: FileSpan,
        pub name: Identifier,
        pub expr: Expr,
    }
}

ast_node! {
    pub struct Def {
        pub span: FileSpan,
        pub attributes: Vec<Attribute>,
        pub bottom: ProtocolRef,
        pub name: Identifier,
        pub params: Vec<DefParam>,
        pub processes: Vec<Process>,
    }
}

ast_node! {
    pub struct Protocol {
        pub span: FileSpan,
        pub attributes: Vec<Attribute>,
        pub name: Identifier,
        pub param: Expr,
        pub dir: Expr,
        pub entries: Vec<ProtocolEntry>,
    }
}

ast_node!{
    pub enum ProtocolEntry {
        Message(ProtocolMessageDef),
    }
}

ast_node!{
    pub struct ProtocolMessageDef {
        pub span: FileSpan,
        pub name: Identifier,
        pub params: Vec<DefParam>,
    }
}

ast_node!{
    pub struct ProtocolRef {
        pub span: FileSpan,
        pub name: Identifier,
        pub param: Expr,
    }
}

ast_node!{
    pub struct Block {
        pub span: FileSpan,
        pub lets: Vec<LetDef>,
        pub actions: Vec<Action>,
    }
}

ast_node!{
    pub enum Action {
        Process(ProcessChain),
        Repeat(ActionRepeat),
        On(ActionOn),
        For(ActionFor),
        Alt(ActionAlt),
    }
}

ast_node!{
    pub struct ActionRepeat {
        pub span: FileSpan,
        pub dir_count: Option<(Expr, Expr)>,
        pub block: Block,
    }
}

ast_node!{
    pub struct ActionOn {
        pub span: FileSpan,
        pub name: Identifier,
        pub args: ExprTup,
        pub block: Option<Block>,
    }
}

ast_node!{
    pub struct ActionFor {
        pub span: FileSpan,
        pub vars: Vec<(Identifier, Expr)>,
        pub block: Block,
    }
}

ast_node!{
    pub struct ActionAlt {
        pub span: FileSpan,
        pub dir: Expr,
        pub expr: Expr,
        pub arms: Vec<AltArm>,
    }
}

ast_node!{
    pub struct AltArm {
        pub span: FileSpan,
        pub discriminant: Expr,
        pub block: Block,
    }
}

ast_node!{
    pub enum Process {
        Call(ProcessCall),
        Seq(ProcessSeq),
        InferSeq(Block),
    }
}

ast_node!{
    pub struct ProcessCall {
        pub span: FileSpan,
        pub name: Identifier,
        pub args: ExprTup,
    }
}

ast_node!{
    pub struct ProcessSeq {
        pub span: FileSpan,
        pub top: ProtocolRef,
        pub block: Block,
    }
}

ast_node!{
    pub struct ProcessChain {
        pub span: FileSpan,
        pub processes: Vec<Process>,
    }
}

ast_node!{
    pub enum DefParam {
        Const(ParamConst),
        Var(ParamVar),
    }
}

ast_node!{
    pub struct ParamConst {
        pub span: FileSpan,
        pub expr: Expr,
    }
}

ast_node!{
    pub struct ParamVar{
        pub span: FileSpan,
        pub direction: Expr,
        pub expr: Expr,
    }
}

ast_node!{
    pub struct PrimitiveHeader {
        pub span: FileSpan,
        pub name: Identifier,
        pub params: Vec<DefParam>,
        pub top: Option<ProtocolRef>,
        pub bottom: ProtocolRef,
    }
}

ast_node!{
    pub enum Expr {
        Value(ExprLiteral),
        String(ExprString), // Produces an Item, not a Value, because it isn't fixed size
        Func(ExprFunc), // Produces an Item
        Tup(ExprTup),
        Ignore(ExprIgnore),
    
        Union(ExprUnion),
        Flip(ExprFlip),
        Range(ExprRange),
        Choose(ExprChoose),
        Concat(ExprConcat),
    
        Bin(ExprBin),
    
        Call(ExprCall),
        Var(Identifier),
    }
}

ast_node!{
    pub struct ExprLiteral {
        pub span: FileSpan,
        pub value: Value,
    }
}

ast_node!{
    pub struct ExprString {
        pub span: FileSpan,
        pub value: String,
    }
}

ast_node!{
    pub struct ExprFunc {
        pub span: FileSpan,
        pub args: Box<Expr>,
        pub body: Box<Expr>,
    }
}

ast_node!{
    pub struct ExprTup {
        pub span: FileSpan,
        pub items: Vec<Expr>,
    }
}

ast_node!{
    pub struct ExprIgnore {
        pub span: FileSpan,
    }
}

ast_node!{
    pub struct ExprUnion {
        pub span: FileSpan,
        pub items: Vec<Expr>,
    }
}

ast_node!{
    pub struct ExprFlip {
        pub span: FileSpan,
        pub dn: Option<Box<Expr>>,
        pub up: Option<Box<Expr>>,
    }
}

ast_node!{
    pub struct ExprRange {
        pub span: FileSpan,
        pub lo: Box<Expr>,
        pub hi: Box<Expr>,
    }
}

ast_node!{
    pub struct ExprChoose {
        pub span: FileSpan,
        pub e: Box<Expr>,
        pub choices: Vec<(Expr, Expr)>,
    }
}


ast_node!{
    pub struct ExprConcat {
        pub span: FileSpan,
        pub elems: Vec<(Option<u32>, Expr)>
    }
}

ast_node! {
    pub struct ExprBin {
        pub span: FileSpan,
        pub l: Box<Expr>,
        pub op: BinOp,
        pub r: Box<Expr>,
    }
}

ast_node!{
    pub struct ExprCall {
        pub span: FileSpan,
        pub func: Box<Expr>,
        pub arg: ExprTup,
    }
}
