use super::{ expr, ast, Item, Scope, Ctxt };

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FunctionId(pub usize);

impl From<usize> for FunctionId {
    fn from(i: usize) -> FunctionId { FunctionId(i) }
}

impl From<FunctionId> for usize {
    fn from(i: FunctionId) -> usize { i.0 }
}

pub enum FunctionDef {
    /// Function literal
    Code(Func),

    /// Reference to a primitive function
    Primitive(PrimitiveFn),
}

impl FunctionDef {
    pub fn apply(&self, ctx: &Ctxt, arg: Item) -> Item {
        match *self {
            FunctionDef::Primitive(f) => {
                (f)(arg).unwrap()
            },
            FunctionDef::Code(ref func) => {
                func.apply(ctx, arg)
            }
        }
    }
}

pub type PrimitiveFn = fn(Item)->Result<Item, &'static str>;

#[derive(Clone)]
pub struct Func {
    pub args: ast::Expr,
    pub body: ast::Expr,
    pub scope: Scope,
}

impl Func {
    pub fn apply(&self, ctx: &Ctxt, arg: Item) -> Item {
        let mut scope = self.scope.child();
        expr::lexpr(ctx, &mut scope, &self.args, arg).expect("failed to match function argument");
        expr::rexpr(ctx, &scope, &self.body)
    }
}
