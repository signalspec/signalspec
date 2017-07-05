use super::{ expr, ast, Item, Scope, Ctxt };

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FunctionId(pub usize);

impl From<usize> for FunctionId {
    fn from(i: usize) -> FunctionId { FunctionId(i) }
}

impl From<FunctionId> for usize {
    fn from(i: FunctionId) -> usize { i.0 }
}

pub enum FunctionDef<'s> {
    /// Function literal
    Code(Func<'s>),

    /// Reference to a primitive function
    Primitive(PrimitiveFn<'s>),
}

impl<'s> FunctionDef<'s> {
    pub fn apply(&self, ctx: &'s Ctxt<'s>, arg: Item) -> Item {
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

pub type PrimitiveFn<'a> = fn(Item)->Result<Item, &'static str>;

#[derive(Clone)]
pub struct Func<'s> {
    pub args: &'s ast::Expr,
    pub body: &'s ast::Expr,
    pub scope: Scope,
}

impl<'s> Func<'s> {
    pub fn apply(&self, ctx: &'s Ctxt<'s>, arg: Item) -> Item {
        let mut scope = self.scope.child();
        expr::assign(ctx.session, &mut scope, self.args, arg);
        expr::rexpr(ctx, &mut scope, self.body)
    }
}
