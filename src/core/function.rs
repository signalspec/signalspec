use crate::{syntax::ast, DiagnosticHandler};
use super::{ rexpr, lexpr, Item, Scope };

pub enum FunctionDef {
    /// Function literal
    Code(Func),

    /// Reference to a primitive function
    Primitive(PrimitiveFn),
}

impl FunctionDef {
    pub fn apply(&self, ctx: &dyn DiagnosticHandler, arg: Item) -> Item {
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
    pub fn apply(&self, ctx: &dyn DiagnosticHandler, arg: Item) -> Item {
        let mut scope = self.scope.child();
        lexpr(ctx, &mut scope, &self.args, &arg).expect("failed to match function argument");
        rexpr(ctx, &scope, &self.body)
    }
}
