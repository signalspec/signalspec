use crate::syntax::ast;
use super::{ Item, Scope };

pub type PrimitiveFn = fn(Item)->Result<Item, &'static str>;

pub enum FunctionDef {
    /// Function literal
    Code(Func),

    /// Reference to a primitive function
    Primitive(PrimitiveFn),
}

#[derive(Clone)]
pub struct Func {
    pub args: ast::Expr,
    pub body: ast::Expr,
    pub scope: Scope,
}
