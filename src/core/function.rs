use std::sync::Arc;

use crate::{syntax::ast, SourceFile};
use super::{ Item, ScopeNames };

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
    pub file: Arc<SourceFile>,
    pub names: ScopeNames,
}
