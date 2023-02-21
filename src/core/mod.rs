mod scope;
mod expr;
mod expr_resolve;
mod step;
mod file;
mod index;
pub(crate) mod protocol;
mod function;
mod matchset;
mod primitive;
mod data;
mod shape;
mod resolve;
mod predicate;

use crate::entitymap::entity_key;

use self::file::ProtocolRef;

pub use self::file::FileScope;
pub use self::index::{ Index };
pub use self::scope::{ Item, LeafItem, Scope };
pub use self::expr::{ Expr, ExprDn, ConcatElem, add_primitive_fns};
pub use self::expr_resolve::{ rexpr, rexpr_tup, lexpr, on_expr_message, value };
pub use self::function::{ PrimitiveFn, FunctionDef, Func };
pub use self::primitive::{ PrimitiveDef };
pub use self::step::{ Step, StepId, StepInfo, AltDnArm, AltUpArm, write_tree };
pub use self::resolve::{ resolve_token, compile_process, ProcessChain };
pub use self::data::{ Type, TypeTree, DataMode };
pub use self::shape::{ Shape, ShapeMsg, ShapeMsgParam };
pub use self::matchset::{ MatchSet, MessagePatternSet };
pub use self::predicate::Predicate;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Dir {
    Up,
    Dn,
}

impl Dir {
    pub fn flip(self) -> Dir {
        match self {
            Dir::Up => Dir::Dn,
            Dir::Dn => Dir::Up,
        }
    }
}

entity_key!(pub VarId);
