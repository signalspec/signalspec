mod item;
mod expr_dn;
mod op;
mod step;
mod index;
mod function;
mod primitive_fn;
mod data;
mod value;
mod shape;
mod resolve;
mod predicate;
mod derivs;

use std::collections::HashMap;

use crate::entitymap::entity_key;

use self::resolve::expr::TryFromConstant;

pub use self::index::{ Index, FileScope, ProtocolRef };
pub use self::resolve::scope::Scope;
pub use self::item::{Item, LeafItem};
pub use self::expr_dn::{ ExprDn, ExprCtx, ExprDnId, ConcatElem };
pub use self::resolve::expr::{ Expr, ExprKind, rexpr, rexpr_tup, lexpr, value, constant };
pub use self::function::{ PrimitiveFn, FunctionDef, Func };
pub use self::step::{ Step, StepId, ChannelId, ProcId };
pub use self::resolve::{ compile_process, ProcessChain };
pub use self::value::Value;
pub use self::data::{ Type, TypeTree };
pub use self::shape::{ ShapeMode, Shape, ShapeMsg, ShapeMsgParam };
pub use self::predicate::Predicate;
pub(crate) use self::derivs::Derivatives;

pub type ScopeNames = HashMap<String, Item>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Dir {
    Up,
    Dn,
}

impl TryFrom<Value> for Dir {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, ()> {
        match value.as_symbol() {
            Some("up") => Ok(Dir::Up),
            Some("dn") => Ok(Dir::Dn),
            _ => Err(()),
        }
    }
}

impl TryFromConstant for Dir {
    const EXPECTED_MSG: &'static str = "#up | #dn";
}

impl std::fmt::Display for Dir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Dir::Up => "#up",
            Dir::Dn => "#dn",
        })
    }
}

impl Dir {
    pub fn flip(self) -> Dir {
        match self {
            Dir::Up => Dir::Dn,
            Dir::Dn => Dir::Up,
        }
    }
}

entity_key!(pub ValueSrcId);

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub struct ValueSrc(pub ValueSrcId, pub u32);
