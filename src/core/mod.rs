mod scope;
mod item;
mod expr;
mod step;
mod file;
mod index;
pub(crate) mod protocol;
mod function;
mod matchset;
mod data;
mod value;
mod shape;
mod resolve;
mod predicate;

use std::collections::HashMap;

use crate::entitymap::entity_key;

use self::resolve::expr::TryFromConstant;
use self::file::ProtocolRef;

pub use self::file::FileScope;
pub use self::index::Index;
pub use self::scope::Scope;
pub use self::item::{Item, LeafItem};
pub use self::expr::{ Expr, ExprDn, ConcatElem };
pub use self::resolve::expr::{ rexpr, rexpr_tup, lexpr, value, constant };
pub use self::function::{ PrimitiveFn, FunctionDef, Func };
pub use self::step::{ Step, StepId, StepInfo, AltDnArm, AltUpArm, write_tree };
pub use self::resolve::{ compile_process, ProcessChain };
pub use self::value::Value;
pub use self::data::{ Type, TypeTree };
pub use self::shape::{ ShapeMode, Shape, ShapeMsg, ShapeMsgParam };
pub use self::matchset::{ MatchSet, MessagePatternSet };
pub use self::predicate::Predicate;

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
