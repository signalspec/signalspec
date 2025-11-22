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

use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::core::step::{ConnectionId, SubProc};
use crate::diagnostic::Diagnostics;
use crate::entitymap::{entity_key, EntityMap};
use crate::syntax::ast;
use crate::DiagnosticContext;

use self::resolve::expr::TryFromConstant;

pub use self::index::{ Index, FileScope, ProtocolRef };
pub use self::resolve::scope::Scope;
pub use self::item::{Item, LeafItem};
pub use self::expr_dn::{ ExprDn, ExprCtx, ExprDnId, ConcatElem };
pub use self::resolve::expr::{ Expr, ExprKind, rexpr, rexpr_tup, lexpr, value, constant };
pub use self::function::{ PrimitiveFn, FunctionDef, Func };
pub use self::step::{ Step, StepId, ChannelId, ProcId };
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

pub struct ProcessChain {
    pub root: StepId,
    pub fsm: BTreeMap<StepId, Derivatives>,
    pub accepting: BTreeSet<StepId>,
    pub exprs: ExprCtx,
    pub conn_dn: ConnectionId,
    pub up: Option<(Shape, ConnectionId)>,
    pub connections: EntityMap<ConnectionId, Shape>,
    pub processes: EntityMap<ProcId, SubProc>,
}

pub fn compile_process(index: &Index, scope: &Scope, shape_dn: Shape, ast: &ast::Process) -> Result<ProcessChain, Diagnostics> {
    let mut dcx = DiagnosticContext::new();
    let mut resolved = resolve::resolve_process(&mut dcx, index, scope, shape_dn, ast);

    if log_enabled!(log::Level::Debug) {
        let mut buf = String::new();
        resolved.steps.write_tree(&mut buf, 0, resolved.step).unwrap();
        debug!("Steps:\n{}", buf);
    }

    if dcx.has_errors() {
        return Err(dcx.diagnostics());
    }

    let (fsm, accepting) = resolved.steps.fsm(resolved.step);

    if dcx.has_errors() {
        return Err(dcx.diagnostics());
    }

    Ok(ProcessChain {
        root: resolved.step,
        fsm,
        accepting,
        conn_dn: resolved.conn_dn,
        up: resolved.up,
        exprs: resolved.steps.ecx,
        connections: resolved.steps.connections,
        processes: resolved.steps.processes,
    })
}
