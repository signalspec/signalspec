mod item;
mod op;
pub(crate) mod step;
mod index;
mod function;
mod primitive_fn;
mod data;
mod value;
mod shape;
pub(crate) mod resolve;
mod derivs;

use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::core::step::{ExprCtx, StepId};
use crate::diagnostic::Diagnostics;
use crate::entitymap::EntityMap;
use crate::syntax::ast;
use crate::DiagnosticContext;

use self::resolve::expr::TryFromConstant;

pub use self::index::{ Index, FileScope, ProtocolRef };
pub use self::resolve::scope::Scope;
pub use self::item::{Item, LeafItem};
pub use self::function::{ PrimitiveFn, FunctionDef, Func };
pub use self::value::Value;
pub use self::data::{ Type, TypeTree };
pub use self::shape::{ ShapeMode, Shape, ShapeMsg, ShapeMsgParam };
pub(crate) use self::derivs::Derivatives;

pub(crate) use step::{ChannelId, ConnectionId, ProcId, SubProc};

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
    let resolved = resolve::resolve_process(&mut dcx, index, scope, shape_dn, ast);

    if dcx.has_errors() {
        return Err(dcx.diagnostics());
    }

    let (mut steps, root) = step::build_step_tree(&mut dcx, &resolved.vars, resolved.connections, &resolved.action);

    if log_enabled!(log::Level::Debug) {
        let mut buf = String::new();
        steps.write_tree(&mut buf, 0, root).unwrap();
        debug!("Steps:\n{}", buf);
    }

    if dcx.has_errors() {
        return Err(dcx.diagnostics());
    }

    let (fsm, accepting) = steps.fsm(root);

    if dcx.has_errors() {
        return Err(dcx.diagnostics());
    }

    Ok(ProcessChain {
        root,
        fsm,
        accepting,
        conn_dn: resolved.conn_dn,
        up: resolved.up,
        exprs: steps.ecx,
        connections: steps.connections,
        processes: steps.processes,
    })
}
