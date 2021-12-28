mod scope;
mod expr;
mod expr_resolve;
mod step;
mod file;
mod index;
mod protocol;
mod function;
mod matchset;
mod primitive;
mod process;
mod data;
mod shape;
mod resolve;

use self::file::ProtocolRef;
pub(crate) use self::step::StepInfo;

pub use self::file::FileScope;
pub use self::index::{ Index };
pub use self::scope::{ Item, Scope };
pub use self::expr::{ Expr, ConcatElem, add_primitive_fns};
pub use self::expr_resolve::{ rexpr, lexpr, on_expr_message, value, pattern_match };
pub use self::function::{ PrimitiveFn, FunctionDef, Func };
pub use self::primitive::{ PrimitiveDef };
pub use self::process::{ Ctxt, Config };
pub use self::protocol::resolve_protocol_invoke;
pub use self::step::{ Step, Message };
pub use self::resolve::{ resolve_token, compile_process_chain, ProcessChain };
pub use self::data::{ Type, DataMode };
pub use self::shape::{ Shape, ShapeMsg, ShapeMsgParam };
pub use self::matchset::{ MatchSet };

#[derive(Copy, Clone, Debug)]
pub enum Dir {
    Up,
    Dn,
}

pub type ValueId = usize;
