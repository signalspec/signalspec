pub mod ast;
mod scope;
mod expr;
mod eval;
mod step;
mod direction_infer;
mod program;
mod module_loader;
mod protocol;
mod function;
mod exec;
mod matchset;
mod primitive;

peg_file! grammar("signalspec.rustpeg");

pub use self::module_loader::{ Config, Ctxt, Module, Test };
pub use self::scope::{ Item, Scope };
pub use self::eval::{ Expr, add_primitive_fns };
pub use self::function::PrimitiveFn;
pub use self::primitive::{ PrimitiveDef, PrimitiveDefFields, call_primitive };
pub use self::step::{ Step, StepInfo, Message };
pub use self::exec::run;

pub use self::grammar::literal as parse_literal;

pub type ValueID = usize;
