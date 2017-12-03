mod ast;
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

peg_file! grammar("signalspec.rustpeg");

pub use self::module_loader::{ Ctxt, Module, Test, PrimitiveDef, PrimitiveDefFields };
pub use self::scope::{ Item, Scope };
pub use self::eval::{ Expr, add_primitive_fns };
pub use self::function::PrimitiveFn;

pub use self::grammar::literal as parse_literal;
