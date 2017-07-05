mod ast;
mod scope;
mod expr;
mod eval;
mod step;
mod nfa;
mod dfa;
mod program;
mod module_loader;
mod protocol;

peg_file! grammar("signalspec.rustpeg");

pub use self::module_loader::{ Ctxt, Module, Test, PrimitiveDef, PrimitiveDefFields, PrimitiveFn };
pub use self::scope::{ Item, Scope };
pub use self::eval::{ add_primitive_fns };

pub use self::grammar::literal as parse_literal;
