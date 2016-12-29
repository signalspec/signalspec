mod ast;
mod scope;
mod expr;
mod eval;
mod step;
mod nfa;
mod dfa;
mod program;
mod module_loader;

peg_file! grammar("signalspec.rustpeg");

pub use self::module_loader::{ ModuleLoader, Module, Test };
pub use self::scope::Item;
pub use self::eval::{ add_primitive_fns };

pub use self::grammar::literal as parse_literal;
