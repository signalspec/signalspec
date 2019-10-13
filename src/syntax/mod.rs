pub mod ast;
mod binop;
mod value;
mod grammar;
pub use self::binop::BinOp;
pub use self::value::Value;

pub type ParseError = peg::error::ParseError<peg::str::LineCol>;

pub use self::grammar::signalspec::{
    literal as parse_literal,
    valexpr as parse_valexpr,
    primitive_header as parse_primitive_header,
    process_chain as parse_process_chain,
    module as parse_module,
};
