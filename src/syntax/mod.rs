pub mod ast;
mod binop;
mod value;
pub use self::binop::BinOp;
pub use self::value::Value;

peg_file! grammar("signalspec.rustpeg");

pub use self::grammar::{
    ParseError,
    literal as parse_literal,
    valexpr as parse_valexpr,
    primitive_header as parse_primitive_header,
    process_chain as parse_process_chain,
    module as parse_module,
};
