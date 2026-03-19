pub mod ast;
mod ast_node;
mod lexer;
mod binop;
mod grammar;
mod span;

pub use ast_node::{ AstNode, enclosing, dump_tree };
pub use self::binop::BinOp;
pub use self::span::{ SourceFile, FilePos, FileSpan };

pub type Number = num_rational::Rational64;

pub type ParseError = peg::error::ParseError<peg::str::LineCol>;

pub fn parse_literal(input: &str) -> ast::Literal {
   let input = lexer::tokenize(input);
   grammar::signalspec::literal(&input).expect("parser failed")
}

pub fn parse_expr(input: &str) -> ast::Expr {
   let input = lexer::tokenize(input);
   grammar::signalspec::expr(&input).expect("parser failed")
}

pub fn parse_process(input: &str) -> ast::Process {
   let input = lexer::tokenize(input);
   grammar::signalspec::process(&input).expect("parser failed")
}

pub fn parse_module(input: &str) -> ast::Module {
   let input = lexer::tokenize(input);
   grammar::signalspec::module(&input).expect("parser failed")
}
