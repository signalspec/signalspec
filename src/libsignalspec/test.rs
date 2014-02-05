#[allow(dead_code)];

extern mod extra;
extern mod arena;

use std::os;
use expr::{Expr, Value};
use std::str;
use std::io::{stdout,stderr};
use std::io::fs::File;

mod session;
mod expr;
mod grammar;
mod bitv;
mod ast;
mod resolve;
mod clock_event;

fn main() {
	let mut sess = session::Session::new();

	let args = os::args();
	let source_utf8 = File::open(&Path::new(args[1])).read_to_end();
	let source = str::from_utf8(source_utf8);
	let module = grammar::module(source.unwrap());

	println!("ast: {:?}\n", module);

	let module = module.unwrap();

	let mut prelude = resolve::Scope::new();
	let timer = clock_event::timer();
	prelude.names.insert(~"time", resolve::EventItem(timer));

	let mut ctx = session::Context::new(&sess);

	let mut modscope = resolve::resolve_module(&mut ctx, &prelude, &module);
	println!("modscope: {:?}\n", modscope);

	let main = match modscope.get("main").unwrap() {
		resolve::EventItem(s) => s,
		_ => fail!("Main is not an event"),
	};

	let w = clock_event::wire_config();
	let event = main.resolve_call(&mut ctx, &[resolve::EntityItem(&w)], None);
	println!("main: {:?}\n", event);

	resolve::print_step_tree(&event, 0);
}

fn E(s: &str) -> Expr {
	grammar::valexpr(s).unwrap()
}

fn S(s: &str) -> Value {
	expr::SymbolValue(s.to_owned())
}

#[test]
fn test_parse_number() {
	let e = E("55");
	assert_eq!(e.get_type(), expr::NumberType);
	assert_eq!(e.const_down(), Some(expr::NumberValue(55.)));
	assert_eq!(e.const_up(&expr::NumberValue(55.)), true);
	assert_eq!(e.const_up(&expr::NumberValue(99.)), false);
	assert_eq!(e.const_down().unwrap().to_str(), ~"55");
}

#[test]
fn test_parse_symbol() {
	let e = E("#foo");
	assert_eq!(e.get_type(), expr::SymbolType);
	assert_eq!(e.const_down(), Some(S("foo")));
	assert_eq!(e.const_up(&S("foo")), true);
	assert_eq!(e.const_up(&S("bar")), false);
	assert_eq!(e.const_down().unwrap().to_str(), ~"$foo");
}

#[test]
fn test_add() {
	let e = E("2 + 3");
	assert_eq!(e.get_type(), expr::NumberType);
	assert_eq!(e.const_down(), Some(expr::NumberValue(5.)));
}

#[test]
fn test_add_ignore() {
	let e = E("2 + ignore");
	assert_eq!(e.get_type(), expr::NumberType);
	assert_eq!(e.const_down(), None);
}

#[test]
fn test_add_wrongtype() {
	let e = E("2 + #test");
	assert_eq!(e.get_type(), expr::InvalidType);
	assert_eq!(e.const_down(), None);
}

#[test]
fn test_bit_literals() {
	assert_eq!(E("'101").const_down().unwrap().to_str(), ~"'b101");
	assert_eq!(E("'b101").const_down().unwrap().to_str(), ~"'b101");
	assert_eq!(E("'hA5").const_down().unwrap().to_str(), ~"'b10100101");
}

#[test]
fn test_choice_expr() {
	let e = E("(#bar)[#foo=#a, #bar=#b, #baz=#c]");
	assert_eq!(e.const_down(), Some(S("b")));
	assert_eq!(e.const_up(&S("c")), false);
	assert_eq!(e.const_up(&S("x")), false);
	assert_eq!(e.const_up(&S("b")), true);
}

#[test]
fn test_flip() {
	let e = E("#l!#r");
	assert_eq!(e.const_down(), Some(S("l")));
	assert_eq!(e.const_up(&S("l")), false);
	assert_eq!(e.const_up(&S("r")), true);
}

#[test]
fn test_range_expr() {
	let e = E("5..10");
	assert_eq!(e.const_down(), None);

	// should fail if the value is below the range
	assert_eq!(e.const_up(&expr::NumberValue(2.)), false);

	// should be bottom-inclusive
	assert_eq!(e.const_up(&expr::NumberValue(5.)), true);

	// should succeed if the value is inside the range
	assert_eq!(e.const_up(&expr::NumberValue(7.)), true);

	// should be top-exclusive
	assert_eq!(e.const_up(&expr::NumberValue(10.)), false);

	// should fail if the value is above the range
	assert_eq!(e.const_up(&expr::NumberValue(12.)), false);

	// should fail if the value is not a number
	assert_eq!(e.const_up(&S("foo")), false);
}

#[test]
fn test_concat() {
	let e = E("['101, '0010]");
	assert_eq!(e.get_type(), expr::BitsType(7));
	assert_eq!(e.const_down().unwrap().to_str(), ~"'b1010010");
	assert_eq!(e.const_up(&E("'1010010").const_down().unwrap()), true);
	assert_eq!(e.const_up(&E("'1010011").const_down().unwrap()), false);	
}
