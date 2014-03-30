use std::iter::AdditiveIterator;
use std::fmt;

use context::{
	Context,
	ValueRef,
		Ignored,
		Constant,
		Dynamic,
		Poison,
};

use ast;
use ast::{
	TopType,
	NumberType,
	BitsType,
	Value,
		NumberValue,
		BitsValue,
};
use resolve;
use entity::Entity;
use eval;

// For now, types have nothing to resolve.
// Eventually, some type parameters will be expressions.
pub type Type = ast::TypeExpr;
fn resolve_type(t: ast::TypeExpr) -> Type { t }

#[deriving(Clone)]
pub enum Item<'s> {
	EntityItem(&'s Entity<'s>),
	ValueItem(Type, ValueRef /*Down*/, ValueRef /*Up*/)
}

impl<'s> Eq for Item<'s> {
	fn eq(&self, other: &Item<'s>) -> bool {
		match (self, other) {
			(&ValueItem(ref ta, ref da, ref ua), &ValueItem(ref tb, ref db, ref ub))
				if ta==tb && da==db && ua==ub => true,
			_ => false
		}
	}
}

impl<'s> fmt::Show for Item<'s> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f.buf, "{:?}", self)
	}
}

fn common_type(a: Type, b: Type) -> Option<Type>{
	match (a, b) {
		(TopType, x) | (x, TopType) => Some(x),
		(a, b) if a == b => Some(a),
		_ => None
	}
}

#[inline]
fn common_type_all<T:Iterator<Type>>(mut l: T) -> Option<Type> {
	l.fold(Some(TopType), |opt_a, b|{opt_a.and_then(|a| common_type(a, b))})
}

// This would take an iterator if it weren't for mozilla/rust#5121
fn count_ref_types<'a, T: Iterator<&'a ValueRef>>(mut l: T) -> Result<(uint, uint, uint), ValueRef> {
	let mut ignores = 0;
	let mut constants = 0;
	let mut dynamics = 0;
	for i in l {
		match *i {
			Ignored      => ignores   += 1,
			Constant(..) => constants += 1,
			Dynamic(..)  => dynamics  += 1,
			Poison(..)   => return Err((*i).clone()),
		}
	}
	return Ok((ignores, constants, dynamics));
}

fn resolve_value_expr<'s>(ctx: &mut Context, scope: &resolve::Scope, e: &ast::Expr) ->  (Type, ValueRef /*Down*/, ValueRef /*Up*/) {
	match resolve_expr(ctx, scope, e) {
		ValueItem(t, du, uu) => (t, du, uu),
		_ => fail!("Expected a value expression"),
	}
}

pub fn resolve_expr<'s>(ctx: &mut Context, scope: &resolve::Scope<'s>, e: &ast::Expr) -> Item<'s> {
	match *e {
		ast::IgnoreExpr => ValueItem(TopType, Ignored, Ignored),

		ast::VarExpr(ref name) => {
			scope.get(name.as_slice()).expect("Undefined variable")
		}

		ast::DotExpr(~ref lexpr, ref name) => {
			match resolve_expr(ctx, scope, lexpr) {
				EntityItem(ref e) => e.get_property(ctx, name.as_slice()).expect("Undefined property"),
				_ => fail!("dot only works on entities"),
			}
		}

		ast::ValueExpr(ref val) => ValueItem(val.get_type(), Constant(val.clone()), Constant(val.clone())),

		ast::FlipExpr(~ref down, ~ref up) => {
			let (down_type, down_ref, _     ) = resolve_value_expr(ctx, scope, down);
			let (up_type,   _,        up_ref) = resolve_value_expr(ctx, scope, up);

			let common_type = common_type(down_type, up_type).expect("Flip expr sides must be of common type");

			ValueItem(common_type, down_ref, up_ref)
		}

		ast::RangeExpr(~ref min_expr, ~ref max_expr) => {
			let (min_type, min_ref, _) = resolve_value_expr(ctx, scope, min_expr);
			let (max_type, max_ref, _) = resolve_value_expr(ctx, scope, max_expr);

			match (min_type, max_type) {
				(NumberType, NumberType) => (),
				_ => fail!("Range expressions must be numeric")
			}

			fn get_const_default_num(v: &ValueRef, d: f64) -> f64 {
				match *v {
					Constant(NumberValue(n)) => n,
					Ignored => d,
					_ => fail!("Range bounds must be constant")
				}
			}

			let min = get_const_default_num(&min_ref, Float::neg_infinity());
			let max = get_const_default_num(&max_ref, Float::infinity());

			let up = ctx.up_op_cell(0, |cell| eval::RangeCheckOp(cell, min, max));

			//TODO: up is not quite dynamic; specifically should be able to be used in a Choose arm
			ValueItem(NumberType, Poison("Range can only be up-evaluated"), up)
		}

		ast::ChooseExpr(~ref e, ref c) => {
			let (e_type, e_down, e_up) = resolve_value_expr(ctx, scope, e);

			let res = c.iter().map(|&(ref l, ref r)| {
				(resolve_value_expr(ctx, scope, l), resolve_value_expr(ctx, scope, r))
			}).collect::<Vec<((Type, ValueRef, ValueRef), (Type, ValueRef, ValueRef))>>();

			let l_type = common_type_all(res.iter().map(|&((ref t, _, _), _)| *t))
				.expect("Choose expression left arms not of common type");
			let r_type = common_type_all(res.iter().map(|&(_, (ref t, _, _))| *t))
				.expect("Choose expression right arms not of common type");

			common_type(e_type, l_type)
				.expect("Choose expression left arms and base not of common type");

			let down_pairs = res.iter().map(|&((_, ref l, _), (_, ref r, _))| {
				(ctx.get_const(l), ctx.get_const(r))
			}).collect::<~[_]>();

			let down = match e_down {
				Ignored => Ignored,
				Poison(e) => Poison(e),
				Constant(ref v) => Constant(eval::eval_choose(v, down_pairs).expect("Choice down not complete")),
				Dynamic(d) => ctx.down_op(eval::ChooseOp(d, down_pairs)),
			};

			let up_pairs = res.iter().map(|&((_, _, ref l), (_, _, ref r))| {
				(ctx.get_const(l), ctx.get_const(r))
			}).collect::<Vec<_>>();

			let up = match e_up {
				Ignored => Ignored,
				Poison(e) => Poison(e),
				Constant(ref v) => Constant(eval::eval_choose(v, up_pairs.as_slice()).expect("Choice up not complete")),
				Dynamic(d) => ctx.up_op_cell(d, |cell| eval::ChooseOp(cell,
					// Swap the pair because the value coming in to the op is the right side here
					up_pairs.iter().map(|&(ref a, ref b)| (b.clone(), a.clone())).collect()
				)),
			};

			// TODO: ignores, check types for case coverage
			ValueItem(r_type, down, up)
		}

		ast::ConcatExpr(ref v) =>  {
			let res = v.iter().map(|e| resolve_value_expr(ctx, scope, e)).collect::<Vec<(Type, ValueRef, ValueRef)>>();

			let len = res.iter().map(|&(ref t, _, _)| {
				match *t {
					BitsType(len) => len,
					_ => fail!("Concatinating values that are not bits")
				}
			}).sum();

			fn concat_const<'a, T: Iterator<&'a ValueRef>> (mut l: T) -> Value {
				BitsValue(l.flat_map(|r| {
					match *r {
						Constant(BitsValue(ref b)) => b.iter().map(|x| *x),
						_ => fail!("Counted wrong"),
					}
				}).collect())
			}

			let down_refs = || res.iter().map(|&(_, ref d, _ )| d);
			let down = match count_ref_types(down_refs()) {
				Err(x) => x,
				Ok((_i, 0, 0 )) => Ignored,
				Ok((0, _c, 0 )) => Constant(concat_const(down_refs())),
				Ok((0, _c, _d)) => {
					let args = down_refs().map(|r| {
						match *r {
							Constant(BitsValue(ref b)) => eval::BitsConst(b.clone()),
							Dynamic(cell) => eval::BitsDyn(cell),
							_ => fail!("Typechecker didn't do its job"),
						}
					}).collect();
					ctx.down_op(eval::ConcatOp(args))
				}
				Ok((_i, _c, _d)) => Poison("Some bits are ignored in down evaluation")
			};

			let up_refs = || res.iter().map(|&(_, _, ref u)| u);
			let up = match count_ref_types(up_refs()) {
				Err(x) => x,
				Ok((_i, 0, 0)) => Ignored,
				Ok((0, _c, 0)) => Constant(concat_const(up_refs())),
				// TODO: constant + ignore should be able to be used in constant places like Choose arms
				Ok((_i, _c, _d)) => {
					let cell = ctx.up_cell();
					let mut pos = 0;
					for &(ref t, _, ref r) in res.iter() {
						match *t {
							BitsType(l) => {
								match *r {
									Constant(BitsValue(ref b)) => {
										let check = ctx.up_cell();
										ctx.up_op(0, eval::CheckOp(check, BitsValue(b.clone())));
										ctx.up_op(check, eval::SliceOp(cell, pos, l));
									}
									Dynamic(c) => ctx.up_op(c, eval::SliceOp(cell, pos, l)),
									Ignored => (),
									_ => fail!(),
								}
								pos += l;
							}
							_ => fail!(),
						}
	
					}
					Dynamic(cell)
				}
			};

			ValueItem(BitsType(len), down, up)
		}

		ast::BinExpr(~ref a, op, ~ref b) => {
			let (a_type, a_down, a_up) = resolve_value_expr(ctx, scope, a);
			let (b_type, b_down, b_up) = resolve_value_expr(ctx, scope, b);

			let tp = common_type(a_type, b_type).expect("Incompatible types with binary operator");

			let down = match (a_down, b_down) {
				(Ignored, _) | (_, Ignored) => Ignored,
				(Constant(NumberValue(a)), Constant(NumberValue(b))) => Constant(NumberValue(op.eval(a, b))),
				(Dynamic(a), Constant(NumberValue(b))) => ctx.down_op(eval::BinaryConstOp(a, op, b)),
				(Constant(NumberValue(a)), Dynamic(b)) => ctx.down_op(eval::BinaryConstOp(b, op.swap(), a)),
				(Dynamic(a), Dynamic(b))               => ctx.down_op(eval::BinaryOp(a, op, b)),
				(Constant(..), _) | (_, Constant(..)) => fail!("number constant is not a number?"),
				(Poison(e), _) | (_, Poison(e)) => Poison(e),
			};

			let up = match (a_up, b_up) {
				(Ignored, _) | (_, Ignored) => Ignored,
				(Constant(NumberValue(a)), Constant(NumberValue(b))) =>
					Constant(NumberValue(op.eval(a, b))),
				(Dynamic(a), Constant(NumberValue(b))) =>
					ctx.up_op_cell(a, |cell| eval::BinaryConstOp(cell, op.invert(), b)),
				(Constant(NumberValue(a)), Dynamic(b)) =>
					ctx.up_op_cell(b, |cell| eval::BinaryConstOp(cell, op.swap().invert(), a)),
				(Dynamic(_), Dynamic(_)) =>
					Poison("At least one side of an up-evaluated binary operator must be constant"),
				(Constant(..), _) | (_, Constant(..)) => fail!("number constant is not a number?"),
				(Poison(e), _) | (_, Poison(e)) => Poison(e),
			};

			ValueItem(tp, down, up)
		}
	}
}

#[cfg(test)]
mod test {
	use expr::{ValueItem, resolve_expr};
	use session::Session;
	use context::{
		Context,
		ValueRef,
			Ignored,
			Constant,
	};
	use ast::{
		TypeExpr,
			NumberType,
			SymbolType,
			BitsType,
			IntegerType,
		NumberValue,
		IntegerValue,
		BitsValue,
		SymbolValue,
	};
	use resolve::Scope;
	use grammar;

	fn check_const_value(s: &str, t: TypeExpr, down: ValueRef, up: ValueRef) {
		let ses = Session::new();
		let mut ctx = Context::new(&ses);
		let scope = Scope::new();
		let e = grammar::valexpr(s).unwrap();
		let r = resolve_expr(&mut ctx, &scope, &e);
		assert_eq!(r, ValueItem(t, down, up));
	}

	#[test]
	fn test_const_number() {
		check_const_value("55", NumberType, Constant(NumberValue(55f64)), Constant(NumberValue(55f64)));
	}

	#[test]
	fn test_const_symbol() {
		check_const_value("#foo", SymbolType, Constant(SymbolValue(~"foo")), Constant(SymbolValue(~"foo")));
	}

	#[test]
	fn test_const_int() {
		check_const_value("#10", IntegerType, Constant(IntegerValue(10)), Constant(IntegerValue(10)));
	}

	#[test]
	fn test_const_add() {
		check_const_value("2 + 3", NumberType, Constant(NumberValue(5f64)), Constant(NumberValue(5f64)));
	}

	#[test]
	fn test_add_ignore() {
		check_const_value("2 + ignore", NumberType, Ignored, Ignored);
	}

	#[test]
	#[should_fail]
	fn test_add_wrongtype() {
		check_const_value("2 + #test", NumberType, Ignored, Ignored); // TODO: make sure it fails for the right reason
	}

	#[test]
	fn test_const_flip() {
		check_const_value("#l!#r", SymbolType, Constant(SymbolValue(~"l")), Constant(SymbolValue(~"r")));
	}

	#[test]
	fn test_const_choice_expr() {
		check_const_value("(#bar)[#foo=#a, #bar=#b, #baz=#c]", SymbolType, Constant(SymbolValue(~"b")), Constant(SymbolValue(~"b")));
	}

	#[test]
	fn test_const_concat_expr() {
		let b = ~[true, false, true, true, true, false];
		check_const_value("['101, '11, '0]", BitsType(6), Constant(BitsValue(b.clone())), Constant(BitsValue(b.clone())));
	}
}
