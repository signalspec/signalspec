use std::iter::AdditiveIterator;

use ast;
use ast::{
	NumberType,
	VectorType,
	Value,
		NumberValue,
		VectorValue,
};
use eval;
use resolve::context::Context;
use resolve::scope::{ Scope, Item, ValueItem, TupleItem, ValueRef, Constant, Dynamic, Ignored, Poison};
use resolve::types::{Type, TopType, common_type, common_type_all };

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

fn resolve_value_expr<'s>(ctx: &mut Context<'s>, scope: &Scope<'s>, e: &ast::Expr) ->  (Type, ValueRef /*Down*/, ValueRef /*Up*/) {
	match resolve_expr(ctx, scope, e) {
		ValueItem(ref t, ref du, ref uu) => (t.clone(), du.clone(), uu.clone()),
		_ => fail!("Expected a value expression")
	}
}

pub fn resolve_expr<'s>(ctx: &mut Context<'s>, scope: &Scope<'s>, e: &ast::Expr) -> Item<'s> {
	match *e {
		ast::IgnoreExpr => ValueItem(TopType, Ignored, Ignored),

		ast::ValueExpr(ref val) => ValueItem(val.get_type(), Constant(val.clone()), Constant(val.clone())),

		ast::FlipExpr(box ref down, box ref up) => {
			let (down_type, down_ref, _     ) = resolve_value_expr(ctx, scope, down);
			let (up_type,   _,        up_ref) = resolve_value_expr(ctx, scope, up);

			let common_type = common_type(down_type, up_type).expect("Flip expr sides must be of common type");

			ValueItem(common_type, down_ref, up_ref)
		}

		ast::RangeExpr(box ref min_expr, box ref max_expr) => {
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

		ast::ChooseExpr(box ref e, ref c) => {
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
			}).collect::<Vec<_>>();

			let down = match e_down {
				Ignored => Ignored,
				Poison(e) => Poison(e),
				Constant(ref v) => Constant(eval::eval_choose(v, down_pairs.as_slice()).expect("Choice down not complete")),
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
					VectorType(len) => len,
					_ => fail!("Concatinating values that are not vectors")
				}
			}).sum();

			fn concat_const<'a, T: Iterator<&'a ValueRef>> (l: T) -> Value {
				VectorValue(l.flat_map(|r| {
					match *r {
						Constant(VectorValue(ref b)) => b.iter().map(|x| x.clone()),
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
							Constant(VectorValue(ref b)) => eval::Const(b.clone()),
							Dynamic(cell) => eval::Dyn(cell),
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
							VectorType(l) => {
								match *r {
									Constant(VectorValue(ref b)) => {
										let check = ctx.up_cell();
										ctx.up_op(0, eval::CheckOp(check, VectorValue(b.clone())));
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

			ValueItem(VectorType(len), down, up)
		}

		ast::BinExpr(box ref a, op, box ref b) => {
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

		ast::VarExpr(ref name) => {
			scope.get(name.as_slice()).expect("Undefined variable")
		}

		ast::TupExpr(ref items) => {
			TupleItem(items.iter().map(|i| resolve_expr(ctx, scope, i)).collect())
		}

		ast::DotExpr(box ref _lexpr, ref _name) => unimplemented!(),
	}
}

#[cfg(test)]
mod test {
	use super::resolve_expr;
	use session::Session;
	use resolve::context::Context;
	use ast::{
		TypeExpr,
			NumberType,
			SymbolType,
			VectorType,
			IntegerType,
		Value,
			NumberValue,
			IntegerValue,
			VectorValue,
			SymbolValue,
	};
	use resolve::scope::{
		Scope,
		ValueRef,
			Ignored,
			Constant,
		ValueItem,
	};
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
		check_const_value("#foo", SymbolType, Constant(SymbolValue("foo".to_string())), Constant(SymbolValue("foo".to_string())));
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
		check_const_value("#l!#r", SymbolType, Constant(SymbolValue("l".to_string())), Constant(SymbolValue("r".to_string())));
	}

	#[test]
	fn test_const_choice_expr() {
		check_const_value("(#bar)[#foo=#a, #bar=#b, #baz=#c]", SymbolType, Constant(SymbolValue("b".to_string())), Constant(SymbolValue("b".to_string())));
	}

	#[test]
	fn test_const_concat_expr() {
		let b = &[1i64, 0, 1, 1, 1, 0].iter().map(|&i| IntegerValue(i)).collect::<Vec<Value>>();
		check_const_value("['101, '11, '0]", VectorType(6), Constant(VectorValue(b.clone())), Constant(VectorValue(b.clone())));
	}
}
