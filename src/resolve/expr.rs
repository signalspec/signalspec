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
use resolve::scope::{ Scope, Item, ConstantItem, ValueItem, TupleItem, ValueRef, Dynamic, Ignored, Poison, propagate_pair};
use resolve::types::{Type, TopType, common_type, common_type_all };

pub fn resolve_expr<'s>(ctx: &mut Context<'s>, scope: &Scope<'s>, e: &ast::Expr) -> Item<'s> {
	match *e {
		ast::IgnoreExpr => ValueItem(TopType, Ignored, Ignored),
		ast::ValueExpr(ref val) => ConstantItem(val.clone()),

		ast::FlipExpr(box ref down, box ref up) => {
			let (down_type, down_ref) = match resolve_expr(ctx, scope, down) {
				ConstantItem(v) => (v.get_type(), ctx.down_op(eval::ConstOp(v))),
				ValueItem(down_type, down_ref, _) => (down_type, down_ref),
				_ => fail!("Non-value type in flip")
				
			};
			
			let (up_type, up_ref) = match resolve_expr(ctx, scope, up) {
				ConstantItem(v) => (v.get_type(), ctx.up_op(0, |cell| eval::CheckOp(cell, v.clone()))),
				ValueItem(up_type, up_ref, _) => (up_type, up_ref),
				_ => fail!("Non-value type in flip")
			};
			
			let common_type = common_type(down_type, up_type).expect("Flip expr sides must be of common type");
			ValueItem(common_type, down_ref, up_ref)
		}

		ast::RangeExpr(box ref min_expr, box ref max_expr) => {
			fn get_const_default_num(i: Item, _default: f64) -> f64 {
				match i {
					ConstantItem(NumberValue(v)) => v,
					_ => fail!("Range expressions must be numeric constant")
				}
			}

			let min = get_const_default_num(resolve_expr(ctx, scope, min_expr), Float::neg_infinity());
			let max = get_const_default_num(resolve_expr(ctx, scope, max_expr), Float::infinity());

			let up = ctx.up_op(0, |cell| eval::RangeCheckOp(cell, min, max));

			ValueItem(NumberType, Poison("Range can only be up-evaluated"), up)
		}

		ast::ChooseExpr(box ref e, ref c) => {
			let pairs: Vec<(Value, Value)> = c.iter().map(|&(ref le, ref re)| {
				let l = resolve_expr(ctx, scope, le);
				let r = resolve_expr(ctx, scope, re);
				
				match (l, r) {
					(ConstantItem(lv), ConstantItem(rv)) => (lv, rv),
					_ => fail!("Choose expression arms must be constant, for now")
				}	
			}).collect();
			
			match resolve_expr(ctx, scope, e) {
				ConstantItem(v) => ConstantItem(eval::eval_choose(&v, pairs.as_slice()).expect("Choice up not complete")),
				ValueItem(_t, d, u) => {					
					ValueItem(common_type_all(pairs.iter().map(|&(_, ref r)| r.get_type())).expect("Right sides are not of common type"), 
						d.propagate(|d| ctx.down_op(eval::ChooseOp(d, pairs.clone()))),
						u.propagate(|u| ctx.up_op(u, |cell| eval::ChooseOp(cell, 
							pairs.iter().map(|&(ref l, ref r)| (r.clone(), l.clone())).collect()
						)))
					)
				}
				_ => fail!("Invalid type in choice expr")
			}
			
			// TODO: non-constant arms, check types for case coverage
		}

		ast::ConcatExpr(ref v) =>  {
			let mut len = 0;
			let mut consts = Vec::new();
			
			let mut down_parts = Vec::new();
			let mut down_poison = None;
			
			let mut up_parts = Vec::new();
			let mut up_poison = None;
			
			for e in v.iter() {
				match resolve_expr(ctx, scope, e) {
					ConstantItem(v) => {
						consts.push(v);
					}
					ValueItem(_t, d, u) => {	
						if down_poison.is_none() {
							if consts.len() != 0 { down_parts.push(eval::ConstSlice(consts.clone())); }
							match d {
								Dynamic(id) => down_parts.push(eval::DynElem(id)),
								Ignored => { down_poison = Some(Poison("Some elements are ignored in down evaluation")) }
								Poison(..) => { down_poison = Some(d) }
							}
						}
						
						if up_poison.is_none() {
							if consts.len() != 0 { up_parts.push(eval::ConstSlice(consts.clone())); }
							match u {
								Dynamic(id) => up_parts.push(eval::DynElem(id)),
								Ignored => (),
								Poison(..) => { up_poison = Some(u) }
							}
						}
						
						len += consts.len() + 1;
						consts.clear();
					}
					_ => fail!("Concatinating values that are not vectors")
				}
			}
			
			if len == 0 { // Length is only incremented on non-constant elements
				ConstantItem(VectorValue(consts))
			} else {
				let d = match down_poison {
					Some(p) => p,
					None => {
						if consts.len() != 0 { down_parts.push(eval::ConstSlice(consts.clone())); }
						ctx.down_op(eval::ConcatOp(down_parts))
					}
				};
				
				let u = match up_poison {
					Some(p) => p,
					None => {
						if consts.len() != 0 { up_parts.push(eval::ConstSlice(consts.clone())); }
						if up_parts.len() != 0 {
							let up_ref = ctx.make_register();
							let mut pos = 0;
							for elem in up_parts.into_iter() {
								match elem {
									eval::ConstSlice(v) => {
										let slice = ctx.make_register();
										ctx.add_up_op(slice, eval::ElemOp(up_ref, pos));
										pos += v.len();
										ctx.add_up_op(0, eval::CheckOp(slice, VectorValue(v)));
									}
									eval::DynElem(id) => {
										ctx.add_up_op(id, eval::ElemOp(up_ref, pos));
										pos += 1;
									}
									eval::DynSlice(id, len) => {
										ctx.add_up_op(id, eval::SliceOp(up_ref, pos, len));
										pos += len;
									}
								}
							}
							Dynamic(up_ref)
						} else { Ignored }
					}
				};
				
				ValueItem(VectorType(len), d, u)
			}
		}

		ast::BinExpr(box ref a, op, box ref b) => {			
			fn one_const<'s>(ctx: &mut Context<'s>, op: eval::BinOp, _a_type: Type, a_down: ValueRef, a_up: ValueRef, b: f64) -> Item<'s> {
				// TODO: check type	
				ValueItem(NumberType,
					a_down.propagate(|a| ctx.down_op(eval::BinaryConstOp(a, op, b))),
					a_up.propagate(|a| ctx.up_op(a, |i| eval::BinaryConstOp(i, op.invert(), b)))
				)
			}
			
			match (resolve_expr(ctx, scope, a), resolve_expr(ctx, scope, b)) { 
				(ConstantItem(NumberValue(a)), ConstantItem(NumberValue(b))) => {
					ConstantItem(NumberValue(op.eval(a, b)))
				}
				(ValueItem(a_type, a_down, a_up), ConstantItem(NumberValue(b))) => {
					one_const(ctx, op, a_type, a_down, a_up, b)
				}
				(ConstantItem(NumberValue(a)), ValueItem(b_type, b_down, b_up)) => {
					one_const(ctx, op.swap(), b_type, b_down, b_up, a)
				}
				(ValueItem(_a_type, a_down, _), ValueItem(_b_type, b_down, _)) => {
					// TODO: check type
					ValueItem(NumberType,
						propagate_pair(a_down, b_down, |a, b| ctx.down_op(eval::BinaryOp(a, op, b))),
						Poison("At least one side of an up-evaluated binary operator must be constant")
					)	
				}
				_ => fail!("Invalid types in binary {}", op)
			}
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


pub fn resolve_pattern<'s>(ctx: &mut Context<'s>, scope: &mut Scope<'s>, l: &ast::Expr, r: Item<'s>) {
	match *l {
		ast::IgnoreExpr => (),
		ast::ValueExpr(ref _val) => fail!("patterns cannot be falsifiable"),
		ast::RangeExpr(box ref _min_expr, box ref _max_expr) => fail!("patterns cannot be refutable"),

		ast::FlipExpr(box ref _down, box ref _up) => unimplemented!(),
		ast::ChooseExpr(box ref _e, ref _c) => unimplemented!(),
		ast::ConcatExpr(ref _v) =>  unimplemented!(),
		ast::BinExpr(box ref _a, _op, box ref _b) => unimplemented!(),

		ast::VarExpr(ref name) => {
			scope.bind(name.as_slice(), r);
		}

		ast::TupExpr(ref exprs) => {
			match r {
				TupleItem(v) => {
					if exprs.len() != v.len() {
						fail!("can't bind a tuple with a different length");
					}
					for (expr, item) in exprs.iter().zip(v.into_iter()) {
						resolve_pattern(ctx, scope, expr, item);
					}
				}
				_ => fail!("can't bind a tuple with a non-tuple")
			}
		}

		ast::DotExpr(box ref _lexpr, ref _name) => fail!("Cannot declare a property"),
	}
}




#[cfg(test)]
mod test {
	use super::resolve_expr;
	use session::Session;
	use resolve::context::Context;
	use ast::{
		Value,
			NumberValue,
			IntegerValue,
			VectorValue,
			SymbolValue,
	};
	use resolve::scope::{
		Scope,
		ConstantItem,
	};
	use grammar;

	fn check_const_value(s: &str, v: Value) {
		let ses = Session::new();
		let mut ctx = Context::new(&ses);
		let scope = Scope::new();
		let e = grammar::valexpr(s).unwrap();
		let r = resolve_expr(&mut ctx, &scope, &e);
		assert_eq!(r, ConstantItem(v));
	}
	#[test]
	fn test_const_number() {
		check_const_value("55", NumberValue(55f64));
	}

	#[test]
	fn test_const_symbol() {
		check_const_value("#foo", SymbolValue("foo".to_string()));
	}

	#[test]
	fn test_const_int() {
		check_const_value("#10", IntegerValue(10));
	}

	#[test]
	fn test_const_add() {
		check_const_value("2 + 3", NumberValue(5f64));
	}
	
	/*
	#[test]
	fn test_add_ignore() {
		check_const_value("2 + ignore", ValueItem(NumberType, Ignored, Ignored));
	}
	*/

	#[test]
	#[should_fail]
	fn test_add_wrongtype() {
		check_const_value("2 + #test", NumberValue(2.)); // TODO: make sure it fails for the right reason
	}

	/*
	#[test]
	fn test_const_flip() {
		check_const_value("#l!#r", SymbolType, Constant(SymbolValue("l".to_string())), Constant(SymbolValue("r".to_string())));
	}
	*/

	#[test]
	fn test_const_choice_expr() {
		check_const_value("(#bar)[#foo=#a, #bar=#b, #baz=#c]", SymbolValue("b".to_string()));
	}

	#[test]
	fn test_const_concat_expr() {
		let b = [1, 2, 3].iter().map(|&i| IntegerValue(i)).collect::<Vec<Value>>();
		check_const_value("[#1, #2, #3]", VectorValue(b));
	}
}
