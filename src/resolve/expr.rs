use std::num::Float;

use ast;
use ast::Value;
use eval;
use resolve::context::Context;
use resolve::scope::{ Scope, Item };
use resolve::scope::{ ValueRef, Dynamic, Ignored, Poison, propagate_pair };
use resolve::types::{ mod, Type };

pub fn resolve_expr<'s>(ctx: &mut Context<'s>, scope: &Scope<'s>, e: &ast::Expr) -> Item<'s> {
    match *e {
        ast::Expr::Ignore => Item::Value(types::Bottom, Ignored, Ignored),
        ast::Expr::Value(ref val) => Item::Constant(val.clone()),

        ast::Expr::Flip(box ref down, box ref up) => {
            let (down_type, down_ref) = match resolve_expr(ctx, scope, down) {
                Item::Constant(v) => (v.get_type(), ctx.down_op(eval::ValOp::Const(v))),
                Item::Value(down_type, down_ref, _) => (down_type, down_ref),
                _ => panic!("Non-value type in flip")

            };

            let (up_type, up_ref) = match resolve_expr(ctx, scope, up) {
                Item::Constant(v) => (v.get_type(), ctx.up_op(0, |cell| eval::ValOp::Check(cell, v.clone()))),
                Item::Value(up_type, _, up_ref) => (up_type, up_ref),
                _ => panic!("Non-value type in flip")
            };

            let common_type = types::common(down_type, up_type).expect("Flip expr sides must be of common type");
            Item::Value(common_type, down_ref, up_ref)
        }

        ast::Expr::Range(box ref min_expr, box ref max_expr) => {
            fn get_const_default_num(i: Item, _default: f64) -> f64 {
                match i {
                    Item::Constant(Value::Number(v)) => v,
                    _ => panic!("Range expressions must be numeric constant")
                }
            }

            let min = get_const_default_num(resolve_expr(ctx, scope, min_expr), Float::neg_infinity());
            let max = get_const_default_num(resolve_expr(ctx, scope, max_expr), Float::infinity());

            let up = ctx.up_op(0, |cell| eval::ValOp::RangeCheck(cell, min, max));

            Item::Value(types::Number, Poison("Range can only be up-evaluated"), up)
        }

        ast::Expr::Choose(box ref e, ref c) => {
            let pairs: Vec<(Value, Value)> = c.iter().map(|&(ref le, ref re)| {
                let l = resolve_expr(ctx, scope, le);
                let r = resolve_expr(ctx, scope, re);

                match (l, r) {
                    (Item::Constant(lv), Item::Constant(rv)) => (lv, rv),
                    _ => panic!("Choose expression arms must be constant, for now")
                }
            }).collect();

            match resolve_expr(ctx, scope, e) {
                Item::Constant(v) => Item::Constant(eval::eval_choose(&v, pairs.as_slice()).expect("Choice up not complete")),
                Item::Value(_t, d, u) => {
                    Item::Value(types::common_all(pairs.iter().map(|&(_, ref r)| r.get_type())).expect("Right sides are not of common type"),
                        d.propagate(|d| ctx.down_op(eval::ValOp::Choose(d, pairs.clone()))),
                        u.propagate(|u| ctx.up_op(u, |cell| eval::ValOp::Choose(cell,
                            pairs.iter().map(|&(ref l, ref r)| (r.clone(), l.clone())).collect()
                        )))
                    )
                }
                _ => panic!("Invalid type in choice expr")
            }

            // TODO: non-constant arms, check types for case coverage
        }

        ast::Expr::Concat(ref v) =>  {
            let mut len = 0;
            let mut consts = Vec::new();

            let mut down_parts = Vec::new();
            let mut down_poison = None;

            let mut up_parts = Vec::new();
            let mut up_poison = None;

            for e in v.iter() {
                match resolve_expr(ctx, scope, e) {
                    Item::Constant(v) => {
                        consts.push(v);
                    }
                    Item::Value(_t, d, u) => {
                        if down_poison.is_none() {
                            if consts.len() != 0 { down_parts.push(eval::ValueSrc::ConstSlice(consts.clone())); }
                            match d {
                                Dynamic(id) => down_parts.push(eval::ValueSrc::DynElem(id)),
                                Ignored => { down_poison = Some(Poison("Some elements are ignored in down evaluation")) }
                                Poison(..) => { down_poison = Some(d) }
                            }
                        }

                        if up_poison.is_none() {
                            if consts.len() != 0 { up_parts.push(eval::ValueSrc::ConstSlice(consts.clone())); }
                            match u {
                                Dynamic(id) => up_parts.push(eval::ValueSrc::DynElem(id)),
                                Ignored => (),
                                Poison(..) => { up_poison = Some(u) }
                            }
                        }

                        len += consts.len() + 1;
                        consts.clear();
                    }
                    _ => panic!("Concatinating values that are not vectors")
                }
            }

            if len == 0 { // Length is only incremented on non-constant elements
                Item::Constant(Value::Vector(consts))
            } else {
                let d = match down_poison {
                    Some(p) => p,
                    None => {
                        if consts.len() != 0 { down_parts.push(eval::ValueSrc::ConstSlice(consts.clone())); }
                        ctx.down_op(eval::ValOp::Concat(down_parts))
                    }
                };

                let u = match up_poison {
                    Some(p) => p,
                    None => {
                        if consts.len() != 0 { up_parts.push(eval::ValueSrc::ConstSlice(consts.clone())); }
                        if up_parts.len() != 0 {
                            let up_ref = ctx.make_register();
                            let mut pos = 0;
                            for elem in up_parts.into_iter() {
                                match elem {
                                    eval::ValueSrc::ConstSlice(v) => {
                                        let slice = ctx.make_register();
                                        ctx.add_up_op(slice, eval::ValOp::Elem(up_ref, pos));
                                        pos += v.len();
                                        ctx.add_up_op(0, eval::ValOp::Check(slice, Value::Vector(v)));
                                    }
                                    eval::ValueSrc::DynElem(id) => {
                                        ctx.add_up_op(id, eval::ValOp::Elem(up_ref, pos));
                                        pos += 1;
                                    }
                                    eval::ValueSrc::DynSlice(id, len) => {
                                        ctx.add_up_op(id, eval::ValOp::Slice(up_ref, pos, len));
                                        pos += len;
                                    }
                                }
                            }
                            Dynamic(up_ref)
                        } else { Ignored }
                    }
                };

                Item::Value(types::Vector(len), d, u)
            }
        }

        ast::Expr::Bin(box ref a, op, box ref b) => {
            fn one_const<'s>(ctx: &mut Context<'s>, op: eval::BinOp,
                             _a_type: Type, a_down: ValueRef, a_up: ValueRef, b: f64) -> Item<'s> {
                // TODO: check type
                Item::Value(types::Number,
                    a_down.propagate(|a| ctx.down_op(eval::ValOp::BinaryConst(a, op, b))),
                    a_up.propagate(|a| ctx.up_op(a, |i| eval::ValOp::BinaryConst(i, op.invert(), b)))
                )
            }

            match (resolve_expr(ctx, scope, a), resolve_expr(ctx, scope, b)) {
                (Item::Constant(Value::Number(a)), Item::Constant(Value::Number(b))) => {
                    Item::Constant(Value::Number(op.eval(a, b)))
                }
                (Item::Value(a_type, a_down, a_up), Item::Constant(Value::Number(b))) => {
                    one_const(ctx, op, a_type, a_down, a_up, b)
                }
                (Item::Constant(Value::Number(a)), Item::Value(b_type, b_down, b_up)) => {
                    one_const(ctx, op.swap(), b_type, b_down, b_up, a)
                }
                (Item::Value(_a_type, a_down, _), Item::Value(_b_type, b_down, _)) => {
                    // TODO: check type
                    Item::Value(types::Number,
                        propagate_pair(a_down, b_down, |a, b| ctx.down_op(eval::ValOp::Binary(a, op, b))),
                        Poison("At least one side of an up-evaluated binary operator must be constant")
                    )
                }
                _ => panic!("Invalid types in binary {}", op)
            }
        }

        ast::Expr::Var(ref name) => {
            scope.get(name.as_slice()).expect("Undefined variable")
        }

        ast::Expr::Tup(ref items) => {
            Item::Tuple(items.iter().map(|i| resolve_expr(ctx, scope, i)).collect())
        }

        ast::Expr::Dot(box ref _lexpr, ref _name) => unimplemented!(),
    }
}


pub fn resolve_pattern<'s>(ctx: &mut Context<'s>, scope: &mut Scope<'s>, l: &ast::Expr, r: Item<'s>) {
    match *l {
        ast::Expr::Ignore => (),
        ast::Expr::Value(ref val) => {
            match r {
                Item::Constant(ref v) => {
                    if v != val { panic!("Match always fails"); }
                }
                Item::Value(_t, d, u) => {
                    // TODO: type check
                    d.propagate(|d| ctx.down_op(eval::ValOp::Check(d, val.clone())));
                    u.propagate(|u| ctx.up_op(u, |_| eval::ValOp::Const(val.clone())));
                }
                _ => panic!("Type error")
            }
        },
        ast::Expr::Range(box ref _min_expr, box ref _max_expr) => panic!("patterns cannot be refutable"),
        ast::Expr::Flip(box ref down, box ref up) => {
                match r {
                    Item::Constant(..) => unimplemented!(),
                    Item::Value(t, d, u) => {
                        resolve_pattern(ctx, scope, down, Item::Value(t, d, Ignored));
                        resolve_pattern(ctx, scope, up,   Item::Value(t, Ignored, u));
                    }
                    _ => panic!("Type error")
                }
        }
        ast::Expr::Choose(box ref _e, ref _c) => unimplemented!(),
        ast::Expr::Concat(ref _v) =>  unimplemented!(),
        ast::Expr::Bin(box ref _a, _op, box ref _b) => unimplemented!(),

        ast::Expr::Var(ref name) => {
            debug!("defined {} = {}", name, r);
            scope.bind(name.as_slice(), r);
        }

        ast::Expr::Tup(ref exprs) => {
            match r {
                Item::Tuple(v) => {
                    if exprs.len() != v.len() {
                        panic!("can't bind a tuple with a different length");
                    }
                    for (expr, item) in exprs.iter().zip(v.into_iter()) {
                        resolve_pattern(ctx, scope, expr, item);
                    }
                }
                _ => panic!("can't bind a tuple with a non-tuple")
            }
        }

        ast::Expr::Dot(box ref _lexpr, ref _name) => panic!("Cannot declare a property"),
    }
}

pub fn expr_shape(a: &ast::Expr) -> types::Shape {
    match *a {
        ast::Expr::Ignore => types::Shape::Val(types::Bottom, false, false),
        ast::Expr::Value(ref val) => types::Shape::Val(val.get_type(), true, true),
        ast::Expr::Range(..) | ast::Expr::Flip(..)
        | ast::Expr::Choose(..) | ast::Expr::Concat(..)
        | ast::Expr::Bin(..) | ast::Expr::Var(..) => types::Shape::Val(types::Bottom, true, true),
        ast::Expr::Tup(ref exprs) => types::Shape::Tup(exprs.iter().map(|e| expr_shape(e)).collect()),
        ast::Expr::Dot(box ref _lexpr, ref _name) => panic!("Cannot declare a property"),
    }
}


#[cfg(test)]
mod test {
    use super::resolve_expr;
    use session::Session;
    use resolve::context::{Context, ValueID};
    use resolve::types;
    use ast::Value;
    use resolve::scope::{
        Scope,
        Item,
        ValueRef,
            Dynamic,
            Ignored,
    };
    use grammar;

    static XD: ValueID = 100;
    static XU: ValueID = 101;
    static YD: ValueID = 102;
    static YU: ValueID = 103;

    fn check(s: &str, test: proc(Item)) {
        let ses = Session::new();
        let mut ctx = Context::new(&ses);
        let mut scope = Scope::new();
        scope.bind("x", Item::Value(types::Bottom, Dynamic(XD), Dynamic(XU)));
        scope.bind("y", Item::Value(types::Bottom, Dynamic(YD), Dynamic(YU)));
        let e = grammar::valexpr(s).unwrap();
        let r = resolve_expr(&mut ctx, &scope, &e);
        test(r)
    }

    fn check_const(s: &str, v: Value) {
        check(s, proc(r) assert_eq!(r, Item::Constant(v)));
    }

    fn check_dyn(s: &str, t: types::Type, d: ValueRef, u: ValueRef) {
        check(s, proc(r) {
            assert_eq!(r, Item::Value(t, d, u))
        })
    }

    #[test] fn literal_number() { check_const("55",   Value::Number(55f64)); }
    #[test] fn literal_symbol() { check_const("#foo", Value::Symbol("foo".to_string())); }
    #[test] fn literal_int()    { check_const("#10",  Value::Integer(10)); }

    #[test] fn const_add()      { check_const("2 + 3", Value::Number(5f64)); }
    #[test] fn const_switch()   { check_const("(#bar)[#foo=#a, #bar=#b, #baz=#c]", Value::Symbol("b".to_string())); }
    #[test] fn const_concat()   { check_const("[#1, #2]", Value::Vector(vec![Value::Integer(1), Value::Integer(2)])); }

    #[test] fn flip() {
        check_dyn("x!y", types::Bottom, Dynamic(XD), Dynamic(YU));
        check_dyn("<:x", types::Bottom, Dynamic(XD), Ignored);
        check_dyn(":>x", types::Bottom, Ignored, Dynamic(XU));
    }

    #[test] fn ignore_propagate() {
        check_dyn("2+ignore", types::Number, Ignored, Ignored);
        check_dyn("ignore[#foo=#a]", types::Symbol, Ignored, Ignored);
    }

    #[test]
    #[should_fail]
    fn test_add_wrongtype() {
        check_const("2 + #test", Value::Number(2.)); // TODO: make sure it fails for the right reason
    }
}
