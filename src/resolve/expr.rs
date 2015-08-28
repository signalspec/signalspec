use ast;
use data::{ Value, Shape, ShapeData };
use eval::{Expr, ConcatElem};
use session::Session;
use resolve::scope::{ Scope, Item };
use exec::Message;

fn resolve<'s>(session: &'s Session<'s>, var_handler: &mut FnMut(&str) -> Expr, e: &ast::Expr) -> Expr {
    match *e {
        ast::Expr::Ignore => Expr::Ignored,
        ast::Expr::Value(ref val) => Expr::Const(val.clone()),

        ast::Expr::Flip(box ref down, box ref up) => {
            debug!("Flip: {:?} {:?}", down, up);
            Expr::Flip(
                box resolve(session, var_handler, down),
                box resolve(session, var_handler, up),
            )
        }

        ast::Expr::Range(box ref min_expr, box ref max_expr) => {
            fn get_const_default_num(i: Expr, _default: f64) -> f64 {
                if let Expr::Const(Value::Number(v)) = i { v }
                else { panic!("Range expressions must be numeric constant") }
            }

            let min = get_const_default_num(resolve(session, var_handler, min_expr), ::std::f64::NEG_INFINITY);
            let max = get_const_default_num(resolve(session, var_handler, max_expr), ::std::f64::INFINITY);

            Expr::Range(min, max)
        }

        ast::Expr::Choose(box ref e, ref c) => {
            let pairs: Vec<(Value, Value)> = c.iter().map(|&(ref le, ref re)| {
                let l = resolve(session, var_handler, le);
                let r = resolve(session, var_handler, re);

                match (l, r) {
                    (Expr::Const(lv), Expr::Const(rv)) => (lv, rv),
                    _ => panic!("Choose expression arms must be constant, for now")
                }
            }).collect();

            let head = resolve(session, var_handler, e);
            Expr::Choose(box head, pairs)
        }

        ast::Expr::Concat(ref v) =>  {
            let elems = v.iter().map(|e| {
                ConcatElem::Elem(resolve(session, var_handler, e))
            }).collect();

            Expr::Concat(elems)
        }

        ast::Expr::Bin(box ref a, op, box ref b) => {
            match (resolve(session, var_handler, a), resolve(session, var_handler, b)) {
                (Expr::Const(Value::Number(a)), Expr::Const(Value::Number(b))) => {
                    Expr::Const(Value::Number(op.eval(a, b)))
                }
                (l, Expr::Const(Value::Number(b))) => Expr::BinaryConst(box l, op, b),
                (Expr::Const(Value::Number(a)), r) => Expr::BinaryConst(box r, op.swap(), a),
                _ => panic!("One side of a binary operation must be constant")
            }
        }

        ast::Expr::Var(ref name) => var_handler(name),

        ast::Expr::Tup(..) => panic!("Tuple not allowed here"),
        ast::Expr::String(..) => panic!("String not allowed here"),
    }
}

pub fn value<'s>(session: &'s Session<'s>, scope: &Scope<'s>, e: &ast::Expr) -> Expr {
    resolve(session, &mut |name| {
        match scope.get(name) {
            Some(Item::Value(v)) => v,
            Some(..) => panic!("Variable {} is not a value expression", name),
            None => panic!("Undefined variable {}", name),
        }
    }, e)
}

/// Resolve an expression as used in an argument or right hand side of an assignment
pub fn rexpr<'s>(session: &'s Session<'s>, scope: &Scope<'s>, e: &ast::Expr) -> Item<'s> {
    match *e {
        ast::Expr::Var(ref name) => {
            scope.get(name).expect("Undefined variable")
        }

        ast::Expr::Tup(ref items) => {
            Item::Tuple(items.iter().map(|i| rexpr(session, scope, i)).collect())
        }

        ast::Expr::String(ref s) => Item::String(s.clone()),

        ref other => Item::Value(value(session, scope, other))
    }
}

/// Resolve an expression as used in the argument of an `on` block, defining variables
pub fn on_expr_message<'s>(sess: &'s Session<'s>, scope: &mut Scope<'s>,
        shape: &Shape, expr: &ast::Expr) -> Message {
    let mut msg = Message { components: vec![] };

    fn inner<'s>(sess: &'s Session<'s>, scope: &mut Scope<'s>, msg: &mut Message,
                shape: &ShapeData, e: &ast::Expr) {
        match (shape, e) {
            (&ShapeData::Val(ref ty, _), expr) => {
                msg.components.push(resolve(sess, &mut |name| {
                    let id = scope.new_variable(sess, name, ty.clone());
                    Expr::Variable(id, ty.clone())
                }, expr));
            }
            (&ShapeData::Tup(ref ss), &ast::Expr::Tup(ref se)) => {
                for (s, i) in ss.iter().zip(se.iter()) {
                    inner(sess, scope, msg, s, i);
                }
            }
            // TODO: Tup, Var => create tuple item of variables
            (shape, expr) => panic!("Expression {:?} doesn't match shape {:?}", expr, shape)
        }
    }

    inner(sess, scope, &mut msg, &shape.data, expr);
    msg
}


/// Irrefutable destructuring of an item into an expression, such as the LHS of a `let` or a
/// function argument. Only breaks down tuples and assigns variables.
pub fn assign<'s>(session: &'s Session<'s>, scope: &mut Scope<'s>, l: &ast::Expr, r: Item<'s>) {
    match *l {
        ast::Expr::Ignore => (),

        ast::Expr::Var(ref name) => {
            debug!("defined {} = {:?}", name, r);
            scope.bind(name, r);
        }

        ast::Expr::Tup(ref exprs) => {
            match r {
                Item::Tuple(v) => {
                    if exprs.len() != v.len() {
                        panic!("can't bind a tuple with a different length");
                    }
                    for (expr, item) in exprs.iter().zip(v.into_iter()) {
                        assign(session, scope, expr, item);
                    }
                }
                _ => panic!("can't bind a tuple with a non-tuple")
            }
        }

        _ => println!("Cannot destructure into expression {:?}", l),
    }
}
