use std::num::Float;
use ast;
use ast::Value;
use eval::{Expr, ConcatElem};
use session::{ Session, ValueID };
use resolve::scope::{ Scope, Item };
use resolve::types::{ self, Type };

pub enum Vars<'s:'a, 'a> {
    /// Disallow named variables (for unit tests)
    NoVars,

    /// Use named variables from the provided scope
    Use(&'a Scope<'s>),

    /// Define named variables in the provided scope
    Define(&'a mut Scope<'s>),
}

pub fn resolve_value<'s, 'a>(session: &'s Session<'s>, scope: &mut Vars<'s, 'a>, e: &ast::Expr) -> Expr {
    match resolve(session, scope, e) {
        Item::Value(v) => v,
        other => panic!("Expected a value expression, found {:?}", other)
    }
}

pub fn resolve<'s, 'a>(session: &'s Session<'s>, scope: &mut Vars<'s, 'a>, e: &ast::Expr) -> Item<'s> {
    match *e {
        ast::Expr::Ignore => Item::Value(Expr::Ignored),
        ast::Expr::Value(ref val) => Item::Value(Expr::Const(val.clone())),

        ast::Expr::Flip(box ref down, box ref up) => {
            debug!("Flip: {:?} {:?}", down, up);
            Item::Value(Expr::Flip(
                box resolve_value(session, scope, down),
                box resolve_value(session, scope, up),
            ))
        }

        ast::Expr::Range(box ref min_expr, box ref max_expr) => {
            fn get_const_default_num(i: Item, _default: f64) -> f64 {
                match i {
                    Item::Value(Expr::Const(Value::Number(v))) => v,
                    _ => panic!("Range expressions must be numeric constant")
                }
            }

            let min = get_const_default_num(resolve(session, scope, min_expr), Float::neg_infinity());
            let max = get_const_default_num(resolve(session, scope, max_expr), Float::infinity());

            Item::Value(Expr::Range(min, max))
        }

        ast::Expr::Choose(box ref e, ref c) => {
            let pairs: Vec<(Value, Value)> = c.iter().map(|&(ref le, ref re)| {
                let l = resolve_value(session, scope, le);
                let r = resolve_value(session, scope, re);

                match (l, r) {
                    (Expr::Const(lv), Expr::Const(rv)) => (lv, rv),
                    _ => panic!("Choose expression arms must be constant, for now")
                }
            }).collect();

            let head = resolve_value(session, scope, e);
            Item::Value(Expr::Choose(box head, pairs))
        }

        ast::Expr::Concat(ref v) =>  {
            let elems = v.iter().map(|e| {
                ConcatElem::Elem(resolve_value(session, scope, e))
            }).collect();

            Item::Value(Expr::Concat(elems))
        }

        ast::Expr::Bin(box ref a, op, box ref b) => {
            let v = match (resolve_value(session, scope, a), resolve_value(session, scope, b)) {
                (Expr::Const(Value::Number(a)), Expr::Const(Value::Number(b))) => {
                    Expr::Const(Value::Number(op.eval(a, b)))
                }
                (l, Expr::Const(Value::Number(b))) => Expr::BinaryConst(box l, op, b),
                (Expr::Const(Value::Number(a)), r) => Expr::BinaryConst(box r, op.swap(), a),
                _ => panic!("One side of a binary operation must be constant")
            };
            Item::Value(v)
        }

        ast::Expr::Var(ref name) => {
            match *scope {
                Vars::Use(s) => s.get(name.as_slice()).expect("Undefined variable"),
                Vars::Define(ref mut s) => {
                    let id = session.make_id();
                    let item = Item::Value(Expr::Variable(id));
                    s.bind(name, item.clone());
                    item
                }
                Vars::NoVars => panic!("Variables not allowed here")
            }
        }

        ast::Expr::Tup(ref items) => {
            Item::Tuple(items.iter().map(|i| resolve(session, scope, i)).collect())
        }

        ast::Expr::String(ref s) => Item::String(s.clone()),

        ast::Expr::Dot(box ref _lexpr, ref _name) => unimplemented!(),
    }
}

/// Irrefutable destructuring of an item into an expression, such as the LHS of a `let`,
/// only breaks down tuples and assigns variables.
pub fn pattern<'s>(session: &'s Session<'s>, scope: &mut Scope<'s>, l: &ast::Expr, r: Item<'s>) {
    match *l {
        ast::Expr::Ignore => (),

        ast::Expr::Value(..)
        | ast::Expr::Range(..)
        | ast::Expr::Flip(..)
        | ast::Expr::Choose(..)
        | ast::Expr::Concat(..)
        | ast::Expr::Bin(..) => println!("Cannot destructure into expression {:?}", l),

        ast::Expr::Var(ref name) => {
            debug!("defined {} = {:?}", name, r);
            scope.bind(name.as_slice(), r);
        }

        ast::Expr::Tup(ref exprs) => {
            match r {
                Item::Tuple(v) => {
                    if exprs.len() != v.len() {
                        panic!("can't bind a tuple with a different length");
                    }
                    for (expr, item) in exprs.iter().zip(v.into_iter()) {
                        pattern(session, scope, expr, item);
                    }
                }
                _ => panic!("can't bind a tuple with a non-tuple")
            }
        }

        ast::Expr::String(_) => panic!("Cannot declare a string"),
        ast::Expr::Dot(box ref _lexpr, ref _name) => panic!("Cannot declare a property"),
    }
}
