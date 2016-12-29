use super::ast;
use super::eval::{ Expr, ConcatElem };
use super::scope::{ Scope, Item, Func };
use super::step::Message;
use data::{ Value, Shape, ShapeVariant, ShapeData };
use session::Session;

fn resolve<'s>(session: &Session, scope: Option<&Scope<'s>>, var_handler: &mut FnMut(&str) -> Expr, e: &'s ast::Expr) -> Expr {
    match *e {
        ast::Expr::Ignore => Expr::Ignored,
        ast::Expr::Value(ref val) => Expr::Const(val.clone()),

        ast::Expr::Flip(box ref down, box ref up) => {
            debug!("Flip: {:?} {:?}", down, up);
            Expr::Flip(
                box resolve(session, scope, var_handler, down),
                box resolve(session, scope, var_handler, up),
            )
        }

        ast::Expr::Range(box ref min_expr, box ref max_expr) => {
            let min = resolve(session, scope, var_handler, min_expr);
            let max = resolve(session, scope, var_handler, max_expr);

            match (min, max) {
                (Expr::Const(Value::Number(l)), Expr::Const(Value::Number(h))) => Expr::Range(l, h),
                (Expr::Const(Value::Integer(l)), Expr::Const(Value::Integer(h))) => Expr::RangeInt(l, h),
                _ => panic!("Range expressions must be numeric constant")
            }
        }

        ast::Expr::Union(ref u) => {
            Expr::Union(u.iter().map(|i| resolve(session, scope, var_handler, i)).collect())
        }

        ast::Expr::Choose(box ref e, ref c) => {
            let pairs: Vec<(Value, Value)> = c.iter().map(|&(ref le, ref re)| {
                let l = resolve(session, scope, var_handler, le);
                let r = resolve(session, scope, var_handler, re);

                match (l, r) {
                    (Expr::Const(lv), Expr::Const(rv)) => (lv, rv),
                    _ => panic!("Choose expression arms must be constant, for now")
                }
            }).collect();

            let head = resolve(session, scope, var_handler, e);
            Expr::Choose(box head, pairs)
        }

        ast::Expr::Concat(ref v) =>  {
            let elems = v.iter().map(|e| {
                ConcatElem::Elem(resolve(session, scope, var_handler, e))
            }).collect();

            Expr::Concat(elems)
        }

        ast::Expr::Bin(box ref a, op, box ref b) => {
            use super::eval::Expr::Const;
            let lhs = resolve(session, scope, var_handler, a);
            let rhs = resolve(session, scope, var_handler, b);
            match (lhs, rhs) {
                (Const(Value::Number(a)),  Const(Value::Number(b))) => Const(Value::Number(op.eval(a, b))),
                (Const(Value::Integer(a)), Const(Value::Integer(b))) => Const(Value::Integer(op.eval(a, b))),
                (Const(Value::Complex(a)), Const(Value::Complex(b))) => Const(Value::Complex(op.eval(a, b))),
                (Const(Value::Complex(a)), Const(Value::Number(b))) => Const(Value::Complex(op.eval(a, b))),
                (Const(Value::Number(a)),  Const(Value::Complex(b))) => Const(Value::Complex(op.eval(a, b))),
                (l, Const(b)) => Expr::BinaryConst(box l, op, b),
                (Const(a), r) => Expr::BinaryConst(box r, op.swap(), a),
                _ => panic!("One side of a binary operation must be constant")
            }
        }

        ast::Expr::Var(ref name) => var_handler(name),
        ast::Expr::Call(ref func, ref arg) => {
            let scope = scope.expect("Function call not allowed here");
            match resolve_call(session, scope, func, arg) {
                Item::Value(v) => v,
                other => panic!("Expcted value item, but function evaluated to {:?}", other),
            }
        }

        ast::Expr::Tup(..) => panic!("Tuple not allowed here"),
        ast::Expr::String(..) => panic!("String not allowed here"),
        ast::Expr::Func{..} => panic!("Function not allowed here"),
    }
}

pub fn value<'s>(session: &Session, scope: &Scope<'s>, e: &'s ast::Expr) -> Expr {
    resolve(session, Some(scope), &mut |name| {
        match scope.get(name) {
            Some(Item::Value(v)) => v,
            Some(..) => panic!("Variable {} is not a value expression", name),
            None => panic!("Undefined variable {}", name),
        }
    }, e)
}

/// Resolve an expression as used in an argument or right hand side of an assignment
pub fn rexpr<'s>(session: &Session, scope: &Scope<'s>, e: &'s ast::Expr) -> Item<'s> {
    match *e {
        ast::Expr::Var(ref name) => {
            if let Some(s) = scope.get(name) { s } else { panic!("Undefined variable `{}`", name); }
        }

        ast::Expr::Tup(ref items) => {
            Item::Tuple(items.iter().map(|i| rexpr(session, scope, i)).collect())
        }

        ast::Expr::String(ref s) => Item::String(s.clone()),

        ast::Expr::Func{ ref body, ref args } => Item::Func(Func{
            args: args,
            body: body,
            scope: Box::new(scope.clone()),
        }),

        ast::Expr::Call(ref func, ref arg) => {
            resolve_call(session, scope, func, arg)
        }

        ref other => Item::Value(value(session, scope, other))
    }
}

fn resolve_call<'s>(session: &Session, scope: &Scope<'s>, func: &'s ast::Expr, arg: &'s ast::Expr) -> Item<'s> {
    let func = rexpr(session, scope, func);
    let arg = rexpr(session, scope, arg);
    match func {
        Item::PrimitiveFn(f) => {
            (f)(arg).unwrap()
        },
        Item::Func(func) => {
            func.apply(session, arg)
        }
        i => panic!("{:?} is not a function", i),
    }
}

/// Resolve an expression as used in the argument of an `on` block, defining variables
pub fn on_expr_message<'shape>(sess: &Session, scope: &mut Scope,
        shape: &'shape mut Shape, expr: &ast::Expr) -> (&'shape mut ShapeVariant, Message) {

    fn try_variant(shape: &ShapeData, e: &ast::Expr) -> bool {
        match (shape, e) {
            (&ShapeData::Const(ref c), &ast::Expr::Value(ref val)) => c == val,
            (&ShapeData::Val(ref ty, _), &ast::Expr::Value(ref val)) => {
                ty.includes(val)
            }
            (&ShapeData::Val(..), _) => {
                true
            }
            (&ShapeData::Tup(ref ss), &ast::Expr::Tup(ref se)) => {
                ss.iter().zip(se.iter()).all(|(s,i)| try_variant(s, i))
            }
            (&ShapeData::Tup(..), &ast::Expr::Var(..)) => true,
            _ => false
        }
    }

    fn inner(sess: &Session, scope: &mut Scope, msg: &mut Message,
                shape: &ShapeData, e: &ast::Expr) {
        match (shape, e) {
            (&ShapeData::Const(..), _) => (),

            (&ShapeData::Val(ref ty, _), &ast::Expr::Var(ref name)) => { // A variable binding
                let id = scope.new_variable(sess, &name[..], ty.clone());
                msg.components.push(Expr::Variable(id, ty.clone()))
            }

            (&ShapeData::Tup(ref ss), &ast::Expr::Var(ref name)) => { // A variable binding for a tuple
                // Capture a tuple by recursively building a tuple Item containing each of the
                // captured variables
                fn build_tuple<'s>(sess: &Session, msg: &mut Message, ss: &[ShapeData]) -> Item<'s> {
                    Item::Tuple(ss.iter().map(|i| {
                        match *i {
                            ShapeData::Const(ref c) => Item::Value(Expr::Const(c.clone())),
                            ShapeData::Tup(ref t) => build_tuple(sess, msg, &t[..]),
                            ShapeData::Val(ref ty, _) => {
                                let id = sess.make_id();
                                msg.components.push(Expr::Variable(id, ty.clone()));
                                Item::Value(Expr::Variable(id, ty.clone()))
                            }
                        }
                    }).collect())
                }

                scope.bind(name, build_tuple(sess, msg, &ss[..]))
            }

            (&ShapeData::Val(_, _), expr) => { // A match against a refutable pattern
                msg.components.push(
                    resolve(sess, None, &mut |_| { panic!("Variable binding not allowed here") }, expr));
            }
            (&ShapeData::Tup(ref ss), &ast::Expr::Tup(ref se)) => {
                for (s, i) in ss.iter().zip(se.iter()) {
                    inner(sess, scope, msg, s, i);
                }
            }

            (shape, expr) => panic!("Expression {:?} doesn't match shape {:?}", expr, shape)
        }
    }

    for (idx, variant) in shape.variants.iter_mut().enumerate() {
        if try_variant(&variant.data, expr) {
            let mut msg = Message { tag: idx, components: vec![] };
            inner(sess, scope, &mut msg, &variant.data, expr);
            return (variant, msg)
        }
    }

    panic!("Expression {:?} doesn't match shape", expr)
}


/// Irrefutable destructuring of an item into an expression, such as the LHS of a `let` or a
/// function argument. Only breaks down tuples and assigns variables.
pub fn assign<'s>(session: &Session, scope: &mut Scope<'s>, l: &ast::Expr, r: Item<'s>) {
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

pub fn pattern_match<'s>(session: &Session, scope: &mut Scope<'s>, pat: &ast::Expr, r: &Item<'s>, checks: &mut Vec<(Expr, Expr)>) {
        match (pat, r) {
            (&ast::Expr::Ignore, _) => (),

            (&ast::Expr::Var(ref name), r) => {
                debug!("defined {} = {:?}", name, r);
                scope.bind(name, r.clone());
            }

            (&ast::Expr::Tup(ref exprs), &Item::Tuple(ref v)) => {
                if exprs.len() != v.len() {
                    panic!("can't match a tuple with a different length");
                }
                for (expr, item) in exprs.iter().zip(v.iter()) {
                    pattern_match(session, scope, expr, item, checks);
                }
            }

            (ref other, &Item::Value(ref re)) => {
                let le = resolve(session, None, &mut |_| { panic!("Variable binding not allowed here")}, other);
                checks.push((le, re.clone()));
            }

            (pat, r) => panic!("can't match {:?} with {:?}", pat, r)
        }
}
