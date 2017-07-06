use super::ast;
use super::Ctxt;
use super::eval::{ Expr, ConcatElem };
use super::scope::{ Scope, Item };
use super::function::{FunctionDef, Func};
use data::Value;
use protocol::Shape;
use session::Session;

fn resolve<'s>(ctx: &'s Ctxt<'s>, scope: Option<&Scope>, var_handler: &mut FnMut(&str) -> Expr, e: &'s ast::Expr) -> Expr {
    match *e {
        ast::Expr::Ignore => Expr::Ignored,
        ast::Expr::Value(ref val) => Expr::Const(val.clone()),

        ast::Expr::Flip(box ref down, box ref up) => {
            debug!("Flip: {:?} {:?}", down, up);
            Expr::Flip(
                box resolve(ctx, scope, var_handler, down),
                box resolve(ctx, scope, var_handler, up),
            )
        }

        ast::Expr::Range(box ref min_expr, box ref max_expr) => {
            let min = resolve(ctx, scope, var_handler, min_expr);
            let max = resolve(ctx, scope, var_handler, max_expr);

            match (min, max) {
                (Expr::Const(Value::Number(l)), Expr::Const(Value::Number(h))) => Expr::Range(l, h),
                (Expr::Const(Value::Integer(l)), Expr::Const(Value::Integer(h))) => Expr::RangeInt(l, h),
                _ => panic!("Range expressions must be numeric constant")
            }
        }

        ast::Expr::Union(ref u) => {
            Expr::Union(u.iter().map(|i| resolve(ctx, scope, var_handler, i)).collect())
        }

        ast::Expr::Choose(box ref e, ref c) => {
            let pairs: Vec<(Value, Value)> = c.iter().map(|&(ref le, ref re)| {
                let l = resolve(ctx, scope, var_handler, le);
                let r = resolve(ctx, scope, var_handler, re);

                match (l, r) {
                    (Expr::Const(lv), Expr::Const(rv)) => (lv, rv),
                    _ => panic!("Choose expression arms must be constant, for now")
                }
            }).collect();

            let head = resolve(ctx, scope, var_handler, e);
            Expr::Choose(box head, pairs)
        }

        ast::Expr::Concat(ref v) =>  {
            let elems = v.iter().map(|e| {
                ConcatElem::Elem(resolve(ctx, scope, var_handler, e))
            }).collect();

            Expr::Concat(elems)
        }

        ast::Expr::Bin(box ref a, op, box ref b) => {
            use super::eval::Expr::Const;
            let lhs = resolve(ctx, scope, var_handler, a);
            let rhs = resolve(ctx, scope, var_handler, b);
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
            match resolve_call(ctx, scope, func, arg) {
                Item::Value(v) => v,
                other => panic!("Expcted value item, but function evaluated to {:?}", other),
            }
        }

        ast::Expr::Tup(..) => panic!("Tuple not allowed here"),
        ast::Expr::String(..) => panic!("String not allowed here"),
        ast::Expr::Func{..} => panic!("Function not allowed here"),
    }
}

pub fn value<'s>(ctx: &'s Ctxt<'s>, scope: &Scope, e: &'s ast::Expr) -> Expr {
    resolve(ctx, Some(scope), &mut |name| {
        match scope.get(name) {
            Some(Item::Value(v)) => v,
            Some(..) => panic!("Variable {} is not a value expression", name),
            None => panic!("Undefined variable {}", name),
        }
    }, e)
}

/// Resolve an expression as used in an argument or right hand side of an assignment
pub fn rexpr<'s>(ctxt: &'s Ctxt<'s>, scope: &Scope, e: &'s ast::Expr) -> Item {
    match *e {
        ast::Expr::Var(ref name) => {
            if let Some(s) = scope.get(name) { s } else { panic!("Undefined variable `{}`", name); }
        }

        ast::Expr::Tup(ref items) => {
            Item::Tuple(items.iter().map(|i| rexpr(ctxt, scope, i)).collect())
        }

        ast::Expr::String(ref s) => Item::String(s.clone()),

        ast::Expr::Func{ ref body, ref args } => {
            Item::Func(ctxt.create_function(FunctionDef::Code(Func{
                args: args,
                body: body,
                scope: scope.clone(),
            })))
        }

        ast::Expr::Call(ref func, ref arg) => {
            resolve_call(ctxt, scope, func, arg)
        }

        ref other => Item::Value(value(ctxt, scope, other))
    }
}

fn resolve_call<'s>(ctxt: &'s Ctxt<'s>, scope: &Scope, func: &'s ast::Expr, arg: &'s ast::Expr) -> Item {
    let func = rexpr(ctxt, scope, func);
    let arg = rexpr(ctxt, scope, arg);

    if let Item::Func(fnid) = func {
        ctxt.look_up_function(fnid).apply(ctxt, arg)
    } else {
        panic!("{:?} is not a function", func)
    }
}

/// Resolve an expression as used in the argument of an `on` block, defining variables
pub fn on_expr_message<'s>(ctx: &'s Ctxt<'s>, scope: &mut Scope,
        shape: &Shape, expr: &'s ast::Expr) -> Vec<Option<Expr>> {

    // First test if this shape matches the expression, in order to avoid binding variables
    // for the wrong protocol variant.
    fn try_match(shape: &Shape, e: &ast::Expr) -> bool {
        match (shape, e) {
            (&Shape::Const(ref c), &ast::Expr::Value(ref val)) => c == val,
            (&Shape::Val(ref ty), &ast::Expr::Value(ref val)) => ty.includes(val),
            (&Shape::Tup(ref ss), &ast::Expr::Tup(ref se)) if ss.len() == se.len() => {
                ss.iter().zip(se.iter()).all(|(s,i)| try_match(s, i))
            }
            (&Shape::Protocol{ ref messages, .. }, e) => messages.iter().any(|m| try_match(m, e)),

            // This might fail later, but can't match a different variant
            (&Shape::Val(..), _) => true,
            _ => false
        }
    }

    match (shape, expr) {
        (&Shape::Const(ref c), &ast::Expr::Value(ref val)) if c == val => vec![],

        (&Shape::Val(ref ty), &ast::Expr::Var(ref name)) => { // A variable binding
            let id = scope.new_variable(ctx.session, &name[..], ty.clone());
            vec![Some((Expr::Variable(id, ty.clone())))]
        }

        (&Shape::Tup(ref ss), &ast::Expr::Var(ref name)) => { // A variable binding for a tuple
            // Capture a tuple by recursively building a tuple Item containing each of the
            // captured variables
            fn build_tuple<'s>(ctx: &'s Ctxt<'s>, msg: &mut Vec<Option<Expr>>, ss: &[Shape]) -> Item {
                Item::Tuple(ss.iter().map(|i| {
                    match *i {
                        Shape::Const(ref c) => Item::Value(Expr::Const(c.clone())),
                        Shape::Tup(ref t) => build_tuple(ctx, msg, &t[..]),
                        Shape::Val(ref ty) => {
                            let id = ctx.session.make_id();
                            msg.push(Some((Expr::Variable(id, ty.clone()))));
                            Item::Value(Expr::Variable(id, ty.clone()))
                        }
                        Shape::Protocol{..} => unimplemented!()
                    }
                }).collect())
            }

            let mut msg = Vec::new();
            scope.bind(name, build_tuple(ctx, &mut msg, &ss[..]));
            msg
        }

        (&Shape::Val(_), expr) => { // A match against a refutable pattern
            let e = resolve(ctx, None, &mut |_| { panic!("Variable binding not allowed here") }, expr);
            vec![Some(e)]
        }

        (&Shape::Tup(ref ss), &ast::Expr::Tup(ref se)) => {
            ss.iter().zip(se.iter()).flat_map(|(s, i)| {
                on_expr_message(ctx, scope, s, i)
            }).collect()
        }

        (&Shape::Protocol { ref messages, ..}, expr) => {
            if messages.len() == 1 {
                on_expr_message(ctx, scope, &messages[0], expr)
            } else {
                let mut fields = vec![None];
                let mut matching_variants = 0;
                for (i, shape) in messages.iter().enumerate() {
                    if try_match(shape, expr) {
                        fields[0] = Some(Expr::Const(Value::Integer(i as i64)));
                        fields.extend(on_expr_message(ctx, scope, shape, expr));
                        matching_variants += 1;
                    } else {
                        // Create dummy fields for other variants
                        fields.extend((0..shape.count_fields()).map(|_| None));
                    }
                }

                match matching_variants {
                    1 => fields,
                    0 => panic!("No variant matched {:?}", expr),
                    _ => panic!("Multiple variants matched {:?}", expr)
                }
            }
        }

        (shape, expr) => panic!("Expression {:?} doesn't match shape {:?}", expr, shape)
    }
}


/// Irrefutable destructuring of an item into an expression, such as the LHS of a `let` or a
/// function argument. Only breaks down tuples and assigns variables.
pub fn assign<'s>(session: &Session, scope: &mut Scope, l: &ast::Expr, r: Item) {
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

pub fn pattern_match<'s>(ctx: &'s Ctxt<'s>, scope: &mut Scope, pat: &'s ast::Expr, r: &Item, checks: &mut Vec<(Expr, Expr)>) {
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
                    pattern_match(ctx, scope, expr, item, checks);
                }
            }

            (ref other, &Item::Value(ref re)) => {
                let le = resolve(ctx, None, &mut |_| { panic!("Variable binding not allowed here")}, other);
                checks.push((le, re.clone()));
            }

            (pat, r) => panic!("can't match {:?} with {:?}", pat, r)
        }
}
