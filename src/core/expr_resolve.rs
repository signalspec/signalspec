use std::sync::Arc;
use crate::syntax::{ast, Value};
use super::{ Ctxt, Expr, ConcatElem, Scope, Item , FunctionDef, Func, Shape, DataMode };

fn resolve(scope: Option<&Scope>, var_handler: &mut dyn FnMut(&str) -> Expr, e: &ast::Expr) -> Expr {
    match *e {
        ast::Expr::Ignore => Expr::Ignored,
        ast::Expr::Value(ref val) => Expr::Const(val.clone()),

        ast::Expr::Flip(ref down, ref up) => {
            Expr::Flip(
                Box::new(resolve(scope, var_handler, down.as_ref().map_or(&ast::Expr::Ignore, |s| &s.node))),
                Box::new(resolve(scope, var_handler, up.as_ref().map_or(&ast::Expr::Ignore, |s| &s.node))),
            )
        }

        ast::Expr::Range(ref min_expr, ref max_expr) => {
            let min = resolve(scope, var_handler, min_expr);
            let max = resolve(scope, var_handler, max_expr);

            match (min, max) {
                (Expr::Const(Value::Number(l)), Expr::Const(Value::Number(h))) => Expr::Range(l, h),
                (Expr::Const(Value::Integer(l)), Expr::Const(Value::Integer(h))) => Expr::RangeInt(l, h),
                _ => panic!("Range expressions must be numeric constant")
            }
        }

        ast::Expr::Union(ref u) => {
            Expr::Union(u.iter().map(|i| resolve(scope, var_handler, i)).collect())
        }

        ast::Expr::Choose(ref e, ref c) => {
            let pairs: Vec<(Value, Value)> = c.iter().map(|&(ref le, ref re)| {
                let l = resolve(scope, var_handler, le);
                let r = resolve(scope, var_handler, re);

                match (l, r) {
                    (Expr::Const(lv), Expr::Const(rv)) => (lv, rv),
                    _ => panic!("Choose expression arms must be constant, for now")
                }
            }).collect();

            let head = resolve(scope, var_handler, e);
            Expr::Choose(Box::new(head), pairs)
        }

        ast::Expr::Concat(ref v) =>  {
            let elems = v.iter().map(|&(slice_width, ref e)| {
                let expr = resolve(scope, var_handler, e);
                match slice_width {
                    None => ConcatElem::Elem(expr),
                    Some(w) => ConcatElem::Slice(expr, w)
                }
            }).collect();

            Expr::Concat(elems)
        }

        ast::Expr::Bin(ref a, op, ref b) => {
            use self::Expr::Const;
            let lhs = resolve(scope, var_handler, a);
            let rhs = resolve(scope, var_handler, b);
            match (lhs, rhs) {
                (Const(Value::Number(a)),  Const(Value::Number(b))) => Const(Value::Number(op.eval(a, b))),
                (Const(Value::Integer(a)), Const(Value::Integer(b))) => Const(Value::Integer(op.eval(a, b))),
                (Const(Value::Complex(a)), Const(Value::Complex(b))) => Const(Value::Complex(op.eval(a, b))),
                (Const(Value::Complex(a)), Const(Value::Number(b))) => Const(Value::Complex(op.eval(a, b))),
                (Const(Value::Number(a)),  Const(Value::Complex(b))) => Const(Value::Complex(op.eval(a, b))),
                (l, Const(b)) => Expr::BinaryConst(Box::new(l), op, b),
                (Const(a), r) => Expr::BinaryConst(Box::new(r), op.swap(), a),
                _ => panic!("One side of a binary operation must be constant")
            }
        }

        ast::Expr::Var(ref name) => var_handler(name),
        ast::Expr::Call(ref func, ref arg) => {
            let scope = scope.expect("Function call not allowed here");
            match resolve_call(scope, func, arg) {
                Item::Value(v) => v,
                other => panic!("Expcted value item, but function evaluated to {:?}", other),
            }
        }

        ast::Expr::Tup(..) => panic!("Tuple not allowed here"),
        ast::Expr::String(..) => panic!("String not allowed here"),
        ast::Expr::Func{..} => panic!("Function not allowed here"),
    }
}

pub fn value(scope: &Scope, e: &ast::Expr) -> Expr {
    resolve(Some(scope), &mut |name| {
        match scope.get(name) {
            Some(Item::Value(v)) => v,
            Some(..) => panic!("Variable {} is not a value expression", name),
            None => panic!("Undefined variable {}", name),
        }
    }, e)
}

/// Resolve an expression as used in an argument or right hand side of an assignment
pub fn rexpr(scope: &Scope, e: &ast::Expr) -> Item {
    match *e {
        ast::Expr::Var(ref name) => {
            if let Some(s) = scope.get(name) { s } else { panic!("Undefined variable `{}`", name); }
        }

        ast::Expr::Tup(ref items) => {
            Item::Tuple(items.iter().map(|i| rexpr(scope, i)).collect())
        }

        ast::Expr::String(ref s) => Item::String(s.clone()),

        ast::Expr::Func{ ref body, ref args } => {
            Item::Func(Arc::new(FunctionDef::Code(Func{
                args: args.node.clone(),
                body: body.node.clone(),
                scope: scope.clone(),
            })))
        }

        ast::Expr::Call(ref func, ref arg) => {
            resolve_call(scope, func, arg)
        }

        ref other => Item::Value(value(scope, other))
    }
}

fn resolve_call(scope: &Scope, func: &ast::Expr, arg: &ast::Expr) -> Item {
    let func = rexpr(scope, func);
    let arg = rexpr(scope, arg);

    if let Item::Func(func) = func {
        func.apply(arg)
    } else {
        panic!("{:?} is not a function", func)
    }
}

/// Resolve expressions as used in the arguments of an `on` block, defining variables
pub fn on_expr_message(ctx: &Ctxt, scope: &mut Scope, shape: &Shape, variant: &str, exprs: &[ast::SpannedExpr]) -> Vec<Option<Expr>> {
    fn perform_match(ctx: &Ctxt, scope: &mut Scope, shape: &Item, expr: &ast::Expr, dir: DataMode, push: &mut dyn FnMut(Expr)) {
        match (shape, expr) {
            (&Item::Value(Expr::Const(ref c)), &ast::Expr::Value(ref val)) if c == val => (),

            (&Item::Value(ref v), &ast::Expr::Var(ref name)) => { // A variable binding
                let ty = v.get_type();
                let id = scope.new_variable(ctx, &name[..], ty.clone(), dir);
                push(Expr::Variable(id, ty, dir));
            }

            (&Item::Tuple(ref ss), &ast::Expr::Var(ref name)) => { // A variable binding for a tuple
                // Capture a tuple by recursively building a tuple Item containing each of the
                // captured variables
                fn build_tuple(ctx: &Ctxt, dir: DataMode, push: &mut dyn FnMut(Expr), ss: &[Item]) -> Item {
                    Item::Tuple(ss.iter().map(|i| {
                        match *i {
                            Item::Value(Expr::Const(ref c)) => Item::Value(Expr::Const(c.clone())),
                            Item::Tuple(ref t) => build_tuple(ctx, dir, push, &t[..]),
                            Item::Value(ref v) => {
                                let ty = v.get_type();
                                let id = ctx.make_id();
                                push(Expr::Variable(id, ty.clone(), dir));
                                Item::Value(Expr::Variable(id, ty, dir))
                            }

                            _ => panic!("{:?} not allowed in shape", i)
                        }
                    }).collect())
                }

                scope.bind(name, build_tuple(ctx, dir, push, &ss[..]));
            }

            (&Item::Value(_), expr) => { // A match against a refutable pattern
                push(resolve(None, &mut |_| { panic!("Variable binding not allowed here") }, expr));
            }

            (&Item::Tuple(ref ss), &ast::Expr::Tup(ref se)) => {
                for (s, i) in ss.iter().zip(se.iter()) {
                    perform_match(ctx, scope, s, i, dir, push)
                }
            }

            (shape, expr) => panic!("Expression {:?} doesn't match shape {:?}", expr, shape)
        }
    }

    shape.build_variant_fields(variant, |msg_params, push| {
        for (param, expr) in msg_params.iter().zip(exprs) {
            if param.direction.up || param.direction.down {
                perform_match(ctx, scope, &param.item, expr, param.direction, push);
            }
        }
    })
}

#[derive(Clone, Debug)]
pub struct PatternError {
    expected: &'static str,
    found: Item,
}

/// Destructures an item into left-hand-side binding, such as a function argument, returning
/// whether the pattern matched. No runtime evaluation is allowed.
pub fn lexpr(scope: &mut Scope, l: &ast::Expr, r: Item) -> Result<(), PatternError> {
    Ok(match *l {
        ast::Expr::Ignore => (),

        ast::Expr::Var(ref name) => {
            debug!("defined {} = {:?}", name, r);
            scope.bind(name, r);
        }

        ast::Expr::Tup(ref exprs) => {
            match r {
                Item::Tuple(v) => {
                    if exprs.len() != v.len() {
                        return Err(PatternError { expected: "tuple with a different length", found: Item::Tuple(v) });
                    }
                    for (expr, item) in exprs.iter().zip(v.into_iter()) {
                        lexpr(scope, expr, item)?
                    }
                }
                r => return Err(PatternError { expected: "tuple", found: r })
            }
        }

        ast::Expr::Value(ref lv) => {
            match r {
                Item::Value(Expr::Const(ref rv)) if lv == rv => (),
                r => return Err(PatternError { expected: "a constant", found: r })
            }
        }

        _ => {
            let lv = &value(scope, l);
            if let Item::Value(rv) = &r {
                debug!("{:?} == {:?} => {:?}", lv, rv, lv == rv);
                if lv != rv {
                    return Err(PatternError { expected: "tuple, variable, or ignore", found: r })
                }
            }
        }
    })
}

/// Destructure an item into an expression, like lexpr, but allowing runtime pattern-matching
pub fn pattern_match(ctx: &Ctxt, scope: &mut Scope, pat: &ast::Expr, r: &Item, checks: &mut Vec<(Expr, Expr)>) {
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
                let le = resolve(None, &mut |_| { panic!("Variable binding not allowed here")}, other);
                checks.push((le, re.clone()));
            }

            (pat, r) => panic!("can't match {:?} with {:?}", pat, r)
        }
}
