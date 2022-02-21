use std::sync::Arc;
use crate::{syntax::{ast, Value}, tree::Tree};
use super::{ConcatElem, Expr, Func, FunctionDef, Item, Scope, ShapeMsg, ExprDn, VarId, LeafItem };

fn resolve(scope: Option<&Scope>, var_handler: &mut dyn FnMut(&str) -> Expr, e: &ast::SpannedExpr) -> Expr {
    match e.node {
        ast::Expr::Ignore => Expr::Ignored,
        ast::Expr::Value(ref val) => Expr::Const(val.clone()),

        ast::Expr::Flip(ref down, ref up) => {
            Expr::Flip(
                Box::new(down.as_ref().map_or(Expr::Ignored,  |dn| resolve(scope, var_handler, dn))),
                Box::new(up.as_ref().map_or(Expr::Ignored,  |up| resolve(scope, var_handler, up))),
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
            let head = resolve(scope, var_handler, e);

            if let Expr::Const(value) = head {
                let re = c.iter().find(|&(ref le, _)| {
                    let l = resolve(scope, var_handler, le);
                    l.eval_up(&mut |_, _| panic!("Variable not expected here"), value.clone())
                }).map(|x| &x.1).unwrap_or_else(|| {
                    panic!("No match for {} at {:?}", value, e.span);
                });
                resolve(scope, var_handler, re)
            } else {
                let pairs: Vec<(Value, Value)> = c.iter().map(|&(ref le, ref re)| {
                    let l = resolve(scope, var_handler, le);
                    let r = resolve(scope, var_handler, re);

                    match (l, r) {
                        (Expr::Const(lv), Expr::Const(rv)) => (lv, rv),
                        _ => panic!("Choose expression arms must be constant, for now")
                    }
                }).collect();

                Expr::Choose(Box::new(head), pairs)
            }
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
                Item::Leaf(LeafItem::Value(v)) => v,
                other => panic!("Expcted value item, but function evaluated to {:?}", other),
            }
        }

        ast::Expr::Tup(..) => panic!("Tuple not allowed here"),
        ast::Expr::String(..) => panic!("String not allowed here"),
        ast::Expr::Func{..} => panic!("Function not allowed here"),
    }
}

pub fn value(scope: &Scope, e: &ast::SpannedExpr) -> Expr {
    resolve(Some(scope), &mut |name| {
        match scope.get(name) {
            Some(Item::Leaf(LeafItem::Value(v))) => v,
            Some(..) => panic!("Variable {} is not a value expression", name),
            None => panic!("Undefined variable {}", name),
        }
    }, e)
}

/// Resolve an expression as used in an argument or right hand side of an assignment
pub fn rexpr(scope: &Scope, e: &ast::SpannedExpr) -> Item {
    match e.node {
        ast::Expr::Var(ref name) => {
            if let Some(s) = scope.get(name) { s } else { panic!("Undefined variable `{}`", name); }
        }

        ast::Expr::Tup(ref items) => {
            Item::Tuple(items.iter().map(|i| rexpr(scope, i)).collect())
        }

        ast::Expr::String(ref s) => Item::Leaf(LeafItem::String(s.clone())),


        ast::Expr::Func{ ref body, ref args } => {
            Item::Leaf(LeafItem::Func(Arc::new(FunctionDef::Code(Func{
                args: (**args).clone(),
                body: (**body).clone(),
                scope: scope.clone(),
            }))))
        }

        ast::Expr::Call(ref func, ref arg) => {
            resolve_call(scope, func, arg)
        }

        _ => Item::Leaf(LeafItem::Value(value(scope, e)))
    }
}

fn resolve_call(scope: &Scope, func: &ast::SpannedExpr, arg: &ast::SpannedExpr) -> Item {
    let func = rexpr(scope, func);
    let arg = rexpr(scope, arg);

    if let Item::Leaf(LeafItem::Func(func)) = func {
        func.apply(arg)
    } else {
        panic!("{:?} is not a function", func)
    }
}

fn zip_ast<T>(ast: &ast::SpannedExpr, tree: &Tree<T>, f: &mut impl FnMut(&ast::SpannedExpr, &Tree<T>) -> Result<(), &'static str>) -> Result<(), &'static str> {
    match (&ast.node, tree) {
        (ast::Expr::Tup(a), Tree::Tuple(t)) => {
            let mut a = a.iter();
            let mut t = t.iter();
            loop {
                match (a.next(), t.next()) {
                    (None, None) => break,
                    (Some(ai), Some(ti)) => zip_ast(ai, ti, f)?,
                    _ => return Err("mismatched tuples")
                }
            }
        }
        (_, t) => f(ast, t)?
    }
    Ok(())
}

/// Resolve expressions as used in the arguments of an `on` block, defining variables
pub fn on_expr_message(mut new_var: impl FnMut() -> VarId, scope: &mut Scope, msg_def: &ShapeMsg, exprs: &[ast::SpannedExpr]) -> (Vec<Expr>, Vec<ExprDn>) {
    assert_eq!(msg_def.params.len(), exprs.len());

    let mut dn = Vec::new();
    let mut up = Vec::new();

    for (param, expr) in msg_def.params.iter().zip(exprs) {
        let dir = param.direction;
        let push_up = &mut |i: Expr| up.push(i.down());
        let push_dn = &mut |i| dn.push(i);
        let push: &mut dyn FnMut(Expr);
        
        if param.direction.up {
            push = push_up;
        } else if param.direction.down {
            push = push_dn;
        } else {
            unreachable!()
        };

        zip_ast(expr, &param.item, &mut |a, e| {
            match (e, &a.node) {
                (&Item::Leaf(LeafItem::Value(ref v)), ast::Expr::Var(ref name)) => {
                    let ty = v.get_type();
                    let id = new_var();
                    scope.bind(name, Item::Leaf(LeafItem::Value(Expr::Variable(id, ty.clone(), dir))));
                    push(Expr::Variable(id, ty, dir));
                }
                (&Item::Leaf(LeafItem::Value(_)), _) => {
                    push(resolve(None, &mut |_| { panic!("Variable binding not allowed here") }, a));
                }
                (t @ &Item::Tuple(_), ast::Expr::Var(ref name)) => {
                    // Capture a tuple by recursively building a tuple Item containing each of the
                    // captured variables
                    let v = t.map_leaf(&mut |i| {
                        match i {
                            LeafItem::Value(Expr::Const(ref c)) => LeafItem::Value(Expr::Const(c.clone())),
                            LeafItem::Value(ref v) => {
                                let ty = v.get_type();
                                let id = new_var();
                                push(Expr::Variable(id, ty.clone(), dir));
                                LeafItem::Value(Expr::Variable(id, ty, dir))
                            }
                            _ => panic!("{:?} not allowed in shape", i)
                        }
                    });
                    scope.bind(name, v);
                }
                _ => return Err("invalid pattern")
            }
            Ok(())
        }).unwrap();
    }

    (dn, up)
}

#[derive(Clone, Debug)]
pub struct PatternError {
    msg: &'static str,
}

/// Destructures an item into left-hand-side binding, such as a function argument, returning
/// whether the pattern matched. No runtime evaluation is allowed.
pub fn lexpr(scope: &mut Scope, l: &ast::SpannedExpr, r: Item) -> Result<(), PatternError> {
    zip_ast(l, &r, &mut |l, r| {
        Ok(match l.node {
            ast::Expr::Ignore => {},
            ast::Expr::Var(ref name) => {
                scope.bind(name, r.clone())
            }
            _ => {
                let lv = &value(scope, l);
                if let Item::Leaf(LeafItem::Value(rv)) = &r {
                    debug!("{:?} == {:?} => {:?}", lv, rv, lv == rv);
                    if lv != rv {
                        Err("expected tuple, variable, or ignore")?
                    }
                }
            }
        })
    }).map_err(|msg| PatternError { msg })
}

/// Destructure an item into an expression, like lexpr, but allowing runtime pattern-matching
pub fn pattern_match(scope: &mut Scope, pat: &ast::SpannedExpr, r: &Item, checks: &mut Vec<(Expr, Expr)>) {
    zip_ast(pat, r, &mut |pat, r| {
        match (&pat.node, r) {
            (ast::Expr::Ignore, _) => (),

            (ast::Expr::Var(ref name), r) => {
                debug!("defined {} = {:?}", name, r);
                scope.bind(name, r.clone());
            }

            (_, Item::Leaf(LeafItem::Value(ref re))) => {
                let le = resolve(None, &mut |_| { panic!("Variable binding not allowed here")}, pat);
                checks.push((le, re.clone()));
            }

            (pat, r) => panic!("can't match {:?} with {:?}", pat, r)
        }
        Ok(())
    }).unwrap();
}
