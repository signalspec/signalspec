use std::sync::Arc;
use crate::{syntax::{ast, Value}, tree::Tree, core::Dir};
use super::{ConcatElem, Expr, Func, FunctionDef, Item, Scope, ShapeMsg, ExprDn, VarId, LeafItem };

pub fn value(scope: &Scope, e: &ast::Expr) -> Expr {
    match e {
        ast::Expr::Ignore(_) => Expr::Ignored,
        ast::Expr::Value(ref node) => Expr::Const(node.value.clone()),

        ast::Expr::Flip(ref node) => {
            Expr::Flip(
                Box::new(node.dn.as_ref().map_or(Expr::Ignored,  |dn| value(scope, dn))),
                Box::new(node.up.as_ref().map_or(Expr::Ignored,  |up| value(scope, up))),
            )
        }

        ast::Expr::Range(ref node) => {
            let min = value(scope, &node.lo);
            let max = value(scope, &node.hi);

            match (min, max) {
                (Expr::Const(Value::Number(l)), Expr::Const(Value::Number(h))) => Expr::Range(l, h),
                (Expr::Const(Value::Integer(l)), Expr::Const(Value::Integer(h))) => Expr::RangeInt(l, h),
                _ => panic!("Range expressions must be numeric constant")
            }
        }

        ast::Expr::Union(ref node) => {
            Expr::Union(node.items.iter().map(|i| value(scope, i)).collect())
        }

        ast::Expr::Choose(ref node) => {
            let head = value(scope, &node.e);

            if let Expr::Const(v) = head {
                let re = node.choices.iter().find(|&(ref le, _)| {
                    let l = value(scope, le);
                    l.eval_up(&mut |_, _, _| panic!("Variable not expected here"), v.clone())
                }).map(|x| &x.1).unwrap_or_else(|| {
                    panic!("No match for {} at {:?}", v, &node.span);
                });
                value(scope, re)
            } else {
                let pairs: Vec<(Value, Value)> = node.choices.iter().map(|&(ref le, ref re)| {
                    let l = value(scope, le);
                    let r = value(scope, re);

                    match (l, r) {
                        (Expr::Const(lv), Expr::Const(rv)) => (lv, rv),
                        _ => panic!("Choose expression arms must be constant, for now")
                    }
                }).collect();

                Expr::Choose(Box::new(head), pairs)
            }
        }

        ast::Expr::Concat(ref node) =>  {
            let elems = node.elems.iter().map(|&(slice_width, ref e)| {
                let expr = value(scope, e);
                match slice_width {
                    None => ConcatElem::Elem(expr),
                    Some(w) => ConcatElem::Slice(expr, w as usize)
                }
            }).collect();

            Expr::Concat(elems)
        }

        ast::Expr::Bin(ref node) => {
            use self::Expr::Const;
            let lhs = value(scope, &node.l);
            let rhs = value(scope, &node.r);
            let op = node.op;
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

        ast::Expr::Var(ref name) => {
            match scope.get(&name.name) {
                Some(Item::Leaf(LeafItem::Value(v))) => v,
                Some(..) => panic!("Variable {} is not a value expression", name.name),
                None => panic!("Undefined variable {}", name.name),
            }
        }

        ast::Expr::Call(ref node) => {
            match resolve_call(scope, &node.func, &node.arg) {
                Item::Leaf(LeafItem::Value(v)) => v,
                other => panic!("Expcted value item, but function evaluated to {:?}", other),
            }
        }

        ast::Expr::Tup(t) if t.items.len() == 1 => { value(scope, &t.items[0]) }

        ast::Expr::Tup(..) => panic!("Tuple not allowed here"),
        ast::Expr::String(..) => panic!("String not allowed here"),
        ast::Expr::Func{..} => panic!("Function not allowed here"),
        ast::Expr::Error(_) => panic!("Syntax error"),
    }
}

pub fn rexpr_tup(scope: &Scope, node: &ast::ExprTup) -> Item {
    let values: Vec<Item> = node.items.iter().map(|i| rexpr(scope, i)).collect();
    if values.len() == 1 {
        values.into_iter().next().unwrap()
    } else {
        Item::Tuple(values)
    }
}

/// Resolve an expression as used in an argument or right hand side of an assignment
pub fn rexpr(scope: &Scope, e: &ast::Expr) -> Item {
    match e {
        ast::Expr::Var(ref name) => {
            if let Some(s) = scope.get(&name.name) { s } else { panic!("Undefined variable `{}`", name.name); }
        }

        ast::Expr::Tup(ref node) => rexpr_tup(scope, node),

        ast::Expr::String(ref node) => Item::Leaf(LeafItem::String(node.value.clone())),

        ast::Expr::Func(ref node) => {
            Item::Leaf(LeafItem::Func(Arc::new(FunctionDef::Code(Func{
                args: (*node.args).clone(),
                body: (*node.body).clone(),
                scope: scope.clone(),
            }))))
        }

        ast::Expr::Call(ref node) => {
            resolve_call(scope, &node.func, &node.arg)
        }

        _ => Item::Leaf(LeafItem::Value(value(scope, e)))
    }
}

fn resolve_call(scope: &Scope, func: &ast::Expr, arg: &ast::ExprTup) -> Item {
    let func = rexpr(scope, func);
    let arg = rexpr_tup(scope, arg);

    if let Item::Leaf(LeafItem::Func(func)) = func {
        func.apply(arg)
    } else {
        panic!("{:?} is not a function", func)
    }
}

fn zip_ast<T>(ast: &ast::Expr, tree: &Tree<T>, f: &mut impl FnMut(&ast::Expr, &Tree<T>) -> Result<(), &'static str>) -> Result<(), &'static str> {
    match (&ast, tree) {
        (ast::Expr::Tup(a), Tree::Tuple(t)) => {
            let mut a = a.items.iter();
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
pub fn on_expr_message(mut new_var: impl FnMut() -> VarId, scope: &mut Scope, msg_def: &ShapeMsg, exprs: &[ast::Expr]) -> (Vec<Expr>, Vec<ExprDn>) {
    assert_eq!(msg_def.params.len(), exprs.len());

    let mut dn = Vec::new();
    let mut up = Vec::new();

    for (param, expr) in msg_def.params.iter().zip(exprs) {
        let push_up = &mut |i: Expr| up.push(i.down().unwrap());
        let push_dn = &mut |i| dn.push(i);
        let push: &mut dyn FnMut(Expr) = match param.direction {
            Dir::Up => push_up,
            Dir::Dn => push_dn,
        };

        zip_ast(expr, &param.item, &mut |a, e| {
            match (e, &a) {
                (&Item::Leaf(LeafItem::Value(ref v)), ast::Expr::Var(ref name)) => {
                    let ty = v.get_type();
                    let id = new_var();
                    scope.bind(&name.name, Item::Leaf(LeafItem::Value(Expr::Variable(id, ty.clone(), param.direction))));
                    push(Expr::Variable(id, ty, param.direction.flip()));
                }
                (&Item::Leaf(LeafItem::Value(_)), _) => {
                    push(value(scope, a));
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
                                push(Expr::Variable(id, ty.clone(), param.direction.flip()));
                                LeafItem::Value(Expr::Variable(id, ty, param.direction))
                            }
                            _ => panic!("{:?} not allowed in shape", i)
                        }
                    });
                    scope.bind(&name.name, v);
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
/// whether the pattern matched. If `checks` is not None, runtime checks are enabled
/// and collected.
pub fn lexpr(scope: &mut Scope, pat: &ast::Expr, r: &Item, mut checks: Option<&mut Vec<(Expr, Expr)>>) -> Result<(), PatternError> {
    zip_ast(pat, r, &mut move |pat, r| {
        let pat = match pat {
            ast::Expr::Tup(t) if t.items.len() == 1 => &t.items[0],
            pat => pat,
        };
        match pat {
            ast::Expr::Ignore(_) => Ok(()),

            ast::Expr::Var(ref name) => {
                debug!("defined {} = {:?}", name.name, r);
                scope.bind(&name.name, r.clone());
                Ok(())
            }

            pat => {
                let lv = value(scope, pat); //TODO: disallow variable references?

                if let Item::Leaf(LeafItem::Value(ref rv)) = r {
                    if &lv == rv {
                        Ok(())
                    } else if let Some(checks) = &mut checks {
                        // TODO: check overlap
                        checks.push((lv, rv.clone()));
                        Ok(())
                    } else {
                        Err("expected tuple, variable, or ignore")
                    }
                } else {
                    Err("item cannot be matched")
                }
            }
        }
    }).map_err(|msg| PatternError { msg })
}
