use std::sync::Arc;
use num_complex::Complex;

use crate::{syntax::{ast, Value}, tree::Tree, diagnostic::{DiagnosticHandler, Diagnostic, Span}};
use super::{ConcatElem, Expr, Func, FunctionDef, Item, Scope, LeafItem };

pub fn value(ctx: &dyn DiagnosticHandler, scope: &Scope, e: &ast::Expr) -> Expr {
    match rexpr(ctx, scope, e) {
        Item::Leaf(LeafItem::Value(v)) => v,
        _ => panic!("Expected a value"),
    }
}

pub fn rexpr_tup(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprTup) -> Item {
    let values: Vec<Item> = node.items.iter().map(|i| rexpr(ctx, scope, i)).collect();
    if values.len() == 1 {
        values.into_iter().next().unwrap()
    } else {
        Item::Tuple(values)
    }
}

/// Resolve an expression as used in an argument or right hand side of an assignment
pub fn rexpr(ctx: &dyn DiagnosticHandler, scope: &Scope, e: &ast::Expr) -> Item {
    match e {
        ast::Expr::Var(ref name) => {
            if let Some(s) = scope.get(&name.name) { s } else {
                ctx.report(Diagnostic::UndefinedVariable {
                    span: Span::new(&scope.file, name.span),
                    name: name.name.clone()
                });
                Item::Leaf(LeafItem::Invalid)
            }
        }

        ast::Expr::Tup(ref node) => rexpr_tup(ctx, scope, node),

        ast::Expr::String(ref node) => Item::Leaf(LeafItem::String(node.value.clone())),

        ast::Expr::Func(ref node) => {
            Item::Leaf(LeafItem::Func(Arc::new(FunctionDef::Code(Func{
                args: (*node.args).clone(),
                body: (*node.body).clone(),
                scope: scope.clone(),
            }))))
        }

        ast::Expr::Call(ref node) => {
            let func = rexpr(ctx, scope, &node.func);
            let arg = rexpr_tup(ctx, scope, &node.arg);

            if let Item::Leaf(LeafItem::Func(func)) = func {
                func.apply(ctx, arg)
            } else {
                panic!("{:?} is not a function", func)
            }
        }

        ast::Expr::Ignore(_) => Item::Leaf(LeafItem::Value(Expr::Ignored)),
        ast::Expr::Value(ref node) => Item::Leaf(LeafItem::Value(Expr::Const(node.value.clone()))),

        ast::Expr::Flip(ref node) => {
            Item::Leaf(LeafItem::Value(Expr::Flip(
                Box::new(node.dn.as_ref().map_or(Expr::Ignored,  |dn| value(ctx, scope, dn))),
                Box::new(node.up.as_ref().map_or(Expr::Ignored,  |up| value(ctx, scope, up))),
            )))
        }

        ast::Expr::Range(ref node) => {
            let min = value(ctx, scope, &node.lo);
            let max = value(ctx, scope, &node.hi);

            Item::Leaf(LeafItem::Value(match (min, max) {
                (Expr::Const(Value::Number(l)), Expr::Const(Value::Number(h))) => Expr::Range(l, h),
                _ => panic!("Range expressions must be numeric constant")
            }))
        }

        ast::Expr::Union(ref node) => {
            Item::Leaf(LeafItem::Value(Expr::Union(node.items.iter().map(|i| value(ctx, scope, i)).collect())))
        }

        ast::Expr::Choose(ref node) => {
            let head = value(ctx, scope, &node.e);

            if let Expr::Const(v) = head {
                let re = node.choices.iter().find(|&(ref le, _)| {
                    let l = value(ctx, scope, le).eval_const();
                    l == v
                }).map(|x| &x.1).unwrap_or_else(|| {
                    panic!("No match for {} at {:?}", v, &node.span);
                });
                Item::Leaf(LeafItem::Value(value(ctx, scope, re)))
            } else {
                let pairs: Vec<(Value, Value)> = node.choices.iter().map(|&(ref le, ref re)| {
                    let l = value(ctx, scope, le);
                    let r = value(ctx, scope, re);

                    match (l, r) {
                        (Expr::Const(lv), Expr::Const(rv)) => (lv, rv),
                        _ => panic!("Choose expression arms must be constant, for now")
                    }
                }).collect();

                Item::Leaf(LeafItem::Value(Expr::Choose(Box::new(head), pairs)))
            }
        }

        ast::Expr::Concat(ref node) =>  {
            let elems = node.elems.iter().map(|&(slice_width, ref e)| {
                let expr = value(ctx, scope, e);
                match slice_width {
                    None => ConcatElem::Elem(expr),
                    Some(w) => ConcatElem::Slice(expr, w)
                }
            }).collect();

            Item::Leaf(LeafItem::Value(Expr::Concat(elems)))
        }

        ast::Expr::Bin(ref node) => {
            use self::Expr::Const;
            let lhs = value(ctx, scope, &node.l);
            let rhs = value(ctx, scope, &node.r);
            let op = node.op;
            Item::Leaf(LeafItem::Value( match (lhs, rhs) {
                (Const(Value::Number(a)),  Const(Value::Number(b))) => Const(Value::Number(op.eval(a, b))),
                (Const(Value::Complex(a)), Const(Value::Complex(b))) => Const(Value::Complex(op.eval(a, b))),
                (Const(Value::Complex(a)), Const(Value::Number(b))) => Const(Value::Complex(op.eval(a, Complex::from(b)))),
                (Const(Value::Number(a)),  Const(Value::Complex(b))) => Const(Value::Complex(op.eval(Complex::from(a), b))),
                (l, Const(b)) => Expr::BinaryConst(Box::new(l), op, b),
                (Const(a), r) => Expr::BinaryConst(Box::new(r), op.swap(), a),
                _ => panic!("One side of a binary operation must be constant")
            }))
        }

        ast::Expr::Error(_) => Item::Leaf(LeafItem::Invalid),
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

/// Resolve an expression in an `on` block, defining variables
pub fn bind_tree_fields<T, S>(ctx: &dyn DiagnosticHandler, expr: &ast::Expr, t: &Tree<T>, scope: &mut Scope, mut s: S, mut variable: impl FnMut(&mut S, &T) -> Expr, mut pattern: impl FnMut(&mut S, Expr)) {
    zip_ast(expr, t, &mut |a, e| {
        match (e, &a) {
            (Tree::Leaf(t), ast::Expr::Var(ref name)) => {
                scope.bind(&name.name, Item::Leaf(LeafItem::Value(variable(&mut s, t))));
            }
            (Tree::Leaf(_), a) => {
                pattern(&mut s, value(ctx, scope, a));
            }
            (t @ &Tree::Tuple(_), ast::Expr::Var(ref name)) => {
                // Capture a tuple by recursively building a tuple Item containing each of the
                // captured variables
                let v = t.map_leaf(&mut |t| {
                    LeafItem::Value(variable(&mut s, t))
                });
                scope.bind(&name.name, v);
            }
            _ => return Err("invalid pattern")
        }
        Ok(())
    }).unwrap();
}

/// Destructures an item into left-hand-side binding, such as a `let` or function argument
pub fn lexpr(ctx: &dyn DiagnosticHandler, scope: &mut Scope, pat: &ast::Expr, r: &Item) -> Result<(), &'static str> {
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
                let lv = value(ctx, scope, pat); //TODO: disallow variable references?

                if let Item::Leaf(LeafItem::Value(ref rv)) = r {
                    if &lv == rv {
                        Ok(())
                    } else {
                        Err("expected tuple, variable, or ignore")
                    }
                } else {
                    Err("item cannot be matched")
                }
            }
        }
    })
}
