use std::sync::Arc;

use crate::{syntax::{ast::{self, AstNode, BinOp}, Value, Number}, tree::Tree, diagnostic::{DiagnosticHandler, Diagnostic, Span, ErrorReported}, Type, core::ConcatElem};
use super::{Expr, Func, FunctionDef, Item, Scope, LeafItem, expr::{ExprKind, eval_choose, eval_binary} };

pub fn value(ctx: &dyn DiagnosticHandler, scope: &Scope, e: &ast::Expr) -> Result<Expr, ErrorReported> {
    match rexpr(ctx, scope, e) {
        Item::Leaf(LeafItem::Value(v)) => Ok(v),
        Item::Leaf(LeafItem::Invalid(r)) => Err(r),
        other => Err(ctx.report(
            Diagnostic::ExpectedValue {
                span: Span::new(&scope.file, e.span()),
                found: other.to_string()
            }
        )),
    }
}

pub fn constant(ctx: &dyn DiagnosticHandler, scope: &Scope, e: &ast::Expr) -> Result<Value, ErrorReported> {
    match rexpr(ctx, scope, e) {
        Item::Leaf(LeafItem::Value(Expr::Const(c))) => Ok(c),
        Item::Leaf(LeafItem::Invalid(r)) => Err(r),
        other => Err(ctx.report(
            Diagnostic::ExpectedConst {
                span: Span::new(&scope.file, e.span()),
                found: other.to_string()
            }
        )),
    }
}

pub fn constant_number(ctx: &dyn DiagnosticHandler, scope: &Scope, e: &ast::Expr) -> Result<Number, ErrorReported> {
    match rexpr(ctx, scope, e) {
        Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(v)))) => Ok(v),
        Item::Leaf(LeafItem::Invalid(r)) => Err(r),
        other => Err(ctx.report(
            Diagnostic::ExpectedConstNumber {
                span: Span::new(&scope.file, e.span()),
                found: other.to_string()
            }
        )),
    }
}

/// Like iter.collect::<Result<..>>() but doesn't short-circuit so errors from
/// all subtrees are reported
pub fn collect_or_err<T, C: FromIterator<T>>(iter: impl Iterator<Item=Result<T, ErrorReported>>) -> Result<C, ErrorReported> {
    let mut error = None;
    let result = C::from_iter(iter.flat_map(|i| {
        match i {
            Ok(elem) => Some(elem),
            Err(err) => { error = Some(err); None }
        }
    }));
    if let Some(e) = error { Err(e) } else { Ok(result) }
}

pub fn rexpr_tup(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprTup) -> Item {
    let values: Vec<Item> = node.items.iter().map(|i| rexpr(ctx, scope, i)).collect();
    if values.len() == 1 {
        values.into_iter().next().unwrap()
    } else {
        Item::Tuple(values)
    }
}

macro_rules! try_item {
    ($e:expr) => {
        match $e {
            Ok(e) => e,
            Err(r) => return LeafItem::Invalid(r).into()
        }
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
                }).into()
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
            resolve_function_call(ctx, || Span::new(&scope.file, node.span), func, arg)
        }

        ast::Expr::Value(ref node) => Expr::Const(node.value.clone()).into(),
        ast::Expr::Ignore(_) => Expr::ignored().into(),

        ast::Expr::Typed(ref node) => resolve_expr_typed(ctx, scope, node),
        ast::Expr::Flip(ref node) => resolve_expr_flip(ctx, scope, node),
        ast::Expr::Range(ref node) => resolve_expr_range(ctx, scope, node),
        ast::Expr::Union(ref node) => resolve_expr_union(ctx, scope, node),
        ast::Expr::Choose(ref node) => resolve_expr_choose(ctx, scope, node),
        ast::Expr::Concat(ref node) => resolve_expr_concat(ctx, scope, node),
        ast::Expr::Bin(ref node) => resolve_expr_binary(ctx, scope, node),

        ast::Expr::Error(e) => Item::Leaf(LeafItem::Invalid(ErrorReported::from_ast(e))),
    }
}

fn resolve_expr_flip(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprFlip) -> Item {
    let dn = node.dn.as_ref().map_or(Ok(Expr::ignored()),  |dn| value(ctx, scope, dn));
    let up = node.up.as_ref().map_or(Ok(Expr::ignored()),  |up| value(ctx, scope, up));

    let dn = try_item!(dn);
    let up = try_item!(up);

    let ty = Type::union(dn.get_type(), up.get_type())
        .map_err(|err| err.report_at(ctx, Span::new(&scope.file, node.span)));
    let ty = try_item!(ty);

    Expr::Expr(ty, ExprKind::Flip(Box::new(dn.inner()), Box::new(up.inner()))).into()
}

fn resolve_expr_range(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprRange) -> Item {
    let min = constant_number(ctx, scope, &node.lo);
    let max = constant_number(ctx, scope, &node.hi);

    let min = try_item!(min);
    let max = try_item!(max);

    let ty = Type::Number(min, max);
    Expr::Expr(ty, ExprKind::Range(min, max)).into()
}

fn resolve_expr_typed(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprTyped) -> Item {
    let expr = value(ctx, scope, &node.expr);
    let bound_expr = value(ctx, scope, &node.ty);

    let expr = try_item!(expr);
    let bound = try_item!(bound_expr).get_type();

    let span = || Span::new(&scope.file, node.span);

    match expr {
        Expr::Const(c) => {
            if bound.test(&c) {
                Expr::Const(c).into()
            } else {
                ctx.report(Diagnostic::TypeConstraint { span: span(), found: c.get_type(), bound }).into()
            }
        },
        Expr::Expr(t, e) => {
            match t.bound(bound) {
                Ok(new_t) => Expr::Expr(new_t, e).into(),
                Err((t, bound)) =>
                    ctx.report(Diagnostic::TypeConstraint { span: span(), found: t, bound }).into(),
            }
        },
    }
}

fn resolve_expr_union(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprUnion) -> Item {
    let opts: Vec<_> = try_item!(
        collect_or_err(node.items.iter().map(|i| value(ctx, scope, i)))
    );

    let ty = Type::union_iter(opts.iter().map(|x| x.get_type()));
    let ty = try_item!(ty.map_err(|err| err.report_at(ctx, Span::new(&scope.file, node.span))));

    Expr::Expr(ty, ExprKind::Union(opts.into_iter().map(|x| x.inner()).collect())).into()
}

fn resolve_expr_choose(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprChoose) -> Item {
    let e = value(ctx, scope, &node.e);
    let pairs = collect_or_err(
        node.choices.iter().map(|&(ref le, ref re)| {
            let l = constant(ctx, scope, le);
            let r = constant(ctx, scope, re);
            Ok((l?, r?))
        }
    ));

    let e = try_item!(e);
    let pairs: Vec<_> = try_item!(pairs);

    let span = || Span::new(&scope.file, node.span);

    let lt = Type::union_iter(pairs.iter().map(|x| x.0.get_type()));
    let rt = Type::union_iter(pairs.iter().map(|x| x.1.get_type()));

    let lt = try_item!(lt.map_err(|err| err.report_at(ctx, span())));
    let rt = try_item!(rt.map_err(|err| err.report_at(ctx, span())));

    match e {
        Expr::Const(c) => {
            let Some(v) = eval_choose(&c, &pairs) else {
                return ctx.report(crate::Diagnostic::ChooseNotCovered { span: span(), found: c.get_type() }).into();
            };

            Expr::Const(v)
        }
        Expr::Expr(ty, e) => {
            //TODO: doesn't check coverage of full number range
            if ty != lt {
                return ctx.report(crate::Diagnostic::ChooseNotCovered { span: span(), found: ty }).into();
            }

            Expr::Expr(rt, ExprKind::Choose(Box::new(e), pairs))
        }
    }.into()
}

fn resolve_expr_concat(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprConcat) -> Item {
    enum ConcatBuilder {
        Const(Vec<Value>),
        Expr(Vec<ConcatElem<ExprKind>>),
    }

    impl ConcatBuilder {
        fn push_const(&mut self, c: Value) {
            use ConcatBuilder::*;
            match self {
                Const(v) => v.push(c),
                Expr(v) => v.push(ConcatElem::Elem(ExprKind::Const(c))),
            }
        }

        fn extend(&mut self, i: impl Iterator<Item = ConcatElem<ExprKind>>) {
            use ConcatBuilder::*;
            match self {
                Const(v) => {
                    *self = Expr(
                        v.drain(..)
                            .map(|c| ConcatElem::Elem(ExprKind::Const(c)))
                            .chain(i)
                            .collect()
                    );
                }
                Expr(v) => v.extend(i),
            }
        }

        fn push(&mut self, e: ConcatElem<ExprKind>) {
            self.extend(std::iter::once(e))
        }
    }

    let mut elem_ty = Type::Ignored;
    let mut len: u32 = 0;
    let mut elems = ConcatBuilder::Const(Vec::new());

    let it = node.elems.iter().map(|&(width, ref e)| {
        let elem = value(ctx, scope, e)?;
        let span = || Span::new(&scope.file, node.span);

        if let Some(w) = width {
            match elem {
                Expr::Const(Value::Vector(vs)) if w as usize == vs.len() => {
                    for c in vs {
                        elem_ty.union_with(c.get_type()).map_err(|err| err.report_at(ctx, span()))?;
                        elems.push_const(c);
                        len += 1;
                    }
                }

                Expr::Expr(Type::Vector(w1, ty), e) if w == w1 => {
                    len += w;
                    elem_ty.union_with(*ty).map_err(|err| err.report_at(ctx, span()))?;

                    match e {
                        // Flatten nested slice
                        ExprKind::Concat(es) => {
                            elems.extend(es.into_iter());
                        }
                        e => {
                            elems.push(ConcatElem::Slice(e, w))
                        }
                    }
                }

                Expr::Expr(Type::Ignored, e) => {
                    len += w;
                    elems.push(ConcatElem::Slice(e, w));
                }

                other => Err(ctx.report(Diagnostic::ConcatSpreadInvalidType {
                    span: span(),
                    expected_width: w,
                    found: other.get_type(),
                }))?
            }
            Ok(())
        } else {
            match elem {
                Expr::Const(c) => {
                    elem_ty.union_with(c.get_type()).map_err(|err| err.report_at(ctx, span()))?;
                    len += 1;
                    elems.push_const(c);
                }
                Expr::Expr(ty, e) => {
                    elem_ty.union_with(ty).map_err(|err| err.report_at(ctx, span()))?;
                    len += 1;
                    elems.push(ConcatElem::Elem(e))
                }
            }
            Ok(())
        }
    });
    try_item!(collect_or_err(it));

    match elems {
        ConcatBuilder::Const(v) => Expr::Const(Value::Vector(v)),
        ConcatBuilder::Expr(e) => Expr::Expr(
            Type::Vector(len, Box::new(elem_ty)),
            ExprKind::Concat(e)
        ),
    }.into()
}

fn resolve_expr_binary(ctx: &dyn DiagnosticHandler, scope: &Scope, node: &ast::ExprBin) -> Item {
    let lhs = value(ctx, scope, &node.l);
    let rhs = value(ctx, scope, &node.r);

    let lhs = try_item!(lhs);
    let rhs = try_item!(rhs);

    let span = || Span::new(&scope.file, node.span);

    // Swap constant operand to the right, or constant-fold and return
    let (expr, expr_ty, expr_span, op, val, val_span) = match (lhs, rhs) {
        (Expr::Const(l), Expr::Const(r)) => {
            if let Some(v) = eval_binary(l.clone(), node.op, r.clone()) {
                return Expr::Const(v).into()
            } else {
                return ctx.report(
                Diagnostic::BinaryInvalidType {
                    span1: Span::new(&scope.file, node.l.span()),
                    ty1: l.get_type(),
                    span2: Span::new(&scope.file, node.r.span()),
                    ty2: r.get_type(),
                 }
                ).into()
            }
        }
        (Expr::Expr(..), Expr::Expr(..)) => {
            return ctx.report(Diagnostic::BinaryOneSideMustBeConst { span: span() }).into();
        }
        (Expr::Expr(l_ty, l), Expr::Const(r)) => (l, l_ty, node.l.span(), node.op, r, node.r.span()),
        (Expr::Const(l), Expr::Expr(r_ty, r)) => (r, r_ty, node.r.span(), node.op.swap(), l, node.l.span()),
    };

    let ty = match (expr_ty, &val) {
        (Type::Number(min, max), Value::Number(c)) => {
            assert!(min <= max);
            let (a, b) = (op.eval(min, c), op.eval(max, c));
            if op == BinOp::SubSwap || op == BinOp::DivSwap {
                assert!(b <= a);
                Type::Number(b, a)
            } else {
                assert!(a <= b);
                Type::Number(a, b)
            }
        }
        (Type::Complex, Value::Number(..)) => Type::Complex,
        (Type::Number(..), Value::Complex(..)) => Type::Complex,
        (expr_ty, _) => return ctx.report(
            Diagnostic::BinaryInvalidType {
                span1: Span::new(&scope.file, expr_span),
                ty1: expr_ty,
                span2: Span::new(&scope.file, val_span),
                ty2: val.get_type(),
             }
        ).into()
    };
    
    Expr::Expr(ty, ExprKind::BinaryConst(Box::new(expr), op, val)).into()
}

fn resolve_function_call(ctx: &dyn DiagnosticHandler, call_site_span: impl FnOnce() -> Span, func: Item, arg: Item) -> Item {
    match func {
        Item::Leaf(LeafItem::Func(func_def)) => {
            match *func_def {
                FunctionDef::Code(ref func) => {
                    let mut inner_scope = func.scope.child();
                    if let Err(msg) = lexpr(ctx, &mut inner_scope, &func.args, &arg) {
                        return ctx.report(Diagnostic::FunctionArgumentMismatch {
                            span: call_site_span(),
                            def: Span::new(&func.scope.file, func.args.span()),
                            msg: msg.into(),
                        }).into()
                    }
                    rexpr(ctx, &inner_scope, &func.body)
                }
                FunctionDef::Primitive(primitive) => {
                    primitive(arg).unwrap_or_else(|msg| {
                        ctx.report(Diagnostic::ErrorInPrimitiveFunction {
                            span: call_site_span(),
                            msg: msg.into(),
                        }).into()
                    })
                },
            }
        }
        e @ Item::Leaf(LeafItem::Invalid(_)) => e,
        _ => ctx.report(Diagnostic::NotAFunction {
            span: call_site_span(),
            found: format!("{}", func)
        }).into()
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
pub fn bind_tree_fields<T, S>(
    ctx: &dyn DiagnosticHandler,
    expr: &ast::Expr, t: &Tree<T>,
    scope: &mut Scope,
    mut s: S,
    mut variable: impl FnMut(&mut S, &T, &ast::Identifier) -> Expr,
    mut pattern: impl FnMut(&mut S, Expr)
) {
    zip_ast(expr, t, &mut |a, e| {
        match (e, &a) {
            (Tree::Leaf(t), ast::Expr::Var(ref name)) => {
                scope.bind(&name.name, Item::Leaf(LeafItem::Value(variable(&mut s, t, name))));
            }
            (Tree::Leaf(_), a) => {
                pattern(&mut s, value(ctx, scope, a).unwrap());
            }
            (t @ &Tree::Tuple(_), ast::Expr::Var(ref name)) => {
                // Capture a tuple by recursively building a tuple Item containing each of the
                // captured variables
                let v = t.map_leaf(&mut |t| {
                    LeafItem::Value(variable(&mut s, t, name))
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

            ast::Expr::Typed(ref node) => {
                let Ok(ty) = value(ctx, scope, &node.ty) else {
                    return Err("failed to evaluate type constraint")
                };
                let ty = ty.get_type();

                if let Item::Leaf(LeafItem::Value(ref rv)) = r {
                    if rv.get_type().is_subtype(&ty) {
                        lexpr(ctx, scope, &node.expr, r)
                    } else {
                        Err("type mismatch")
                    }
                } else {
                    Err("expected a single value to match type pattern")
                }
            }

            ast::Expr::Value(lv) => {
                if let Item::Leaf(LeafItem::Value(Expr::Const(ref rv))) = r {
                    (lv.value == *rv).then_some(()).ok_or("literal value mismatch")
                } else {
                    Err("expected a constant to match literal pattern")
                }
            }

            _ => Err("invalid pattern")
        }
    })
}
