use std::{sync::Arc, fmt::Display};

use crate::{
    core::{
        expr::{eval_binary, eval_choose, ExprKind},
        resolve::action::ValueSinkId,
        ConcatElem, Expr, ExprDn, Func, FunctionDef, Item, LeafItem, Predicate, Scope, data::{NumberType, NumberTypeError},
    },
    diagnostic::{Diagnostic, DiagnosticHandler, ErrorReported, Span},
    syntax::{
        ast::{self, AstNode, BinOp},
        Number,
    },
    tree::Tree,
    FileSpan, SourceFile, Type, Value,
};

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

pub trait TryFromConstant: TryFrom<Value> {
    /// Error message, used as "expected constant {x}"
    const EXPECTED_MSG: &'static str;
}

impl TryFromConstant for Value {
    const EXPECTED_MSG: &'static str = "value";
}

impl TryFromConstant for Number {
    const EXPECTED_MSG: &'static str = "number";
}

pub fn constant<'a, T: TryFromConstant>(ctx: &dyn DiagnosticHandler, scope: &Scope, ast: &ast::Expr) -> Result<T, ErrorReported> {
    let item = rexpr(ctx, scope, ast);
    let found = item.to_string();

    match item {
        Item::Leaf(LeafItem::Value(Expr::Const(v))) => {
            if let Ok(r) = T::try_from(v) {
                return Ok(r);
            }
        }
        Item::Leaf(LeafItem::Invalid(r)) => {
            return Err(r);
        }
        _ => {}
    }

    Err(ctx.report(
        Diagnostic::ExpectedConst {
            span: Span::new(&scope.file, ast.span()),
            found,
            expected: T::EXPECTED_MSG.to_string(),
        }
    ))
}

pub fn type_tree(ctx: &dyn DiagnosticHandler, scope: &Scope, e: &ast::Expr) -> Result<Tree<Type>, ErrorReported> {
    match rexpr(ctx, scope, e) {
        Item::Leaf(LeafItem::Invalid(r)) => Err(r),
        i => i.as_type_tree().ok_or_else(|| {
            ctx.report(Diagnostic::ExpectedType {
                span: Span::new(&scope.file, e.span()),
                found: i.to_string()
            })
        })
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
                file: scope.file.clone(),
                names: scope.names.clone(),
            }))))
        }

        ast::Expr::Call(ref node) => {
            let func = rexpr(ctx, scope, &node.func);
            let arg = rexpr_tup(ctx, scope, &node.arg);
            resolve_function_call(ctx, || Span::new(&scope.file, node.span), func, arg)
        }

        ast::Expr::Value(ref node) => Expr::Const(Value::from_literal(&node.value)).into(),
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
    let min = constant::<Number>(ctx, scope, &node.lo);
    let max = constant::<Number>(ctx, scope, &node.hi);
    let step = node.step.as_ref().map(|s| constant::<Number>(ctx, scope, s)).transpose();

    let min = try_item!(min);
    let max = try_item!(max);
    let step = try_item!(step).unwrap_or(Number::new(1, 1));

    let nt = match NumberType::from_scaled(min, max, step) {
        Ok(t) => t,
        Err(NumberTypeError::BoundsNotMultipleOfStep) => return ctx.report(
            Diagnostic::RangeNotMultipleOfStep {
                min, min_span: Span::new(&scope.file, node.lo.span()),
                max, max_span: Span::new(&scope.file, node.hi.span()),
                step
            }
        ).into(),
        Err(NumberTypeError::Order) => return ctx.report(
            Diagnostic::RangeOrder {
                min, min_span: Span::new(&scope.file, node.lo.span()),
                max, max_span: Span::new(&scope.file, node.hi.span()),
            }).into(),
        Err(NumberTypeError::StepIsZero) => return ctx.report(
            Diagnostic::RangeStepZero {
                step, step_span: Span::new(&scope.file, node.step.as_ref().unwrap().span()),
            }).into(),
    };

    Expr::Expr(Type::Number(nt), ExprKind::Range(min, max)).into()
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
            let l = constant::<Value>(ctx, scope, le);
            let r = constant::<Value>(ctx, scope, re);
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
            if !(ty.is_subtype(&lt) && lt.is_subtype(&ty)) {
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

                other => Err(ctx.report(Diagnostic::ExpectedVector {
                    span: span(),
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
        (Type::Number(nt), Value::Number(c)) => {
            let check_scale = |n: Option<NumberType>| {
                n.ok_or_else(|| {
                    ctx.report(Diagnostic::OperandNotMultipleOfScale {
                        const_span: Span::new(&scope.file, val_span),
                        const_val: *c,
                        var_span: Span::new(&scope.file, expr_span),
                        var_scale: nt.scale(),
                    })
                })
            };

            Type::Number(match op {
                BinOp::Add => try_item!(check_scale(nt.add(*c))),
                BinOp::Sub => try_item!(check_scale(nt.add(-c))),
                BinOp::SubSwap => try_item!(check_scale(nt.mul(Number::new(-1,1)).add(*c))),

                BinOp::Mul => nt.mul(*c),
                BinOp::Div => nt.mul(c.recip()),
                BinOp::DivSwap => return ctx.report(Diagnostic::DivisionMustBeConst {
                    span: Span::new(&scope.file, expr_span),
                }).into(),
            })
        }
        (Type::NumberSet(ls), Value::Number(c)) => {
            Type::NumberSet(ls.into_iter().map(|l| op.eval(l, c)).collect())
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
                    let mut inner_scope = Scope { file: func.file.clone(), names: func.names.clone() };
                    if let Err(_) = lexpr(ctx, &mut inner_scope, &func.args, &arg) {
                        // TODO: should be chained to inner error
                        return ctx.report(Diagnostic::FunctionArgumentMismatch {
                            span: call_site_span(),
                            def: Span::new(&func.file, func.args.span()),
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

pub fn zip_ast<T: Display>(
    ctx: &dyn DiagnosticHandler,
    file: &Arc<SourceFile>,
    ast: &ast::Expr,
    tree: &Tree<T>,
    f: &mut impl FnMut(&ast::Expr, &Tree<T>) -> Result<(), ErrorReported>
) -> Result<(), ErrorReported> {
    match (&ast, tree) {
        (ast::Expr::Tup(tup_ast), Tree::Tuple(t)) => {
            let mut res = Ok(());
            let mut a = tup_ast.items.iter();
            let mut t = t.iter();
            loop {
                match (a.next(), t.next()) {
                    (None, None) => break,
                    (Some(ai), Some(ti)) => {
                        res = zip_ast(ctx,  file, ai, ti, f).and(res);
                    }
                    (Some(ai), None) => {
                        let (n, span) = a.fold((1, ai.span()), |(n, sp), e| (n + 1, sp.to(e.span())));
                        return Err(ctx.report(Diagnostic::TupleTooFewPositional { span: Span::new(file, span), n }));
                    }
                    (None, Some(_)) => {
                        let span = tup_ast.close.as_ref().map(|a| a.span).unwrap_or(tup_ast.span);
                        let n = t.count() + 1;
                        return Err(ctx.report(Diagnostic::TupleTooManyPositional { span: Span::new(file, span), n }));
                    }
                }
            }
            res
        }
        (ast::Expr::Tup(tup_ast), t) if tup_ast.items.len() != 1 => {
            Err(ctx.report(Diagnostic::ExpectedTuple {
                span: Span::new(file, tup_ast.span),
                found: t.to_string()
            }))
        }
        (_, t) => f(ast, t)
    }
}

/// Pattern matching for `alt` or `on` in the down direction.
/// 
/// Make a predicate for matching `pat`, and bind variables from `pat` in `scope`
/// with values from `rhs`. `rhs` is only used for binding variables and is not
/// tested against the predicates here. It's up to the caller to test it with the
// returned predicate at runtime before entering the scope.
pub fn lvalue_dn(
    ctx: &dyn DiagnosticHandler,
    scope: &mut Scope,
    pat: &ast::Expr,
    rhs: Expr
) -> Result<Predicate, ErrorReported> {
    match pat {
        ast::Expr::Var(ref name) => {
            scope.bind(&name.name, Item::Leaf(LeafItem::Value(rhs)));
            Ok(Predicate::Any)
        }

        ast::Expr::Ignore(_) => Ok(Predicate::Any),

        ast::Expr::Value(lit) => {
            let val = Value::from_literal(&lit.value);

            let r_ty = rhs.get_type();
            if !r_ty.test(&val) {
                ctx.report(Diagnostic::TypeConstraint {
                    span: Span::new(&scope.file, lit.span),
                    found: val.get_type(),
                    bound: r_ty
                });
            }

            // from_literal Value is always a valid predicate.
            Ok(Predicate::from_value(&val).unwrap())
        }

        ast::Expr::Concat(node) => {
            let pat_w: u32 = node.elems.iter().map(|&(w, _)| w.unwrap_or(1)).sum();

            let elem_ty = match rhs.get_type() {
                Type::Vector(ty_w, elem_ty) if pat_w == ty_w => *elem_ty,
                expected_ty => return Err(ctx.report(Diagnostic::PatternExpectedVector {
                    span: Span::new(&scope.file, node.span),
                    found_width: pat_w,
                    expected: expected_ty,
                }))
            };

            // Can't fail:
            // * In `on`, this is always a `VarDn`.
            // * In `alt`, we've already checked.
            let dn = rhs.down().unwrap();

            let mut i = 0;
            let mut parts = Vec::new();
            let mut err = None;

            for (width, e) in &node.elems {
                if let Some(width) = *width {
                    let ri = Expr::Expr(
                        Type::Vector(width, Box::new(elem_ty.clone())),
                        ExprKind::VarDn(ExprDn::Slice(Box::new(dn.clone()), i, i+width))
                    );

                    // Flatten nested vector predicates
                    match lvalue_dn(ctx, scope, e, ri) {
                        Ok(Predicate::Vector(inner)) => parts.extend(inner),
                        Ok(p) => parts.push(ConcatElem::Slice(p, width)),
                        Err(r) => { parts.push(ConcatElem::Slice(Predicate::Any, width)); err = Some(r) }
                    }

                    i += width;
                } else {
                    let ri = Expr::Expr(
                        elem_ty.clone(),
                        ExprKind::VarDn(ExprDn::Index(Box::new(dn.clone()), i))
                    );

                    match lvalue_dn(ctx, scope, e, ri) {
                        Ok(p) => parts.push(ConcatElem::Elem(p)),
                        Err(r) => { parts.push(ConcatElem::Elem(Predicate::Any)); err = Some(r) }
                    }

                    i += 1;
                }
            }

            if let Some(r) = err { Err(r) } else {Ok(Predicate::Vector(parts))  }
        }

        pat => Err(ctx.report(Diagnostic::NotAllowedInPattern {
            span: Span::new(&scope.file, pat.span())
        }))
    }
}

pub enum LValueSrc {
   Val(ExprDn),
   Var(ValueSinkId, FileSpan),
   Concat(Vec<ConcatElem<LValueSrc>>),
}

/// Pattern matching for `alt` or `on` in the up direction.
/// 
/// Bind variables from `pat` in `scope`, and return an action to be taken when
/// leaving the scope to produce the up-evaluated values, either by capturing an
/// upvalue in the scope, or a constant from the pattern.
pub fn lvalue_up(
    ctx: &dyn DiagnosticHandler,
    scope: &mut Scope,
    pat: &ast::Expr,
    ty: Type,
    add_value_sink: &mut impl FnMut() -> ValueSinkId,
) -> Result<LValueSrc, ErrorReported> {
    match pat {
        ast::Expr::Var(ref name) => {
            let id = add_value_sink();
            scope.bind(&name.name, Item::Leaf(LeafItem::Value(Expr::var_up(id, ty))));
            Ok(LValueSrc::Var(id, name.span))
        }

        ast::Expr::Value(lit) => {
            let val = Value::from_literal(&lit.value);

            if !ty.test(&val) {
                ctx.report(Diagnostic::TypeConstraint {
                    span: Span::new(&scope.file, lit.span),
                    found: val.get_type(),
                    bound: ty
                });
            }

            Ok(LValueSrc::Val(ExprDn::Const(val)))
        }

        ast::Expr::Concat(node) => {
            let pat_w: u32 = node.elems.iter().map(|&(w, _)| w.unwrap_or(1)).sum();

            let elem_ty = match ty {
                Type::Vector(ty_w, elem_ty) if pat_w == ty_w => *elem_ty,
                expected_ty => return Err(ctx.report(Diagnostic::PatternExpectedVector {
                    span: Span::new(&scope.file, node.span),
                    found_width: pat_w,
                    expected: expected_ty,
                }))
            };

            let parts = collect_or_err(node.elems.iter()
                .map(|&(width, ref e)| {
                    if let Some(width) = width {
                        let ty_inner = Type::Vector(width, Box::new(elem_ty.clone()));
                        Ok(ConcatElem::Slice(lvalue_up(ctx, scope, e, ty_inner, add_value_sink)?, width))
                    } else {
                        Ok(ConcatElem::Elem(lvalue_up(ctx, scope, e, elem_ty.clone(), add_value_sink)?))
                    }
                })
            );

            Ok(LValueSrc::Concat(parts?))
        }

        pat => Err(ctx.report(Diagnostic::NotAllowedInPattern {
            span: Span::new(&scope.file, pat.span())
        }))
    }
}

/// Pattern matching for constant `alt`
/// 
/// Bind variables from `pat` in `scope`, and return whether the pattern matches a constant
pub fn lvalue_const(
    ctx: &dyn DiagnosticHandler,
    scope: &mut Scope,
    pat: &ast::Expr,
    val: &Value,
) -> Result<bool, ErrorReported> {
    match pat {
        ast::Expr::Var(ref name) => {
            scope.bind(&name.name, Item::Leaf(LeafItem::Value(Expr::Const(val.clone()))));
            Ok(true)
        }

        ast::Expr::Value(lit) => {
            let pat_val = Value::from_literal(&lit.value);
            Ok(&pat_val == val)
        }

        ast::Expr::Concat(node) => {
            let pat_w: u32 = node.elems.iter().map(|&(w, _)| w.unwrap_or(1)).sum();

            let elems = match val {
                Value::Vector(v) if pat_w as usize == v.len() => v,
                _ => return Ok(false),
            };

            let mut elems = elems.into_iter();

            for (width, pat) in node.elems.iter() {
                let res = if let Some(width) = width {
                    let slice = (0..*width).map(|_| elems.next().unwrap().clone()).collect();
                    lvalue_const(ctx, scope, pat, &Value::Vector(slice))?
                } else {
                    lvalue_const(ctx, scope, pat, elems.next().unwrap())?
                };

                if !res { return Ok(false) }
            }

            Ok(true)
        }

        pat => Err(ctx.report(Diagnostic::NotAllowedInPattern {
            span: Span::new(&scope.file, pat.span())
        }))
    }
}

/// Destructures an item into an infallible left-hand-side binding, such as a `let` or function argument
pub fn lexpr(ctx: &dyn DiagnosticHandler, scope: &mut Scope, pat: &ast::Expr, r: &Item) -> Result<(), ErrorReported> {
    zip_ast(ctx, &scope.file.clone(), pat, r, &mut move |pat, r| {
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
                let ty = value(ctx, scope, &node.ty)?.get_type();

                match r {
                    Item::Leaf(LeafItem::Value(ref rv)) => {
                        let found = rv.get_type();
                        if found.is_subtype(&ty) {
                            lexpr(ctx, scope, &node.expr, r)
                        } else {
                            Err(ctx.report(Diagnostic::TypeConstraint {
                                span: Span::new(&scope.file, node.span),
                                found,
                                bound: ty,
                            }))
                        }
                    }
                    Item::Leaf(LeafItem::Invalid(r)) => Err(r.clone()),
                    non_value => Err(ctx.report(Diagnostic::ExpectedValue {
                        span: Span::new(&scope.file, node.span),
                        found: non_value.to_string()
                    }))
                }
            }

            ast::Expr::Value(lv) => {
                let lval = Value::from_literal(&lv.value);
                match r {
                    Item::Leaf(LeafItem::Value(Expr::Const(ref rv))) if lval == *rv => Ok(()),
                    Item::Leaf(LeafItem::Invalid(r)) => Err(r.clone()),
                    non_match => {
                        Err(ctx.report(Diagnostic::ExpectedConst {
                            span: Span::new(&scope.file, lv.span),
                            found: non_match.to_string(),
                            expected: lval.to_string(),
                        }))
                    }
                }
            }

            pat => Err(ctx.report(Diagnostic::NotAllowedInPattern {
                span: Span::new(&scope.file, pat.span())
            }))
        }
    })
}
