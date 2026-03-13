use std::{fmt::{self, Display}, iter, sync::Arc};

use itertools::{Itertools, EitherOrBoth};

use crate::{
    SourceFile, Type, TypeTree, Value, core::{
        Func, FunctionDef, Item, LeafItem, Scope, data::{NumberType, NumberTypeError}, op::{ConcatElem, UnaryOp, eval_binary, eval_choose}, resolve::type_expr::type_tree
    }, diagnostic::{Diagnostic, DiagnosticContext, ErrorReported, Span, collect_or_err}, entitymap::{EntityMap, entity_key}, syntax::{
        Number, ast::{self, AstNode, BinOp}
    }, tree::{Tree, TupleFields}
};

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Const(Value),
    Expr(Type, ExprKind),
}

entity_key!(pub VarId);

/// An expression representing a runtime computation
#[derive(PartialEq, Debug, Clone)]
pub enum ExprKind {
    Ignored,
    Const(Value),
    Var(VarId),
    Range(Number, Number),
    Union(Vec<ExprKind>),
    Flip(Box<ExprKind>, Box<ExprKind>),
    Concat(Vec<ConcatElem<ExprKind>>),
    Unary(Box<ExprKind>, UnaryOp),
}

impl Expr {
    /// Return the `Type` for the set of possible values this expression may down-evaluate to or
    /// match on up-evaluation.
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Const(c) => c.get_type(),
            Expr::Expr(t, _) => t.clone(),
        }
    }

    pub(crate) fn inner(self) -> ExprKind {
        match self {
            Expr::Const(v) => ExprKind::Const(v),
            Expr::Expr(_, k) => k,
        }
    }

    pub fn ignored() -> Self {
        Expr::Expr(Type::Ignored, ExprKind::Ignored)
    }
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Const(c) => c.fmt(f),
            Expr::Expr(_, ExprKind::Ignored) => write!(f, "_"),
            Expr::Expr(_, ExprKind::Const(p)) => write!(f, "{}", p),
            Expr::Expr(_, ExprKind::Range(a, b)) => write!(f, "{}..{}", a, b),
            Expr::Expr(ty, _) => write!(f, "<{ty}>"),
        }
    }
}

pub fn value(dcx: &mut DiagnosticContext, scope: &Scope, e: &ast::Expr) -> Result<Expr, ErrorReported> {
    match rexpr(dcx, scope, e) {
        Item::Leaf(LeafItem::Value(v)) => Ok(v),
        Item::Leaf(LeafItem::Invalid(r)) => Err(r),
        other => Err(dcx.report(
            Diagnostic::ExpectedValue {
                span: scope.span(e.span()),
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

impl TryFromConstant for u32 {
    const EXPECTED_MSG: &'static str = "integer";
}

pub fn constant<'a, T: TryFromConstant>(dcx: &mut DiagnosticContext, scope: &Scope, ast: &ast::Expr) -> Result<T, ErrorReported> {
    let item = rexpr(dcx, scope, ast);
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

    Err(dcx.report(
        Diagnostic::ExpectedConst {
            span: scope.span(ast.span()),
            found,
            expected: T::EXPECTED_MSG.to_string(),
        }
    ))
}

macro_rules! try_item {
    ($e:expr) => {
        match $e {
            Ok(e) => e,
            Err(r) => return LeafItem::Invalid(r).into()
        }
    }
}

pub fn rexpr_tup(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprTup) -> Item {
    if node.fields.len() == 1 && node.fields[0].name.is_none() {
        // Unwrap singleton tuple
        rexpr(dcx, scope, &node.fields[0].expr)
    } else {
        Item::Tuple(node.fields.iter()
            .map(|f| (
                f.name.as_ref().map(|n| n.name.clone()),
                rexpr(dcx, scope, &f.expr)
            )).collect()
        )
    }
}

/// Resolve an expression as used in an argument or right hand side of an assignment
pub fn rexpr(dcx: &mut DiagnosticContext, scope: &Scope, e: &ast::Expr) -> Item {
    match e {
        ast::Expr::Var(name) => {
            if let Some(s) = scope.get(&name.name) { s } else {
                dcx.report(Diagnostic::UndefinedVariable {
                    span: scope.span(name.span),
                    name: name.name.clone()
                }).into()
            }
        }

        ast::Expr::Tup(node) => rexpr_tup(dcx, scope, node),

        ast::Expr::String(node) => Item::Leaf(LeafItem::String(node.value.clone())),

        ast::Expr::Func(node) => {
            Item::Leaf(LeafItem::Func(Arc::new(FunctionDef::Code(Func{
                args: (*node.args).clone(),
                body: (*node.body).clone(),
                file: scope.file.clone(),
                names: scope.names.clone(),
            }))))
        }

        ast::Expr::Type(node) => {
            type_tree(dcx, scope, &node.expr).map(LeafItem::Type).into()
        }

        ast::Expr::Call(node) => {
            let func = rexpr(dcx, scope, &node.func);
            let arg = rexpr_tup(dcx, scope, &node.arg);
            resolve_function_call(dcx, || scope.span(node.span), func, arg)
        }

        ast::Expr::Value(node) => Expr::Const(Value::from_literal(&node.value)).into(),
        ast::Expr::Ignore(_) => Expr::ignored().into(),

        ast::Expr::Typed(node) => resolve_expr_typed(dcx, scope, node),
        ast::Expr::Flip(node) => resolve_expr_flip(dcx, scope, node),
        ast::Expr::Range(node) => resolve_expr_range(dcx, scope, node),
        ast::Expr::Union(node) => resolve_expr_union(dcx, scope, node),
        ast::Expr::Choose(node) => resolve_expr_choose(dcx, scope, node),
        ast::Expr::Concat(node) => resolve_expr_concat(dcx, scope, node),
        ast::Expr::ArrayRep(_) => todo!(),
        ast::Expr::Bin(node) => resolve_expr_binary(dcx, scope, node),

        ast::Expr::Error(e) => Item::Leaf(LeafItem::Invalid(ErrorReported::from_ast(e))),
    }
}

fn resolve_expr_flip(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprFlip) -> Item {
    let dn = node.dn.as_ref().map_or(Ok(Expr::ignored()),  |dn| value(dcx, scope, dn));
    let up = node.up.as_ref().map_or(Ok(Expr::ignored()),  |up| value(dcx, scope, up));

    let dn = try_item!(dn);
    let up = try_item!(up);

    let ty = Type::union(dn.get_type(), up.get_type())
        .map_err(|err| err.report_at(dcx, scope.span(node.span)));
    let ty = try_item!(ty);

    Expr::Expr(ty, ExprKind::Flip(Box::new(dn.inner()), Box::new(up.inner()))).into()
}

fn resolve_expr_range(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprRange) -> Item {
    let min = constant::<Number>(dcx, scope, &node.lo);
    let max = constant::<Number>(dcx, scope, &node.hi);
    let step = node.step.as_ref().map(|s| constant::<Number>(dcx, scope, s)).transpose();

    let min = try_item!(min);
    let max = try_item!(max);
    let step = try_item!(step).unwrap_or(Number::new(1, 1));

    let nt = match NumberType::from_scaled(min, max, step) {
        Ok(t) => t,
        Err(NumberTypeError::BoundsNotMultipleOfStep) => return dcx.report(
            Diagnostic::RangeNotMultipleOfStep {
                min, min_span: scope.span(node.lo.span()),
                max, max_span: scope.span(node.hi.span()),
                step
            }
        ).into(),
        Err(NumberTypeError::Order) => return dcx.report(
            Diagnostic::RangeOrder {
                min, min_span: scope.span(node.lo.span()),
                max, max_span: scope.span(node.hi.span()),
            }).into(),
        Err(NumberTypeError::StepIsZero) => return dcx.report(
            Diagnostic::RangeStepZero {
                step, step_span: scope.span(node.step.as_ref().unwrap().span()),
            }).into(),
    };

    Expr::Expr(Type::Number(nt), ExprKind::Range(min, max)).into()
}

fn resolve_expr_typed(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprTyped) -> Item {
    let expr = value(dcx, scope, &node.expr);
    let bound_expr = value(dcx, scope, &node.ty);

    let expr = try_item!(expr);
    let bound = try_item!(bound_expr).get_type();

    let span = || scope.span(node.span);

    match expr {
        Expr::Const(c) => {
            if bound.test(&c) {
                Expr::Const(c).into()
            } else {
                dcx.report(Diagnostic::TypeConstraint { span: span(), found: c.get_type(), bound }).into()
            }
        },
        Expr::Expr(t, e) => {
            match t.bound(bound) {
                Ok(new_t) => Expr::Expr(new_t, e).into(),
                Err((t, bound)) =>
                    dcx.report(Diagnostic::TypeConstraint { span: span(), found: t, bound }).into(),
            }
        },
    }
}

fn resolve_expr_union(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprUnion) -> Item {
    let opts: Vec<_> = try_item!(
        collect_or_err(node.items.iter().map(|i| value(dcx, scope, i)))
    );

    let ty = Type::union_iter(opts.iter().map(|x| x.get_type()));
    let ty = try_item!(ty.map_err(|err| err.report_at(dcx, scope.span(node.span))));

    Expr::Expr(ty, ExprKind::Union(opts.into_iter().map(|x| x.inner()).collect())).into()
}

fn resolve_expr_choose(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprChoose) -> Item {
    let e = value(dcx, scope, &node.e);
    let pairs: Result<Vec<_>, _> = collect_or_err(
        node.choices.iter().map(|&(ref le, ref re)| {
            let l = constant::<Value>(dcx, scope, le);
            let r = constant::<Value>(dcx, scope, re);
            Ok((l?, r?))
        }
    ));

    let e = try_item!(e);
    let pairs = try_item!(pairs).into_boxed_slice();

    let span = || scope.span(node.span);

    let lt = Type::union_iter(pairs.iter().map(|x| x.0.get_type()));
    let rt = Type::union_iter(pairs.iter().map(|x| x.1.get_type()));

    let lt = try_item!(lt.map_err(|err| err.report_at(dcx, span())));
    let rt = try_item!(rt.map_err(|err| err.report_at(dcx, span())));

    match e {
        Expr::Const(c) => {
            let Some(v) = eval_choose(&c, &pairs) else {
                return dcx.report(crate::Diagnostic::ChooseNotCovered { span: span(), found: c.get_type() }).into();
            };

            Expr::Const(v)
        }
        Expr::Expr(ty, e) => {
            //TODO: doesn't check coverage of full number range
            if !(ty.is_subtype(&lt) && lt.is_subtype(&ty)) {
                return dcx.report(crate::Diagnostic::ChooseNotCovered { span: span(), found: ty }).into();
            }

            Expr::Expr(rt, ExprKind::Unary(Box::new(e), UnaryOp::Mapping(pairs)))
        }
    }.into()
}

fn resolve_expr_concat(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprConcat) -> Item {
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
        let elem = value(dcx, scope, e)?;
        let span = || scope.span(node.span);

        if let Some(w) = width {
            match elem {
                Expr::Const(Value::Vector(vs)) if w as usize == vs.len() => {
                    for c in vs {
                        elem_ty.union_with(c.get_type()).map_err(|err| err.report_at(dcx, span()))?;
                        elems.push_const(c);
                        len += 1;
                    }
                }

                Expr::Expr(Type::Vector(w1, ty), e) if w == w1 => {
                    len += w;
                    elem_ty.union_with(*ty).map_err(|err| err.report_at(dcx, span()))?;

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

                other => Err(dcx.report(Diagnostic::ExpectedVector {
                    span: span(),
                    found: other.get_type(),
                }))?
            }
            Ok(())
        } else {
            match elem {
                Expr::Const(c) => {
                    elem_ty.union_with(c.get_type()).map_err(|err| err.report_at(dcx, span()))?;
                    len += 1;
                    elems.push_const(c);
                }
                Expr::Expr(ty, e) => {
                    elem_ty.union_with(ty).map_err(|err| err.report_at(dcx, span()))?;
                    len += 1;
                    elems.push(ConcatElem::Elem(e))
                }
            }
            Ok(())
        }
    });
    try_item!(collect_or_err::<(), ()>(it));

    match elems {
        ConcatBuilder::Const(v) => Expr::Const(Value::Vector(v)),
        ConcatBuilder::Expr(e) => Expr::Expr(
            Type::Vector(len, Box::new(elem_ty)),
            ExprKind::Concat(e)
        ),
    }.into()
}

fn resolve_expr_binary(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprBin) -> Item {
    let lhs = value(dcx, scope, &node.l);
    let rhs = value(dcx, scope, &node.r);

    let lhs = try_item!(lhs);
    let rhs = try_item!(rhs);

    let span = || scope.span(node.span);

    // Swap constant operand to the right, or constant-fold and return
    let (expr, expr_ty, expr_span, op, val, val_span) = match (lhs, rhs) {
        (Expr::Const(l), Expr::Const(r)) => {
            if let Some(v) = eval_binary(l.clone(), node.op, r.clone()) {
                return Expr::Const(v).into()
            } else {
                return dcx.report(
                Diagnostic::BinaryInvalidType {
                    span1: scope.span(node.l.span()),
                    ty1: l.get_type(),
                    span2: scope.span(node.r.span()),
                    ty2: r.get_type(),
                 }
                ).into()
            }
        }
        (Expr::Expr(..), Expr::Expr(..)) => {
            return dcx.report(Diagnostic::BinaryOneSideMustBeConst { span: span() }).into();
        }
        (Expr::Expr(l_ty, l), Expr::Const(r)) => (l, l_ty, node.l.span(), node.op, r, node.r.span()),
        (Expr::Const(l), Expr::Expr(r_ty, r)) => (r, r_ty, node.r.span(), node.op.swap(), l, node.l.span()),
    };

    let ty = match (expr_ty, &val) {
        (Type::Number(nt), Value::Number(c)) => {
            let mut check_scale = |n: Option<NumberType>| {
                n.ok_or_else(|| {
                    dcx.report(Diagnostic::OperandNotMultipleOfScale {
                        const_span: scope.span(val_span),
                        const_val: *c,
                        var_span: scope.span(expr_span),
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
                BinOp::DivSwap => return dcx.report(Diagnostic::DivisionMustBeConst {
                    span: scope.span(expr_span),
                }).into(),
            })
        }
        (Type::NumberSet(ls), Value::Number(c)) => {
            Type::NumberSet(ls.into_iter().map(|l| op.eval(l, c)).collect())
        }
        (Type::Complex, Value::Number(..)) => Type::Complex,
        (Type::Number(..), Value::Complex(..)) => Type::Complex,
        (expr_ty, _) => return dcx.report(
            Diagnostic::BinaryInvalidType {
                span1: scope.span(expr_span),
                ty1: expr_ty,
                span2: scope.span(val_span),
                ty2: val.get_type(),
             }
        ).into()
    };

    match val {
        Value::Number(val) => {
            Expr::Expr(ty, ExprKind::Unary(Box::new(expr), UnaryOp::BinaryConstNumber(op, val))).into()
        },
        _ => todo!()
    }
}

fn resolve_function_call(dcx: &mut DiagnosticContext, call_site_span: impl FnOnce() -> Span, func: Item, arg: Item) -> Item {
    match func {
        Item::Leaf(LeafItem::Func(func_def)) => {
            match *func_def {
                FunctionDef::Code(ref func) => {
                    let mut inner_scope = Scope { file: func.file.clone(), names: func.names.clone() };
                    lexpr(dcx, &mut inner_scope, &func.args, &arg);
                    rexpr(dcx, &inner_scope, &func.body)
                }
                FunctionDef::Primitive(primitive) => {
                    primitive(arg).unwrap_or_else(|msg| {
                        dcx.report(Diagnostic::ErrorInPrimitiveFunction {
                            span: call_site_span(),
                            msg: msg.into(),
                        }).into()
                    })
                },
            }
        }
        e @ Item::Leaf(LeafItem::Invalid(_)) => e,
        _ => dcx.report(Diagnostic::NotAFunction {
            span: call_site_span(),
            found: format!("{}", func)
        }).into()
    }
}

/// Pattern matching for constant `alt`
///
/// Bind variables from `pat` in `scope`, and return whether the pattern matches a constant
pub fn lvalue_const(
    dcx: &mut DiagnosticContext,
    scope: &mut Scope,
    pat: &ast::Expr,
    val: &Value,
) -> Result<bool, ErrorReported> {
    match pat {
        ast::Expr::Var(name) => {
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
                    lvalue_const(dcx, scope, pat, &Value::Vector(slice))?
                } else {
                    lvalue_const(dcx, scope, pat, elems.next().unwrap())?
                };

                if !res { return Ok(false) }
            }

            Ok(true)
        }

        pat => Err(dcx.report(Diagnostic::NotAllowedInPattern {
            span: scope.span(pat.span())
        }))
    }
}

/// Match the fields of a tuple expression `tup_ast` with `tree`.
///
/// The fields will be iterated in the order of `tree`.
pub fn zip_tuple_ast<'a, 't, T: Display>(
    dcx: &mut DiagnosticContext,
    file: &Arc<SourceFile>,
    tup_ast: &'a ast::ExprTup,
    tree: &'t Tree<T>,
) -> impl Iterator<Item = ZipTupleResult<&'a ast::Expr, &'t Tree<T>>> {
    enum IterRes<A, B> {
        A(A),
        B(B),
        Empty,
    }

    impl<A: Iterator, B: Iterator<Item = A::Item>> Iterator for IterRes<A, B> {
        type Item = A::Item;

        fn next(&mut self) -> Option<Self::Item> {
            match self {
                IterRes::A(a) => a.next(),
                IterRes::B(b) => b.next(),
                IterRes::Empty => None
            }
        }
    }

    match tree {
        Tree::Tuple(f) => {
            IterRes::A(zip_tuple_ast_fields(dcx, file, tup_ast, f))
        }
        t if tup_ast.fields.len() == 1 && tup_ast.fields[0].name.is_none() => {
            IterRes::B(iter::once(ZipTupleResult::Both(&tup_ast.fields[0].expr, t)))
        }
        t => {
            dcx.report(Diagnostic::ExpectedTuple {
                span: Span::new(file, tup_ast.span),
                found: t.to_string()
            });
            IterRes::Empty
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ZipTupleResult<A, B> {
    /// Both values are present.
    Both(A, B),
    /// Only the left value of type `A` is present.
    Left(A, ErrorReported),
    /// Only the right value of type `B` is present.
    Right(ErrorReported, B),
}

pub fn zip_tuple_ast_fields<'a, 't, T>(
    dcx: &mut DiagnosticContext,
    file: &Arc<SourceFile>,
    tup_ast: &'a ast::ExprTup,
    f: &'t TupleFields<T>,
) -> impl Iterator<Item = ZipTupleResult<&'a ast::Expr, &'t T>> {
    let ast_positional_count = tup_ast.positional().count();

    if ast_positional_count < f.positional.len() {
        let n = f.positional.len() - ast_positional_count;
        let span = tup_ast.close.as_ref().map(|a| a.span).unwrap_or(tup_ast.span);
        dcx.report(Diagnostic::TupleTooFewPositional { span: Span::new(file, span), n });

    } else if ast_positional_count > f.positional.len() {
        let n = ast_positional_count - f.positional.len();
        let span = tup_ast.positional().nth(f.positional.len()).unwrap().span()
            .to(tup_ast.positional().last().unwrap().span());
        dcx.report(Diagnostic::TupleTooManyPositional { span: Span::new(file, span), n });
    }

    for name in f.named.keys() {
        if !tup_ast.fields.iter().any(|field_ast| field_ast.name.as_ref().is_some_and(|i| &i.name == name)) {
            let span = tup_ast.close.as_ref().map(|a| a.span).unwrap_or(tup_ast.span);
            dcx.report(Diagnostic::TupleMissingNamed { span: Span::new(file, span), name: name.clone() });
        }
    }

    for (name, span) in tup_ast.fields.iter().filter_map(|a| a.name.as_ref().map(|i| (&i.name, a.span))) {
        if !f.named.contains_key(name) {
            dcx.report(Diagnostic::TupleExtraNamed { span: Span::new(file, span), name: name.clone() });
        }
    }

    tup_ast.positional()
        .zip_longest(f.positional.iter())
        .map(|z| match z {
            EitherOrBoth::Both(a, b) => ZipTupleResult::Both(a, b),
            EitherOrBoth::Left(a) => ZipTupleResult::Left(a, ErrorReported::error_reported()),
            EitherOrBoth::Right(b) => ZipTupleResult::Right(ErrorReported::error_reported(), b),
        })
        .chain(f.named.iter().map(|(name, v)| {
            tup_ast.fields.iter()
                .find(|field_ast| field_ast.name.as_ref().is_some_and(|i| &i.name == name))
                .map_or(ZipTupleResult::Right(ErrorReported::error_reported(), v), |f| ZipTupleResult::Both(&f.expr, v))
        }))
        .chain(tup_ast.fields.iter().filter(|a| {
            a.name.as_ref().is_some_and(|n| !f.named.contains_key(&n.name))
        }).map(|a| ZipTupleResult::Left(&a.expr, ErrorReported::error_reported())))
}

pub fn lexpr_tup(dcx: &mut DiagnosticContext, scope: &mut Scope, tup_pat: &ast::ExprTup, r: &Item) {
    for m in zip_tuple_ast(dcx, &scope.file, tup_pat, r) {
        match m {
            ZipTupleResult::Both(pat, r) => lexpr(dcx, scope, pat, r),
            ZipTupleResult::Left(pat, reported) => {
                lexpr(dcx, scope, pat, &Tree::Leaf(LeafItem::Invalid(reported)))
            }
            ZipTupleResult::Right(_, _) => {}
        }
    }
}

/// Destructures an item into an infallible left-hand-side binding, such as a `let` or function argument
pub fn lexpr(dcx: &mut DiagnosticContext, scope: &mut Scope, pat: &ast::Expr, r: &Item) {
    match pat {
        ast::Expr::Tup(tup_pat) => lexpr_tup(dcx, scope, tup_pat, r),
        ast::Expr::Ignore(_) => {}

        ast::Expr::Var(name) => {
            debug!("defined {} = {:?}", name.name, r);
            scope.bind(&name.name, r.clone());
        }

        ast::Expr::Typed(node) => {
            let ty = match value(dcx, scope, &node.ty) {
                Ok(v) => v.get_type(),
                Err(reported) => {
                    return lexpr(dcx, scope, &node.expr, &Tree::Leaf(LeafItem::Invalid(reported)));
                }
            };

            match r {
                Item::Leaf(LeafItem::Value(rv)) => {
                    let found = rv.get_type();
                    if found.is_subtype(&ty) {
                        lexpr(dcx, scope, &node.expr, r)
                    } else {
                        let reported = dcx.report(Diagnostic::TypeConstraint {
                            span: scope.span(node.span),
                            found,
                            bound: ty,
                        });
                        lexpr(dcx, scope, &node.expr, &Tree::Leaf(LeafItem::Invalid(reported)));
                    }
                }
                non_value => {
                    let reported = if let Item::Leaf(LeafItem::Invalid(r)) = non_value { r.clone() } else {
                        dcx.report(Diagnostic::ExpectedValue {
                            span: scope.span(node.span),
                            found: non_value.to_string()
                        })
                    };
                    lexpr(dcx, scope, &node.expr, &Tree::Leaf(LeafItem::Invalid(reported)));
                }
            }
        }

        ast::Expr::Value(lv) => {
            let lval = Value::from_literal(&lv.value);
            match r {
                Item::Leaf(LeafItem::Value(Expr::Const(rv))) if lval == *rv => {},
                Item::Leaf(LeafItem::Invalid(_)) => {},
                non_match => {
                    dcx.report(Diagnostic::ExpectedConst {
                        span: scope.span(lv.span),
                        found: non_match.to_string(),
                        expected: lval.to_string(),
                    });
                }
            }
        }

        ast::Expr::Type(t) => {
            let _t = type_tree(dcx, scope, &t.expr);
            // TODO: compare them
        }

        pat => {
            dcx.report(Diagnostic::NotAllowedInPattern {
                span: scope.span(pat.span())
            });
        }
    }
}

pub fn bind_fields_const(
    dcx: &mut DiagnosticContext,
    scope: &mut Scope,
    pat: &ast::Expr,
    rhs: &Item,
) -> bool {
    match (pat, rhs) {
        (ast::Expr::Tup(pat_tup), r) => {
            let mut matched = true;
            for m in zip_tuple_ast(dcx, &scope.file, pat_tup, r) {
                match m {
                    ZipTupleResult::Both(p, t) => {
                        matched &= bind_fields_const(dcx, scope, p, t);
                    }
                    ZipTupleResult::Left(_, reported) => {
                        matched = false;
                        bind_fields_const(dcx, scope, pat, &reported.into());
                    }
                    ZipTupleResult::Right(_, _) => {
                        matched = false;
                    }
                }
            }
            matched
        }
        (ast::Expr::Ignore(_), _) => true,
        (ast::Expr::Var(name), t) => {
            scope.bind(&name.name, t.clone());
            true
        }
        (pat, Tree::Leaf(LeafItem::Value(Expr::Const(c)))) => {
            match lvalue_const(dcx, scope, pat, c) {
                Ok(p) => p,
                Err(_) => false,
            }
        }
        (_, Tree::Leaf(LeafItem::Value(e))) => {
            dcx.report(Diagnostic::ExpectedConst {
                span: scope.span(pat.span()),
                found: e.to_string(),
                expected: "value".into(),
            });
            false
        }
        (_, Tree::Leaf(LeafItem::Invalid(_))) => false,
        (pat, e) => {
            dcx.report(Diagnostic::InvalidItemForPattern {
                span: scope.span(pat.span()),
                found: e.to_string(),
            });
            false
        }
    }
}

pub enum Pattern {
    Ignored,
    Const(Value),
    Var(VarId),
    Concat(Vec<ConcatElem<Pattern>>),
}

pub fn bind_fields(
    dcx: &mut DiagnosticContext,
    scope: &mut Scope,
    pat: &ast::Expr,
    rhs: &TypeTree,
    vars: &mut EntityMap<VarId, ()>,
    add_field: &mut impl FnMut(Result<Pattern, ErrorReported>),
) {
    match (pat, rhs) {
        (ast::Expr::Tup(pat_tup), r) => {
            for m in zip_tuple_ast(dcx, &scope.file, pat_tup, r) {
                match m {
                    ZipTupleResult::Both(p, t) => bind_fields(dcx, scope, p, t, vars, add_field),
                    ZipTupleResult::Left(_, _) => {
                        //TODO: bind variables with invalid
                    }
                    ZipTupleResult::Right(reported, t) => {
                        // pad for missing fields
                        t.for_each(&mut |_| { add_field(Err(reported.clone())) })
                    }
                }
            }
        }
        (ast::Expr::Ignore(_), ty) => {
            // Ignore one or more fields
            ty.for_each(&mut |_| {
                add_field(Ok(Pattern::Ignored));
            });
        }
        (ast::Expr::Var(name), ty) => {
            scope.bind(&name.name, ty.map_leaf(&mut |ty| {
                let var = vars.push(());
                add_field(Ok(Pattern::Var(var)));
                LeafItem::Value(Expr::Expr(ty.clone(), ExprKind::Var(var)))
            }));
        }
        (pat, Tree::Leaf(ty)) => {
            add_field(bind_field(dcx, scope, pat, ty, vars));
        }
        (pat, t) => {
            let r = dcx.report(Diagnostic::InvalidItemForPattern {
                span: scope.span(pat.span()),
                found: t.to_string(),
            });

            t.for_each(&mut |_| {
                add_field(Err(r.clone()));
            });
        }
    }
}

pub fn bind_field(dcx: &mut DiagnosticContext, scope: &mut Scope, pat: &ast::Expr, ty: &Type, vars: &mut EntityMap<VarId, ()>) -> Result<Pattern, ErrorReported> {
    match pat {
        ast::Expr::Var(name) => {
            let var = vars.push(());
            scope.bind(&name.name, Item::Leaf(LeafItem::Value(Expr::Expr(ty.clone(), ExprKind::Var(var)))));
            Ok(Pattern::Var(var))
        }

        ast::Expr::Ignore(_) => Ok(Pattern::Ignored),

        ast::Expr::Value(lit) => {
            let val = Value::from_literal(&lit.value);

            if !ty.test(&val) {
                dcx.report(Diagnostic::TypeConstraint {
                    span: scope.span(lit.span),
                    found: val.get_type(),
                    bound: ty.clone()
                });
            }

            Ok(Pattern::Const(val))
        }

        ast::Expr::Concat(node) => {
            let pat_w: u32 = node.elems.iter().map(|&(w, _)| w.unwrap_or(1)).sum();

            let elem_ty = match ty {
                Type::Vector(ty_w, elem_ty) if pat_w == *ty_w => elem_ty,
                expected_ty => return Err(dcx.report(Diagnostic::PatternExpectedVector {
                    span: scope.span(node.span),
                    found_width: pat_w,
                    expected: expected_ty.clone(),
                }))
            };

            let parts = collect_or_err(node.elems.iter()
                .map(|&(width, ref e)| {
                    if let Some(width) = width {
                        let ty_inner = Type::Vector(width, elem_ty.clone());
                        Ok(ConcatElem::Slice(bind_field(dcx, scope, e, &ty_inner, vars)?, width))
                    } else {
                        Ok(ConcatElem::Elem(bind_field(dcx, scope, e, elem_ty, vars)?))
                    }
                })
            )?;

            Ok(Pattern::Concat(parts))
        }

        pat => Err(dcx.report(Diagnostic::NotAllowedInPattern {
            span: scope.span(pat.span())
        }))
    }
}


#[cfg(test)]
pub fn test_expr_parse(e: &str) -> Expr {
    use crate::diagnostic::{DiagnosticContext, print_diagnostics};
    use crate::syntax::{parse_expr, SourceFile};
    use crate::core::Scope;

    let mut dcx = DiagnosticContext::new();

    let scope = Scope {
        file: Arc::new(SourceFile::new("<tests>".into(), "".into())),
        names: crate::core::primitive_fn::expr_prelude()
    };

    let ast = parse_expr(e).unwrap();
    let v = value(&mut dcx, &scope, &ast).unwrap();

    print_diagnostics(dcx.diagnostics());

    v
}

#[test]
fn test_number_const() {
    let two = test_expr_parse("2");
    assert_eq!(two.get_type(), Type::NumberSet([Number::new(2, 1)].into()));

    let decimal = test_expr_parse("1.023");
    assert_eq!(decimal.get_type(), Type::NumberSet([Number::new(1023, 1000)].into()));

    let four = test_expr_parse("2 + 2");
    assert_eq!(four.get_type(), Type::NumberSet([Number::new(4, 1)].into()));
}

#[test]
fn test_number_range() {
    let range = test_expr_parse("0.0..5.0");
    assert_eq!(range.get_type(), Type::Number(NumberType::new(Number::new(1, 1), 0, 5)));

    let sum = test_expr_parse("(0..10) + 5");
    assert_eq!(sum.get_type(), Type::Number(NumberType::new(Number::new(1, 1), 5, 15)));

    let sub = test_expr_parse("(0..10) - 5");
    assert_eq!(sub.get_type(), Type::Number(NumberType::new(Number::new(1, 1), -5, 5)));

    let sub_swap = test_expr_parse("5 - (0..7)");
    assert_eq!(sub_swap.get_type(), Type::Number(NumberType::new(Number::new(-1, 1), -5, 2)));

    let mul = test_expr_parse("(0..10) * 5");
    assert_eq!(mul.get_type(), Type::Number(NumberType::new(Number::new(5, 1), 0, 10)));

    let div = test_expr_parse("(0..10) / 8");
    assert_eq!(div.get_type(), Type::Number(NumberType::new(Number::new(1, 8), 0, 10)));

    let scale_by = test_expr_parse("0.0..2.0 by 0.1");
    assert_eq!(scale_by.get_type(), Type::Number(NumberType::new(Number::new(1, 10), 0, 20)));

    let scale_by_mul = test_expr_parse("(0.0..2.0 by 0.1) * 5");
    assert_eq!(scale_by_mul.get_type(), Type::Number(NumberType::new(Number::new(5, 10), 0, 20)));
}

#[test]
fn test_number_set() {
    let range = test_expr_parse("0 | 1.5");
    assert_eq!(range.get_type(), Type::NumberSet([Number::new(0, 1), Number::new(3, 2)].into()));

    let sum = test_expr_parse("(0|1) + 2");
    assert_eq!(sum.get_type(), Type::NumberSet([Number::new(2, 1), Number::new(3, 1)].into()));

    let sub = test_expr_parse("(100 | 200) - 2");
    assert_eq!(sub.get_type(), Type::NumberSet([Number::new(98, 1), Number::new(198, 1)].into()));

    let sub_swap = test_expr_parse("5 - (1 | 7)");
    assert_eq!(sub_swap.get_type(), Type::NumberSet([Number::new(4, 1), Number::new(-2, 1)].into()));

    let mul = test_expr_parse("(-1 | 2) * 5");
    assert_eq!(mul.get_type(), Type::NumberSet([Number::new(-5, 1), Number::new(10, 1)].into()));

    let div = test_expr_parse("(64 | 32) / 8");
    assert_eq!(div.get_type(), Type::NumberSet([Number::new(8, 1), Number::new(4, 1)].into()));

    let div_swap = test_expr_parse("128 / (64 | 32)");
    assert_eq!(div_swap.get_type(), Type::NumberSet([Number::new(2, 1), Number::new(4, 1)].into()));
}

#[test]
fn exprs() {
    let one_one_i = test_expr_parse("complex(1.0, 0.0) + complex(0, 1)");
    assert_eq!(one_one_i.get_type(), Type::Complex);

    let two_two_i = test_expr_parse("complex(1, 1) * 2");
    assert_eq!(two_two_i.get_type(), Type::Complex);

    let choose = test_expr_parse("(#a | #b)[#a = #x, #b = #y]");
    assert_eq!(choose.get_type(), Type::Symbol(["x".into(), "y".into()].into_iter().collect()));

    let concat = test_expr_parse("[(#a|#b), #c, _, 2:[(#a | #c), _], #a]");
    assert_eq!(concat.get_type(), Type::Vector(6, Box::new(
        Type::Symbol(["a".into(), "b".into(), "c".into()].into_iter().collect())
    )));

    let ignore = test_expr_parse("_");
    assert_eq!(ignore.get_type(), Type::Ignored);

    let down = test_expr_parse("<: #h");
    assert_eq!(down.get_type(), Type::Symbol(Some("h".to_string()).into_iter().collect()));

    let bound1 = test_expr_parse("_ : (0..10)");
    assert_eq!(bound1.get_type(), Type::Number(NumberType::new(Number::new(1, 1), 0, 10)));

    let bound2 = test_expr_parse("(0..2) : (0..10)");
    assert_eq!(bound2.get_type(), Type::Number(NumberType::new(Number::new(1, 1), 0, 2)));

    let fncall = test_expr_parse("((a) => a+3.0)(2.0)");
    assert_eq!(fncall.get_type(), Type::NumberSet([Number::new(5, 1)].into()));
}

#[test]
fn vec_const_fold() {
    assert_eq!(
        test_expr_parse("[1, 2, 2:[3, 1:[4]], 5]"),
        Expr::Const(Value::Vector(vec![1i64.into(), 2i64.into(), 3i64.into(), 4i64.into(), 5i64.into()])),
    );

    assert_eq!(
        test_expr_parse("[1, 2:[2, _], 3]"),
        test_expr_parse("[1, 2, _, 3]")
    );
}
