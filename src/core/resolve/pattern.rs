use std::{fmt::Display, sync::Arc};

use itertools::{Itertools, EitherOrBoth};

use crate::{
    Dir, SourceFile, Type, TypeTree, Value, core::{
        Item, LeafItem, Scope,
        op::ConcatElem ,
        resolve::{action::Vars, expr::{ExprKind, VarId}, type_expr::{type_expr, type_tree}},
    }, diagnostic::{Diagnostic, DiagnosticContext, ErrorReported, Span, collect_or_err}, syntax::
        ast::{self, AstNode}
    , tree::Tree
};

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
            scope.bind(&name.name, Item::Leaf(LeafItem::Value(ExprKind::Const(val.clone()))));
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
) -> impl Iterator<Item = ZipTupleResult<&'a ast::Expr, &'t Tree<T>>> + use<'a, 't, T> {
    enum IterRes<T, A, B> {
        A(A),
        B(B),
        Once(Option<T>),
    }

    impl<T, A: Iterator<Item = T>, B: Iterator<Item = T>> Iterator for IterRes<T, A, B> {
        type Item = T;

        fn next(&mut self) -> Option<Self::Item> {
            match self {
                IterRes::A(a) => a.next(),
                IterRes::B(b) => b.next(),
                IterRes::Once(t) => t.take(),
            }
        }
    }

    match tree {
        Tree::Positional(f) => {
            if tup_ast.has_named() {
                dcx.report(Diagnostic::TupleExpectedPositional { span: Span::new(file, tup_ast.span) });
            } else {
                let ast_positional_count = tup_ast.positional().count();

                if ast_positional_count < f.len() {
                    let n = f.len() - ast_positional_count;
                    let span = tup_ast.close.as_ref().map(|a| a.span).unwrap_or(tup_ast.span);
                    dcx.report(Diagnostic::TupleTooFewPositional { span: Span::new(file, span), n });
                } else if ast_positional_count > f.len() {
                    let n = ast_positional_count - f.len();
                    let span = tup_ast.positional().nth(f.len()).unwrap().span()
                        .to(tup_ast.positional().last().unwrap().span());
                    dcx.report(Diagnostic::TupleTooManyPositional { span: Span::new(file, span), n });
                }
            }

            IterRes::A(tup_ast.positional().zip_longest(f.iter())
                .map(|z| match z {
                    EitherOrBoth::Both(a, b) => ZipTupleResult::Both(a, b),
                    EitherOrBoth::Left(a) => ZipTupleResult::Left(a, ErrorReported::error_reported()),
                    EitherOrBoth::Right(b) => ZipTupleResult::Right(ErrorReported::error_reported(), b),
                })
                .chain(tup_ast.fields.iter().filter(|a| a.name.is_some())
                    .map(|a| ZipTupleResult::Left(&a.expr, ErrorReported::error_reported()))
                )
            )
        }
        Tree::Named(f) => {
            if tup_ast.has_positional() {
                dcx.report(Diagnostic::TupleExpectedNamed { span: Span::new(file, tup_ast.span) });
            } else {
                for name in f.keys() {
                    if !tup_ast.fields.iter().any(|field_ast| field_ast.name.as_ref().is_some_and(|i| &i.name == name)) {
                        let span = tup_ast.close.as_ref().map(|a| a.span).unwrap_or(tup_ast.span);
                        dcx.report(Diagnostic::TupleMissingNamed { span: Span::new(file, span), name: name.clone() });
                    }
                }

                for (name, span) in tup_ast.fields.iter().filter_map(|a| a.name.as_ref().map(|i| (&i.name, a.span))) {
                    if !f.contains_key(name) {
                        dcx.report(Diagnostic::TupleExtraNamed { span: Span::new(file, span), name: name.clone() });
                    }
                }
            }


            IterRes::B(
                tup_ast.positional().map(|a| ZipTupleResult::Left(a, ErrorReported::error_reported()))
                    .chain(f.iter().map(|(name, v)| {
                        tup_ast.fields.iter()
                            .find(|field_ast| field_ast.name.as_ref().is_some_and(|i| &i.name == name))
                            .map_or(ZipTupleResult::Right(ErrorReported::error_reported(), v), |f| ZipTupleResult::Both(&f.expr, v))
                    }))
                    .chain(tup_ast.fields.iter().filter(|a| {
                        a.name.as_ref().is_some_and(|n| !f.contains_key(&n.name))
                    }).map(|a| ZipTupleResult::Left(&a.expr, ErrorReported::error_reported())))
            )
        }
        t @ Tree::Leaf(_) if tup_ast.fields.len() == 1 && tup_ast.fields[0].name.is_none() => {
            IterRes::Once(Some(ZipTupleResult::Both(&tup_ast.fields[0].expr, t)))
        }
        t @ Tree::Leaf(_) => {
            dcx.report(Diagnostic::ExpectedTuple {
                span: Span::new(file, tup_ast.span),
                found: t.to_string()
            });
            IterRes::Once(None)
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
            let ty = match type_expr(dcx, scope, &node.ty) {
                Ok(ty) => ty,
                Err(reported) => {
                    return lexpr(dcx, scope, &node.expr, &Tree::Leaf(LeafItem::Invalid(reported)));
                }
            };

            match r {
                Item::Leaf(LeafItem::Value(rv)) => {
                    if rv.check_type(&ty) {
                        lexpr(dcx, scope, &node.expr, r)
                    } else {
                        let reported = dcx.report(Diagnostic::TypeConstraint {
                            span: scope.span(node.span),
                            found: rv.get_type(),
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
                Item::Leaf(LeafItem::Value(ExprKind::Const(rv))) if lval == *rv => {},
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
        (pat, Tree::Leaf(LeafItem::Value(ExprKind::Const(c)))) => {
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

impl Pattern {
    pub fn each_var(&self, f: &mut impl FnMut(VarId)) {
        match *self {
            Pattern::Var(v) => f(v),
            Pattern::Concat(ref parts) => {
                parts.iter().for_each(|p| p.inner().each_var(f));
            }
            Pattern::Ignored | Pattern::Const(_) => {}
        }
    }
}

pub fn bind_fields(
    dcx: &mut DiagnosticContext,
    vars: &mut Vars,
    scope: &mut Scope,
    pat: &ast::Expr,
    rhs: &TypeTree,
    dir: Dir,
    add_field: &mut impl FnMut(Result<Pattern, ErrorReported>),
) {
    match (pat, rhs) {
        (ast::Expr::Tup(pat_tup), r) => {
            for m in zip_tuple_ast(dcx, &scope.file, pat_tup, r) {
                match m {
                    ZipTupleResult::Both(p, t) => bind_fields(dcx, vars, scope, p, t, dir, add_field),
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
                let var = vars.add(scope.span(name.span), dir.into());
                add_field(Ok(Pattern::Var(var)));
                LeafItem::Value(ExprKind::Var { ty: ty.clone(), id: var })
            }));
        }
        (pat, Tree::Leaf(ty)) => {
            add_field(bind_field(dcx, vars, scope, pat, ty, dir));
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

pub fn bind_field(dcx: &mut DiagnosticContext, vars: &mut Vars, scope: &mut Scope, pat: &ast::Expr, ty: &Type, dir: Dir) -> Result<Pattern, ErrorReported> {
    match pat {
        ast::Expr::Var(name) => {
            let var = vars.add(scope.span(name.span), dir.into());
            scope.bind(&name.name, Item::Leaf(LeafItem::Value(ExprKind::Var { ty: ty.clone(), id: var })));
            Ok(Pattern::Var(var))
        }

        // When up-evaluated, the expression must produce a value
        // so ignore patterns must be rejected.
        ast::Expr::Ignore(_) if dir != Dir::Up => Ok(Pattern::Ignored),

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
                        Ok(ConcatElem::Slice(bind_field(dcx, vars, scope, e, &ty_inner, dir)?, width))
                    } else {
                        Ok(ConcatElem::Elem(bind_field(dcx, vars, scope, e, elem_ty, dir)?))
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
