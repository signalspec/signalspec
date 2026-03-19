use std::{cmp::{max, min}, fmt::{self, Display}, sync::Arc};

use indexmap::IndexSet;
use itertools::Itertools;
use num_traits::{One, Signed};

use crate::{
    Dir, Type, Value, core::{
        Func, FunctionDef, Item, LeafItem, Scope,
        data::NumberType,
        op::{ConcatElem, UnaryOp, eval_binary, eval_binary_on_range, eval_choose},
        resolve::{lexpr, type_expr::{type_expr, type_tree}},
    }, diagnostic::{Diagnostic, DiagnosticContext, ErrorReported, Span, Spanned, collect_or_err}, entitymap::entity_key, syntax::{
        Number, ast::{self, AstNode, BinOp}
    }, util::IterUtil
};

entity_key!(pub VarId);

impl Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var{}", self.0)
    }
}

/// An expression representing a runtime computation
#[derive(PartialEq, Debug, Clone)]
pub enum ExprKind {
    Ignored,
    Const(Value),
    Range(Number, Option<Number>),
    Enum(IndexSet<Value>),
    Var { ty: ExprType, id: VarId },
    Flip(Box<ExprKind>, Box<ExprKind>),
    Concat(ExprType, u32, Vec<ConcatElem<ExprKind>>),
    Unary { ty: ExprType, expr: Box<ExprKind>, op: UnaryOp },
}

impl fmt::Display for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprKind::Ignored => write!(f, "_"),
            ExprKind::Const(p) => write!(f, "{}", p),
            ExprKind::Var { id, ty } => write!(f, "<{id}: {ty}>"),
            ExprKind::Range(a, Some(b)) => write!(f, "{}..{}", a, b),
            ExprKind::Range(a, None) => write!(f, "{}..", a),
            ExprKind::Enum(s) => write!(f, "({})", s.iter().format(" | ")),
            ExprKind::Flip(dn, up) => write!(f, "{} ! {}", dn, up),
            ExprKind::Concat(_, _, es) => write!(f, "[{:?}]", es),
            ExprKind::Unary { expr, op, .. } => write!(f, "{:?}({})", op, expr),
        }
    }
}

impl ExprKind {
    pub fn as_natural_number_range(&self, dir: Dir) -> Option<(u64, Option<u64>)> {
        match self {
            ExprKind::Ignored => Some((0, None)),
            ExprKind::Flip(dn, up) => match dir {
                Dir::Dn => dn.as_natural_number_range(Dir::Dn),
                Dir::Up => up.as_natural_number_range(Dir::Up),
            }

            ExprKind::Const(Value::Number(n)) if n.is_integer() && *n.numer() >= 0 => Some((*n.numer() as u64, Some(*n.numer() as u64 + 1))),

            ExprKind::Range(min, max) if min.is_integer() && *min.numer() >= 0 && max.is_some_and(|m| m.is_integer()) => {
                Some((*min.numer() as u64, max.map(|m| *m.numer() as u64)))
            }

            // TODO: should also check that the predicate is unrestricted
            _ => self.get_type().as_natural_number_range(),
        }
    }

    pub fn check_type(&self, expected: &Type) -> bool {
        match self {
            ExprKind::Ignored => true,
            ExprKind::Const(value) => expected.test(value),
            ExprKind::Range(lo, hi) => {
                match expected {
                    // TODO: Is it a type error if the range doesn't overlap, or just a predicate that never matches?
                    Type::Number(nt) => nt.contains(*lo) && hi.is_none_or(|h| nt.contains(h)),
                    _ => false
                }
            }
            ExprKind::Enum(set) => set.iter().all(|v| expected.test(v)),
            ExprKind::Var { ty, .. } | ExprKind::Unary { ty, .. } => ty.is_subtype_of(expected),
            ExprKind::Flip(dn, up) => {
                dn.check_type(expected) && up.check_type(expected)
            }
            ExprKind::Concat(_, _, elems) => {
                match *expected {
                    Type::Vector(w, ref elem_ty) => {
                        let elem_ty_check = elems.iter().all(|e| e.inner().check_type(elem_ty));
                        let len_check = elems.iter().map(|e| e.elem_count()).sum::<u32>() == w;
                        elem_ty_check && len_check
                    }
                    _ => false
                }
            }
        }
    }

    pub fn get_type(&self) -> ExprType {
        match *self {
            ExprKind::Ignored => ExprType::Ignored,
            ExprKind::Const(ref v) => ExprType::of_value(v),
            ExprKind::Range(lo, hi) => ExprType::Number(ExprNumberType { lo: Some(lo), hi, step: None }),
            ExprKind::Enum(ref e) => ExprType::Enum(e.clone()),
            ExprKind::Var { ref ty, .. } | ExprKind::Unary { ref ty, .. } => ty.clone(),
            ExprKind::Flip(ref dn, _) => dn.get_type(),
            ExprKind::Concat(ref elem_ty, len, _) => ExprType::Vector(len, Box::new(elem_ty.clone())),
        }
    }

    pub fn check_use_dn(&self, with_var: &mut impl FnMut(VarId) -> bool) -> bool {
        match *self {
            ExprKind::Ignored => false,
            ExprKind::Const(_) => true,
            ExprKind::Var { id, .. } => with_var(id),
            ExprKind::Range(..) | ExprKind::Enum(_) => false,
            ExprKind::Flip(ref d, _) => d.check_use_dn(with_var),
            ExprKind::Concat(_, _, ref concat_elems) => {
                let mut result = true;
                for elem in concat_elems {
                    result &= elem.inner().check_use_dn(with_var);
                }
                result
            }
            ExprKind::Unary { ref expr, .. } => expr.check_use_dn(with_var),
        }
    }

    pub fn check_use_up(&self, with_var: &mut impl FnMut(VarId) -> bool) -> bool {
        match *self {
            ExprKind::Ignored => false,
            ExprKind::Const(_) => false,
            ExprKind::Var { id, .. } => with_var(id),
            ExprKind::Range(..) | ExprKind::Enum(..) => false,
            ExprKind::Flip(_, ref u) => u.check_use_up(with_var),
            ExprKind::Concat(_, _, ref concat_elems) => {
                let mut result = false;
                for elem in concat_elems {
                    result |= elem.inner().check_use_up(with_var);
                }
                result
            }
            ExprKind::Unary { ref expr, .. } => expr.check_use_up(with_var),
        }
    }

    pub fn contains_var(&self) -> bool {
        match *self {
            ExprKind::Ignored => false,
            ExprKind::Const(_) => false,
            ExprKind::Var { .. } => true,
            ExprKind::Range(..) | ExprKind::Enum(..) => false,
            ExprKind::Flip(ref l, ref r) => l.contains_var() | r.contains_var(),
            ExprKind::Concat(_, _, ref concat_elems) => {
                concat_elems.iter().any(|elem| elem.inner().contains_var())
            }
            ExprKind::Unary { ref expr, .. } => expr.contains_var(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExprNumberType {
    pub lo: Option<Number>,
    pub hi: Option<Number>,
    pub step: Option<Number>,
}

impl From<&NumberType> for ExprNumberType {
    fn from(n: &NumberType) -> Self {
        ExprNumberType {
            lo: Some(n.scaled_min()),
            hi: Some(n.scaled_max()),
            step: Some(n.scale()),
        }
    }
}

impl ExprNumberType {
    fn add_const(&self, c: &Number) -> Option<ExprNumberType> {
        if let Some(step) = &self.step {
            if !(c / step).is_integer() {
                return None;
            }
        }
        Some(ExprNumberType {
            lo: self.lo.as_ref().map(|lo| lo + c),
            hi: self.hi.as_ref().map(|hi| hi + c),
            step: self.step,
        })
    }

    fn mul_const(&self, c: &Number) -> ExprNumberType {
        ExprNumberType {
            lo: self.lo.as_ref().map(|lo| lo * c),
            hi: self.hi.as_ref().map(|hi| hi * c),
            step: self.step.as_ref().map(|step| step * c),
        }
    }

    fn contains(&self, n: &Number) -> bool {
        self.lo.as_ref().is_none_or(|lo| lo <= n)
            && self.hi.as_ref().is_none_or(|hi| n <= hi)
            && self.step.as_ref().is_none_or(|step| (n / step).is_integer())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprType {
    /// The type of the Ignore expression `_`. No-op union with any type.
    Ignored,
    Enum(IndexSet<Value>),
    Number(ExprNumberType),
    Complex,
    Vector(u32, Box<ExprType>),
}

impl ExprType {
    pub fn of_value(v: &Value) -> Self {
        match *v {
            Value::Number(_) | Value::Symbol(_) => ExprType::Enum([v.clone()].into_iter().collect()),
            Value::Complex(_) => ExprType::Complex,
            Value::Vector(ref n) => {
                let len = n.len() as u32;
                let Ok(inner) = ExprType::union_iter(n.iter().map(ExprType::of_value)) else {
                    // Only a concatenation expression can create a vector
                    // containing different types, and it reports an error,
                    // so this should be unreachable.
                    panic!("Incompatible types in vector: {v:?}")
                };
                ExprType::Vector(len, Box::new(inner))
            }
        }
    }

    pub fn bit() -> ExprType {
        ExprType::Enum([0, 1].into_iter().map(|v| Value::Number(v.into())).collect())
    }

    pub fn bits(n: u32) -> ExprType {
        ExprType::Vector(n, Box::new(ExprType::bit()))
    }

    pub fn from_type(ty: &Type) -> ExprType {
        match ty {
            Type::Enum(e) => ExprType::Enum(e.clone()),
            Type::Number(n) => ExprType::Number(ExprNumberType::from(n)),
            Type::Complex => ExprType::Complex,
            Type::Vector(len, inner) => ExprType::Vector(*len, Box::new(ExprType::from_type(inner))),
        }
    }

    /// Test whether a value is a member of the type
    pub fn test(&self, v: &Value) -> bool {
        match (self, v) {
            (ExprType::Ignored, _) => true,
            (ExprType::Complex, Value::Number(_)) => true,
            (ExprType::Complex, Value::Complex(_)) => true,
            (ExprType::Number(nt), Value::Number(n)) => nt.contains(n),
            (ExprType::Enum(s), v) => s.contains(v),
            (ExprType::Vector(w, t), Value::Vector(v)) =>
                v.len() == *w as usize && v.iter().all(|x| t.test(x)),
            _ => false,
        }
    }

    pub fn is_subtype_of(&self, other: &Type) -> bool {
        match (self, other) {
            (ExprType::Ignored, _) => true,
            (ExprType::Enum(s1), Type::Enum(s2)) => s1.is_subset(s2),
            (ExprType::Enum(s1), other) => s1.iter().all(|v| other.test(v)),
            (ExprType::Number(n1), Type::Number(n2)) =>
                n1.step.is_none_or(|s| s == n2.scale()) && n1.lo.is_none_or(|l| n2.scaled_min() <= l) && n1.hi.is_none_or(|h| h <= n2.scaled_max()),
            (ExprType::Number { .. }, Type::Complex) => true,
            (ExprType::Complex, Type::Complex) => true,
            (ExprType::Vector(w1, t1), Type::Vector(w2, t2)) => w1 == w2 && t1.is_subtype_of(t2),
            _ => false,
        }
    }

    pub fn union_with(&mut self, other: Self) -> Result<(), IncompatibleTypes> {
        *self = ExprType::union(self.clone(), other)?;
        Ok(())
    }

    pub fn union(t1: ExprType, t2: ExprType) -> Result<ExprType, IncompatibleTypes> {
        use ExprType::*;
        match (t1, t2) {
            (Ignored, x) | (x, Ignored) => Ok(x),
            (Enum(a), Enum(b)) => Ok(Enum(a.union(&b).cloned().collect())),
            (Vector(n1, t1), Vector(n2, t2)) => {
                if n1 == n2 {
                    Ok(Vector(n1, Box::new(ExprType::union(*t1, *t2)?)))
                } else {
                    Err(IncompatibleTypes(*t1, *t2))
                }
            }
            (t1 @ Number(ExprNumberType { lo: lo1, hi: hi1, step: step1 }), t2 @ Number(ExprNumberType { lo: lo2, hi: hi2, step: step2 })) => {
                let lo = [lo1, lo2].into_iter().flatten().min();
                let hi = [hi1, hi2].into_iter().flatten().max();
                let step = match (step1, step2) {
                    (Some(step1), Some(step2)) if step1 == step2 => Some(step1),
                    (Some(_), Some(_)) => return Err(IncompatibleTypes(t1, t2)),
                    (Some(step), _) | (_, Some(step)) => Some(step),
                    (None, None) => None,
                };

                Ok(Number(ExprNumberType { lo, hi, step }))
            }
            (Number(ExprNumberType { lo, hi, step }), Enum(set)) | (Enum(set), Number(ExprNumberType { lo, hi, step })) => {
                set.iter().try_fold((lo, hi), |(lo, hi), v| {
                    if let Value::Number(nv) = v {
                        Ok((lo.map(|lo| min(lo, *nv)), hi.map(|hi| max(hi, *nv))))
                    } else {
                        Err(IncompatibleTypes(Enum(set.clone()), Number(ExprNumberType { lo, hi, step })))
                    }
                }).map(|(lo, hi)| Number(ExprNumberType { lo, hi, step }))
            }
            (Complex, Complex) => Ok(Complex),
            (t1, t2) => Err(IncompatibleTypes(t1, t2))
        }
    }

    pub fn union_iter<T: Iterator<Item=ExprType>>(mut i: T) -> Result<ExprType, IncompatibleTypes> {
        i.try_fold(ExprType::Ignored, ExprType::union)
    }

    pub fn as_natural_number_range(&self) -> Option<(u64, Option<u64>)> {
        match self {
            Self::Ignored => Some((0, None)),
            Self::Number(ExprNumberType { lo: Some(lo), hi, step }) if lo.is_integer() && !lo.is_negative() && hi.is_none_or(|hi| hi.is_integer() && !hi.is_negative()) && step.is_none_or(|s| s.is_one()) => {
                Some((*lo.numer() as u64, hi.map(|h| *h.numer() as u64)))
            }
            Self::Enum(v) if v.len() == 1 => {
                if let Value::Number(n) = v.iter().next().unwrap() && n.is_integer() && !n.is_negative() {
                    Some((*n.numer() as u64, Some(*n.numer() as u64 + 1)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprType::Enum(values) => values.iter().join(" | ").fmt(f),
            ExprType::Number(ExprNumberType { lo, hi, step }) => {
                if let Some(lo) = lo {
                    write!(f, "{lo}")?;
                }
                write!(f, "..")?;
                if let Some(hi) = hi {
                    write!(f, "{hi}")?;
                }
                if let Some(step) = step && !step.is_one() {
                    write!(f, " by {step}")?;
                }
                Ok(())
            }
            ExprType::Complex => write!(f, "<complex>"),
            ExprType::Vector(l, t) => write!(f, "[{t}; {l}]"),
            ExprType::Ignored => write!(f, "_"),
        }
    }
}

pub struct IncompatibleTypes(ExprType, ExprType);

impl IncompatibleTypes {
    pub fn report_at(self, dcx: &mut DiagnosticContext, span: Span) -> ErrorReported {
        dcx.report(Diagnostic::IncompatibleTypes { span, t1: self.0.to_string(), t2: self.1.to_string() })
    }
}

pub fn value(dcx: &mut DiagnosticContext, scope: &Scope, e: &ast::Expr) -> Result<ExprKind, ErrorReported> {
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

pub fn spanned_value(dcx: &mut DiagnosticContext, scope: &Scope, e: &ast::Expr) -> Result<Spanned<ExprKind>, ErrorReported> {
    let expr = value(dcx, scope, e)?;
    Ok(scope.span(e.span()).wrap(expr))
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
        Item::Leaf(LeafItem::Value(ExprKind::Const(v))) => {
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
    let has_named = node.has_named();
    let has_positional = node.has_positional();
    if node.fields.len() == 1 && !has_named {
        // Unwrap singleton tuple
        rexpr(dcx, scope, &node.fields[0].expr)
    } else if !has_named {
        Item::Positional(node.fields.iter().map(|f| rexpr(dcx, scope, &f.expr)).collect())
    } else if !has_positional {
        Item::Named(node.fields.iter().map(|f| (
            f.name.as_ref().map(|n| n.name.clone()).unwrap(),
            rexpr(dcx, scope, &f.expr)
        )).collect())
    } else {
        dcx.report(Diagnostic::TupleMixedNamedPositional { span: scope.span(node.span) }).into()
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

        ast::Expr::Value(node) => ExprKind::Const(Value::from_literal(&node.value)).into(),
        ast::Expr::Ignore(_) => ExprKind::Ignored.into(),
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
    let dn = node.dn.as_ref().map_or(Ok(ExprKind::Ignored), |dn| value(dcx, scope, dn));
    let up = node.up.as_ref().map_or(Ok(ExprKind::Ignored), |up| value(dcx, scope, up));

    let dn = try_item!(dn);
    let up = try_item!(up);

    // TODO: check type compatibility

    ExprKind::Flip(Box::new(dn), Box::new(up)).into()
}

fn resolve_expr_range(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprRange) -> Item {
    let min = constant::<Number>(dcx, scope, &node.lo);
    let max = node.hi.as_ref().map(|h| constant::<Number>(dcx, scope, h)).transpose();

    if let Some(step) = &node.step {
        return dcx.report(
            Diagnostic::RangeStepOnExpr { span: scope.span(step.span()) }
        ).into();
    }

    let min = try_item!(min);
    let max = try_item!(max);

    if let Some(max) = max && min > max {
        return dcx.report(
            Diagnostic::RangeOrder {
                min, min_span: scope.span(node.lo.span()),
                max, max_span: scope.span(node.hi.as_ref().unwrap().span()),
            }
        ).into();
    }

    ExprKind::Range(min, max).into()
}

fn resolve_expr_typed(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprTyped) -> Item {
    let expr = value(dcx, scope, &node.expr);
    let bound = type_expr(dcx, scope, &node.ty);

    let expr = try_item!(expr);
    let bound = try_item!(bound);
    let span = || scope.span(node.span);

    if expr.check_type(&bound) {
        expr.into()
    } else {
        dcx.report(Diagnostic::TypeConstraint { span: span(), found: expr.get_type().to_string(), bound: bound.to_string() }).into()
    }
}

fn resolve_expr_union(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprUnion) -> Item {
    let mut opts = IndexSet::new();
    let mut err = Ok(());
    for e in &node.items {
        let opt = value(dcx, scope, e);
        match opt {
            Ok(ExprKind::Const(c)) => { opts.insert(c); },
            Ok(ExprKind::Enum(cs)) => { opts.extend(cs); },
            Ok(other) => {
                err = Err(dcx.report(Diagnostic::ExpectedConst {
                    span: scope.span(e.span()),
                    found: format!("{other}"),
                    expected: "constant".to_string(),
                }));
            }
            Err(r) => {
                err = Err(r);
            }
        }
    }
    try_item!(err);

    ExprKind::Enum(opts).into()
}

fn resolve_expr_choose(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprChoose) -> Item {
    let e = value(dcx, scope, &node.e);
    let l_set = collect_or_err::<_, Vec<_>>(node.choices.iter().map(|&(ref le, _)| {
        constant::<Value>(dcx, scope, le)
    }));
    let r_set = collect_or_err::<_, Vec<_>>(node.choices.iter().map(|&(_, ref re)| {
        constant::<Value>(dcx, scope, re)
    }));

    let e = try_item!(e);
    let l_set = try_item!(l_set).into_boxed_slice();
    let r_set = try_item!(r_set).into_boxed_slice();

    let span = || scope.span(node.span);

    match e {
        ExprKind::Ignored => {
            ExprKind::Ignored
        }
        ExprKind::Const(c) => {
            let Some(v) = eval_choose(&c, &l_set, &r_set) else {
                return dcx.report(crate::Diagnostic::ChooseNotCovered { span: span(), missing: vec![c.to_string()] }).into();
            };

            ExprKind::Const(v)
        }
        ExprKind::Enum(vs) => {
            let Some(vs) = vs.iter().map(|v| eval_choose(&v, &l_set, &r_set)).collect::<Option<IndexSet<_>>>() else {
                return dcx.report(crate::Diagnostic::ChooseNotCovered { span: span(), missing: vs.iter().filter(|&v| !l_set.contains(v)).map(|v| v.to_string()).collect() }).into();
            };

            ExprKind::Enum(vs)
        }
        e => {
            let e_ty = e.get_type();
            let ExprType::Enum(e_set) = e_ty else {
                return dcx.report(crate::Diagnostic::ChooseInvalidType { span: scope.span(node.e.span()), found: e_ty.to_string() }).into();
            };

            if let Some(missing) = e_set.iter().filter(|&v| !l_set.contains(v)).at_least_one() {
                return dcx.report(crate::Diagnostic::ChooseNotCovered { span: span(), missing: missing.map(|e| e.to_string()).collect() }).into();
            }

            ExprKind::Unary { ty: ExprType::Enum(r_set.iter().cloned().collect()), expr: Box::new(e), op: UnaryOp::Mapping(l_set, r_set) }
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

    let mut elem_ty = ExprType::Ignored;
    let mut len: u32 = 0;
    let mut elems = ConcatBuilder::Const(Vec::new());

    let it = node.elems.iter().map(|&(width, ref e)| {
        let elem = value(dcx, scope, e)?;
        let span = || scope.span(node.span);

        if let Some(w) = width {
            match elem {
                ExprKind::Const(Value::Vector(vs)) if w as usize == vs.len() => {
                    for c in vs {
                        elem_ty.union_with(ExprType::of_value(&c)).map_err(|err| err.report_at(dcx, span()))?;
                        elems.push_const(c);
                        len += 1;
                    }
                }

                ExprKind::Ignored => {
                    len += w;
                    elems.push(ConcatElem::Slice(ExprKind::Ignored, w));
                }

                e if let ExprType::Vector(w1, ty) = e.get_type() && w == w1 => {
                    len += w;
                    elem_ty.union_with(*ty).map_err(|err| err.report_at(dcx, span()))?;

                    match e {
                        // Flatten nested slice
                        ExprKind::Concat(_, _, es) => {
                            elems.extend(es.into_iter());
                        }
                        e => {
                            elems.push(ConcatElem::Slice(e, w))
                        }
                    }
                }

                other => Err(dcx.report(Diagnostic::ExpectedVector {
                    span: span(),
                    found: other.to_string(),
                }))?
            }
            Ok(())
        } else {
            match elem {
                ExprKind::Const(c) => {
                    elem_ty.union_with(ExprType::of_value(&c)).map_err(|err| err.report_at(dcx, span()))?;
                    len += 1;
                    elems.push_const(c);
                }
                e => {
                    elem_ty.union_with(e.get_type()).map_err(|err| err.report_at(dcx, span()))?;
                    len += 1;
                    elems.push(ConcatElem::Elem(e))
                }
            }
            Ok(())
        }
    });
    try_item!(collect_or_err::<(), ()>(it));

    match elems {
        ConcatBuilder::Const(v) => ExprKind::Const(Value::Vector(v)),
        ConcatBuilder::Expr(e) => ExprKind::Concat(elem_ty, len, e),
    }.into()
}

fn resolve_expr_binary(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprBin) -> Item {
    let lhs = value(dcx, scope, &node.l);
    let rhs = value(dcx, scope, &node.r);

    let lhs = try_item!(lhs);
    let rhs = try_item!(rhs);

    let span = || scope.span(node.span);

    // Swap constant operand to the right
    let (expr, expr_span, op, val, val_span) = match (lhs, rhs) {
        (ExprKind::Ignored, _ ) | (_, ExprKind::Ignored) => return ExprKind::Ignored.into(),
        (l, ExprKind::Const(r)) => (l, node.l.span(), node.op, r, node.r.span()),
        (ExprKind::Const(l), r) => (r, node.r.span(), node.op.swap(), l, node.l.span()),
        (_, _) => {
            return dcx.report(Diagnostic::BinaryOneSideMustBeConst { span: span() }).into();
        }
    };

    let mut report_invalid_type = |expr: ExprKind, val: Value| {
        dcx.report(Diagnostic::BinaryInvalidType {
            span1: scope.span(expr_span),
            ty1: expr.to_string(),
            span2: scope.span(val_span),
            ty2: val.to_string(),
         }).into()
    };

    match (&expr, &val) {
        (ExprKind::Ignored, _) => ExprKind::Ignored.into(),
        (ExprKind::Const(l), r) => {
            if let Some(v) = eval_binary(l.clone(), op, r.clone()) {
                ExprKind::Const(v).into()
            } else {
                report_invalid_type(expr, val)
            }
        }
        (ExprKind::Range(lo, hi), Value::Number(r)) => {
            match eval_binary_on_range(Some(*lo), *hi, op, *r) {
                Ok((Some(lo), hi)) => ExprKind::Range(lo, hi).into(),
                Ok((None, _)) => todo!(),
                Err(()) => {
                    return dcx.report(Diagnostic::DivisionMustBeConst {
                        span: scope.span(expr_span),
                    }).into()
                }
            }
        }
        (ExprKind::Enum(vals), r) => {
            if let Some(vals) = vals.iter()
                .map(|v| eval_binary(v.clone(), op, r.clone()))
                .collect::<Option<IndexSet<Value>>>()
            {
                ExprKind::Enum(vals).into()
            } else {
                report_invalid_type(expr, val)
            }
        }
        (expr, Value::Number(v)) => {
            let ty = match (expr.get_type(), &val) {
                (ExprType::Number(nt), Value::Number(c)) => {
                    let mut check_scale = |n: Option<ExprNumberType>| {
                        n.ok_or_else(|| {
                            dcx.report(Diagnostic::OperandNotMultipleOfScale {
                                const_span: scope.span(val_span),
                                const_val: *c,
                                var_span: scope.span(expr_span),
                                var_scale: nt.step.unwrap(),
                            })
                        })
                    };

                    ExprType::Number(match op {
                        BinOp::Add => try_item!(check_scale(nt.add_const(c))),
                        BinOp::Sub => try_item!(check_scale(nt.add_const(&-c))),
                        BinOp::SubSwap => try_item!(check_scale(nt.mul_const(&Number::new(-1,1)).add_const(c))),

                        BinOp::Mul => nt.mul_const(c),
                        BinOp::Div => nt.mul_const(&c.recip()),
                        BinOp::DivSwap => return dcx.report(Diagnostic::DivisionMustBeConst {
                            span: scope.span(expr_span),
                        }).into(),
                    })
                }
                (ExprType::Enum(vs), Value::Number(c)) => {
                    let res: Result<IndexSet<Value>, ()> = vs.iter().map(|v| match v {
                        Value::Number(n) => Ok(Value::Number(op.eval(n, c))),
                        _ => Err(())
                    }).collect();

                    let Ok(res) = res else {
                        return dcx.report(
                            Diagnostic::BinaryInvalidType {
                                span1: scope.span(expr_span),
                                ty1: ExprType::Enum(vs).to_string(),
                                span2: scope.span(val_span),
                                ty2: val.to_string(),
                                }
                        ).into()
                    };

                    ExprType::Enum(res)
                }
                (ExprType::Complex, Value::Number(..)) | (ExprType::Number { .. }, Value::Complex(..)) | (ExprType::Complex, Value::Complex(..)) => ExprType::Complex,
                (expr_ty, _) => return dcx.report(
                    Diagnostic::BinaryInvalidType {
                        span1: scope.span(expr_span),
                        ty1: expr_ty.to_string(),
                        span2: scope.span(val_span),
                        ty2: val.to_string(),
                        }
                ).into()
            };

            ExprKind::Unary { ty, expr: Box::new(expr.clone()), op: UnaryOp::BinaryConstNumber(op, *v) }.into()
        }
        _ => report_invalid_type(expr, val)
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

#[track_caller]
#[cfg(test)]
pub fn test_expr_parse(e: &str) -> ExprKind {
    use crate::diagnostic::{DiagnosticContext, print_diagnostics};
    use crate::syntax::{parse_expr, SourceFile};
    use crate::core::Scope;

    let mut dcx = DiagnosticContext::new();

    let scope = Scope {
        file: Arc::new(SourceFile::new("<tests>".into(), "".into())),
        names: crate::core::primitive_fn::expr_prelude()
    };

    let ast = parse_expr(e);
    crate::diagnostic::report_parse_errors(&mut dcx, &scope.file, &ast);
    let v = value(&mut dcx, &scope, &ast);

    print_diagnostics(dcx.diagnostics());

    v.unwrap()
}

#[test]
fn test_number_const() {
    let two = test_expr_parse("2");
    assert_eq!(two, ExprKind::Const(Value::Number(Number::new(2, 1))));

    let decimal = test_expr_parse("1.023");
    assert_eq!(decimal, ExprKind::Const(Value::Number(Number::new(1023, 1000))));
}

#[test]
fn test_arithmetic() {
    use num_complex::Complex;

    assert_eq!(test_expr_parse("2 + 2"), ExprKind::Const(Value::Number(4.into())));
    assert_eq!(test_expr_parse("1 / 2"), ExprKind::Const(Value::Number(Number::new(1, 2))));

    let one_one_i = test_expr_parse("complex(1.0, 0.0) + complex(0, 1)");
    assert_eq!(one_one_i, ExprKind::Const(Value::Complex(Complex { re: 1.into(), im: 1.into() })));

    let two_two_i = test_expr_parse("complex(1, 1) * 2");
    assert_eq!(two_two_i, ExprKind::Const(Value::Complex(Complex { re: 2.into(), im: 2.into() })));

    assert_eq!(test_expr_parse("_ + _"), ExprKind::Ignored);
    assert_eq!(test_expr_parse("0 + _"), ExprKind::Ignored);
    assert_eq!(test_expr_parse("_ * 1"), ExprKind::Ignored);
    assert_eq!(test_expr_parse("_ * 1"), ExprKind::Ignored);
}

#[test]
fn test_arithmetic_range() {
    let range = test_expr_parse("0.0..5.0");
    assert_eq!(range, ExprKind::Range(0.into(), Some(5.into())));

    let sum = test_expr_parse("(0..10) + 5");
    assert_eq!(sum, ExprKind::Range(5.into(), Some(15.into())));

    let sub = test_expr_parse("(0..10) - 5");
    assert_eq!(sub, ExprKind::Range((-5).into(), Some(5.into())));

    let sub_swap = test_expr_parse("5 - (0..7)");
    assert_eq!(sub_swap, ExprKind::Range((-2).into(), Some(5.into())));

    let mul = test_expr_parse("(0..10) * 5");
    assert_eq!(mul, ExprKind::Range(0.into(), Some(50.into())));

    let mul = test_expr_parse("(0..10) * -5");
    assert_eq!(mul, ExprKind::Range((-50).into(), Some(0.into())));

    let div = test_expr_parse("(1..32) / 8");
    assert_eq!(div, ExprKind::Range(Number::new(1, 8), Some(4.into())));
}

#[test]
fn test_arithmetic_enum() {
    let range = test_expr_parse("0 | 1.5");
    assert_eq!(range, ExprKind::Enum([Number::new(0, 1), Number::new(3, 2)].into_iter().map(Value::Number).collect()));

    let sum = test_expr_parse("(0|1) + 2");
    assert_eq!(sum, ExprKind::Enum([Number::new(2, 1), Number::new(3, 1)].into_iter().map(Value::Number).collect()));

    let sub = test_expr_parse("(100 | 200) - 2");
    assert_eq!(sub, ExprKind::Enum([Number::new(98, 1), Number::new(198, 1)].into_iter().map(Value::Number).collect()));

    let sub_swap = test_expr_parse("5 - (1 | 7)");
    assert_eq!(sub_swap, ExprKind::Enum([Number::new(4, 1), Number::new(-2, 1)].into_iter().map(Value::Number).collect()));

    let mul = test_expr_parse("(-1 | 2) * 5");
    assert_eq!(mul, ExprKind::Enum([Number::new(-5, 1), Number::new(10, 1)].into_iter().map(Value::Number).collect()));

    let div = test_expr_parse("(64 | 32) / 8");
    assert_eq!(div, ExprKind::Enum([Number::new(8, 1), Number::new(4, 1)].into_iter().map(Value::Number).collect()));

    let div_swap = test_expr_parse("128 / (64 | 32)");
    assert_eq!(div_swap, ExprKind::Enum([Number::new(2, 1), Number::new(4, 1)].into_iter().map(Value::Number).collect()));
}

#[test]
fn exprs() {
    let choose = test_expr_parse("(#a | #b)[#a = #x, #b = #y]");
    assert_eq!(choose, ExprKind::Enum(["x".into(), "y".into()].into_iter().map(Value::Symbol).collect()));

    let fncall = test_expr_parse("((a) => a+3.0)(2.0)");
    assert_eq!(fncall, ExprKind::Const(Value::Number(5.into())));
}

#[test]
fn vec_const_fold() {
    assert_eq!(
        test_expr_parse("[1, 2, 2:[3, 1:[4]], 5]"),
        ExprKind::Const(Value::Vector(vec![1i64.into(), 2i64.into(), 3i64.into(), 4i64.into(), 5i64.into()])),
    );

    assert_eq!(
        test_expr_parse("[1, 2:[2, _], 3]"),
        test_expr_parse("[1, 2, _, 3]")
    );
}
