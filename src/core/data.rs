use std::cmp::{min, max};
use std::fmt::Display;
use std::ops::Range;

use indexmap::IndexSet;
use itertools::Itertools;
use num_traits::{One, Signed};

use crate::{Diagnostic, Value, DiagnosticContext};
use crate::diagnostic::{Span, ErrorReported};
use crate::syntax::Number;
use crate::tree::Tree;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumberType {
    scale: Number,
    min: i64,
    max: i64,
}

pub enum NumberTypeError {
    Order,
    BoundsNotMultipleOfStep,
    StepIsZero,
}

fn as_integer(n: Number) -> Option<i64> {
    n.is_integer().then_some(*n.numer())
}

impl NumberType {
    pub fn new(scale: Number, min: i64, max: i64) -> NumberType {
        assert!(min < max);
        assert!(scale != 0.into());
        NumberType { scale, min, max }
    }

    pub fn integer(min: i64, max: i64) -> NumberType {
        Self::new(1.into(), min, max)
    }

    pub fn bit() -> NumberType {
        Self::integer(0, 2)
    }

    pub(crate) fn from_scaled(min: Number, max: Number, scale: Number) -> Result<NumberType, NumberTypeError> {
        if scale == 0.into() {
            return Err(NumberTypeError::StepIsZero)
        }

        let (Some(min), Some(max)) = (as_integer(min / scale), as_integer(max / scale)) else {
            return Err(NumberTypeError::BoundsNotMultipleOfStep);
        };

        if min > max {
            return Err(NumberTypeError::Order);
        }

        Ok(NumberType { scale, min, max })
    }

    pub fn is_integer(&self) -> bool {
        self.scale == Number::new(1, 1)
    }

    pub fn scale(&self) -> Number { self.scale }
    pub fn min(&self) -> i64 { self.min }
    pub fn max(&self) -> i64 { self.max }
    pub fn range(&self) -> Range<i64> { self.min..self.max }

    pub fn iter(&self) -> impl Iterator<Item = Number> + use<> {
        let scale = self.scale;
        (self.min..self.max).map(move |v| Number::from(v) * scale)
    }

    pub fn scaled_min(&self) -> Number { self.scale * Number::new(self.min, 1) }
    pub fn scaled_max(&self) -> Number { self.scale * Number::new(self.max, 1) }

    pub fn contains(&self, n: Number) -> bool {
        as_integer(n / self.scale).is_some_and(|v| self.range().contains(&v))
    }

    pub(crate) fn add(&self, c: Number) -> Option<NumberType> {
        if let Some(v) = as_integer(c / self.scale()) {
            Some(NumberType { min: self.min + v, max: self.max + v, ..*self })
        } else {
            None
        }
    }
    pub(crate) fn mul(&self, c: Number) -> NumberType {
        NumberType { scale: self.scale*c, ..*self }
    }
}

/// A type represents a set of possible values
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Enum(IndexSet<Value>),
    Number(NumberType),
    Complex, // TODO: bounds?
    Vector(u32, Box<Type>),

    /// The type of the Ignore expression `_`. No-op union with any type.
    Ignored,
}

impl Type {
    pub fn bit() -> Type {
        Type::Number(NumberType::bit())
    }

    pub fn bits(width: u32) -> Type {
        Type::Vector(width, Box::new(Type::bit()))
    }

    pub fn union_with(&mut self, other: Self) -> Result<(), IncompatibleTypes> {
        *self = Type::union(self.clone(), other)?;
        Ok(())
    }

    pub fn union(t1: Type, t2: Type) -> Result<Type, IncompatibleTypes> {
        use self::Type::*;
        match (t1, t2) {
            (Ignored, x) | (x, Ignored) => Ok(x),
            (Enum(a), Enum(b)) => Ok(Enum(a.union(&b).cloned().collect())),
            (Vector(n1, t1), Vector(n2, t2)) => {
                if n1 == n2 {
                    Ok(Vector(n1, Box::new(Type::union(*t1, *t2)?)))
                } else {
                    Err(IncompatibleTypes(*t1, *t2))
                }
            }
            (Number(n1), Number(n2)) => {
                if n1.scale() == n2.scale() {
                    Ok(Number(NumberType::new(n1.scale(), min(n1.min(), n2.min()), max(n1.max(), n2.max()))))
                } else {
                    Err(IncompatibleTypes(Number(n1), Number(n2)))
                }
            }
            (Number(range), Enum(set)) | (Enum(set), Number(range)) => {
                set.iter().try_fold(range.clone(), |n, v| {
                    if let Value::Number(nv) = v {
                        if let Some(v) = as_integer(nv / n.scale()) {
                            Ok(NumberType::new(n.scale(), min(n.min(), v), max(n.max(), v)))
                        } else {
                            Err(IncompatibleTypes(Enum(set.clone()), Number(n)))
                        }
                    } else {
                        Err(IncompatibleTypes(Enum(set.clone()), Number(n)))
                    }
                }).map(Type::Number)
            }
            (Complex, Complex) => Ok(Complex),
            (t1, t2) => Err(IncompatibleTypes(t1, t2))
        }
    }

    pub fn union_iter<T: Iterator<Item=Type>>(mut i: T) -> Result<Type, IncompatibleTypes> {
        i.try_fold(Type::Ignored, Type::union)
    }

    /// Test whether a value is a member of the type
    pub fn test(&self, v: &Value) -> bool {
        match (self, v) {
            (Type::Ignored, _) => true,
            (Type::Complex, Value::Number(_)) => true,
            (Type::Complex, Value::Complex(_)) => true,
            (Type::Number(nt), Value::Number(n)) => nt.contains(*n),
            (Type::Enum(s), v) => s.contains(v),
            (Type::Vector(w, t), Value::Vector(v)) =>
                v.len() == *w as usize && v.iter().all(|x| t.test(x)),
            _ => false,
        }
    }

    pub fn is_subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (_, Type::Ignored) => true,
            (Type::Ignored, _) => true,
            (Type::Enum(s1), Type::Enum(s2)) => s1.is_subset(s2),
            (Type::Enum(s1), other) => s1.iter().all(|v| other.test(v)),
            (Type::Number(range), Type::Enum(set)) => range.iter().all(|v| set.contains(&Value::Number(v))),
            (Type::Number(n1), Type::Number(n2)) =>
                n1.scale() == n2.scale() && n2.min() <= n1.min() && n1.max() <= n2.max(),
            (Type::Number { .. }, Type::Complex) => true,
            (Type::Complex, Type::Complex) => true,
            (Type::Vector(w1, t1), Type::Vector(w2, t2)) => w1 == w2 && t1.is_subtype(t2),
            _ => false,
        }
    }

    pub fn bound(self, constraint: Type) -> Result<Type, (Type, Type)> {
        match (self, constraint) {
            (Type::Ignored, other) => Ok(other),
            (Type::Vector(w, t), Type::Vector(wb, tb)) if w == wb =>
                Ok(Type::Vector(w, Box::new(t.bound(*tb)?))),
            (a, b) if a.is_subtype(&b) => Ok(a),
            (a, b) => Err((a, b)),
        }
    }

    pub fn is_natural_number(&self) -> bool {
        match self {
            Type::Number(nt) => nt.is_integer() && !nt.min().is_negative(),
            Type::Enum(s) => s.iter().all(|v| matches!(v, Value::Number(n) if n.is_integer() && !n.is_negative())),
            Type::Ignored => true,
            _ => false
        }
    }

    pub fn as_natural_number_range(&self) -> Option<(u64, Option<u64>)> {
        match self {
            Type::Ignored => Some((0, None)),
            Type::Number(nt) if nt.is_integer() && !nt.min().is_negative() => Some((nt.min() as u64, Some(nt.max() as u64))),
            Type::Enum(v) if v.len() == 1 => {
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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Enum(values) => values.iter().join(" | ").fmt(f),
            Type::Number(n) => {
                if n.scale().is_one() {
                    write!(f, "{lo}..{hi}", lo = n.scaled_min(), hi = n.scaled_max())
                } else {
                    write!(f, "{lo}..{hi} by {scale}", lo = n.scaled_min(), hi = n.scaled_max(), scale = n.scale())
                }
            }
            Type::Complex => write!(f, "<complex>"),
            Type::Vector(l, t) => write!(f, "[{t}; {l}]"),
            Type::Ignored => write!(f, "_"),
        }
    }
}

pub struct IncompatibleTypes(Type, Type);

impl IncompatibleTypes {
    pub fn report_at(self, dcx: &mut DiagnosticContext, span: Span) -> ErrorReported {
        dcx.report(Diagnostic::IncompatibleTypes { span, t1: self.0, t2: self.1 })
    }
}

pub type TypeTree = Tree<Type>;

#[track_caller]
#[cfg(test)]
pub fn test_type_parse(e: &str) -> Type {
    use std::sync::Arc;
    use crate::diagnostic::{DiagnosticContext, print_diagnostics};
    use crate::syntax::{parse_expr, SourceFile};
    use crate::core::Scope;

    let mut dcx = DiagnosticContext::new();

    let scope = Scope {
        file: Arc::new(SourceFile::new("<tests>".into(), "".into())),
        names: crate::core::primitive_fn::expr_prelude()
    };

    let ast = parse_expr(e).unwrap();
    crate::diagnostic::report_parse_errors(&mut dcx, &scope.file, &ast);
    let t = crate::core::resolve::type_expr::type_expr(&mut dcx, &scope, &ast);

    print_diagnostics(dcx.diagnostics());

    t.unwrap()
}

#[test]
fn test_type_expr() {
    let range1 = test_type_parse("0.0..2.0 by 0.1");
    assert_eq!(range1, Type::Number(NumberType::new(Number::new(1, 10), 0, 20)));
}

#[test]
fn test_subtype() {
    assert!(test_type_parse("0|1").is_subtype(&Type::bit()));
    assert!(Type::bit().is_subtype(&test_type_parse("0|1")));

    assert!(test_type_parse("0..10").is_subtype(&test_type_parse("0..100")));
    assert!(!test_type_parse("0..100").is_subtype(&test_type_parse("0..10")));

    assert!(test_type_parse("#a|#b|#c").is_subtype(&test_type_parse("#a|#b|#c|#d")));
    assert!(!test_type_parse("#a|#b|#c|#d").is_subtype(&test_type_parse("#a|#b|#c")));
}
