use std::cmp::{min, max};
use std::collections::BTreeSet;
use std::fmt::Display;
use std::ops::Range;

use indexmap::IndexSet;
use num_traits::One;

use crate::{Diagnostic, Value, DiagnosticContext};
use crate::diagnostic::{Span, ErrorReported};
use crate::syntax::Number as Rational;
use crate::tree::Tree;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumberType {
    scale: Rational,
    min: i64,
    max: i64,
}

pub enum NumberTypeError {
    Order,
    BoundsNotMultipleOfStep,
    StepIsZero,
}

impl NumberType {
    pub fn new(scale: Rational, min: i64, max: i64) -> NumberType {
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

    pub(crate) fn from_scaled(min: Rational, max: Rational, step: Rational) -> Result<NumberType, NumberTypeError> {
        if step == 0.into() {
            return Err(NumberTypeError::StepIsZero)
        }

        let min = min / step;
        let max = max / step;

        if !min.is_integer() || !max.is_integer() {
            return Err(NumberTypeError::BoundsNotMultipleOfStep);
        }

        if min.numer() > max.numer() {
            return Err(NumberTypeError::Order);
        }

        Ok(NumberType { scale: step, min: *min.numer(), max: *max.numer() })
    }

    pub fn is_integer(&self) -> bool {
        self.scale == Rational::new(1, 1)
    }

    pub fn scale(&self) -> Rational { self.scale }
    pub fn min(&self) -> i64 { self.min }
    pub fn max(&self) -> i64 { self.max }
    pub fn range(&self) -> Range<i64> { self.min..self.max }

    pub fn iter(&self) -> impl Iterator<Item = Rational> {
        let scale = self.scale;
        (self.min..self.max).map(move |v| Rational::from(v) * scale)
    }

    pub fn scaled_min(&self) -> Rational { self.scale * Rational::new(self.min, 1) }
    pub fn scaled_max(&self) -> Rational { self.scale * Rational::new(self.max, 1) }

    pub fn contains(&self, n: Rational) -> bool {
        let v = n / self.scale;
        *v.denom() == 1 && self.range().contains(v.numer())
    }

    pub(crate) fn add(&self, c: Rational) -> Option<NumberType> {
        let v = c / self.scale;
        if v.is_integer() {
            Some(NumberType { min: self.min + v.numer(), max: self.max + v.numer(), ..*self })
        } else {
            None
        }
    }
    pub(crate) fn mul(&self, c: Rational) -> NumberType {
        NumberType { scale: self.scale*c, ..*self }
    }
}

/// A type represents a set of possible values
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Symbol(IndexSet<String>),
    Number(NumberType),
    NumberSet(BTreeSet<Rational>),
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
            (Symbol(a), Symbol(b)) => Ok(Symbol(a.union(&b).cloned().collect())),
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
            (NumberSet(s1), NumberSet(s2)) => {
                Ok(NumberSet(s1.union(&s2).cloned().collect()))
            }
            (NumberSet(ref c), Number(ref n)) | (Number(ref n), NumberSet(ref c)) => {
                c.iter().try_fold(n.clone(), |n, v| {
                    let v = v / n.scale();
                    if v.is_integer() {
                        Ok(NumberType::new(n.scale(), min(n.min(), *v.numer()), max(n.max(), *v.numer())))
                    } else {
                        Err(IncompatibleTypes(NumberSet(c.clone()), Number(n)))
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
            (Type::NumberSet(s), Value::Number(n)) => s.contains(n),
            (Type::Symbol(s), Value::Symbol(v)) =>
                s.contains(v),
            (Type::Vector(w, t), Value::Vector(v)) =>
                v.len() == *w as usize && v.iter().all(|x| t.test(x)),
            _ => false,
        }
    }

    pub fn is_subtype(&self, other: &Type) -> bool {
        match (self, other) {
            (_, Type::Ignored) => true,
            (Type::Ignored, _) => true,
            (Type::Symbol(s1), Type::Symbol(s2)) => s1.is_subset(s2),
            (Type::Number(n1), Type::Number(n2)) =>
                n1.scale() == n2.scale() && n2.min() <= n1.min() && n1.max() <= n2.max(),
            (Type::NumberSet(s1), Type::NumberSet(s2)) => s1.is_subset(s2),
            (Type::NumberSet(s), Type::Number(n)) => s.iter().all(|v| n.contains(*v)),
            (Type::Number(n), Type::NumberSet(s)) => n.iter().all(|v| s.contains(&v)),
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
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Symbol(symbols) => {
                for (i, s) in symbols.iter().enumerate() {
                    if i != 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "#{s}")?;
                }
                Ok(())
            }
            Type::Number(n) => {
                if n.scale().is_one() {
                    write!(f, "{lo}..{hi}", lo = n.scaled_min(), hi = n.scaled_max())
                } else {
                    write!(f, "{lo}..{hi} by {scale}", lo = n.scaled_min(), hi = n.scaled_max(), scale = n.scale())
                }
            }
            Type::NumberSet(set) => {
                for (i, s) in set.iter().enumerate() {
                    if i != 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{s}")?;
                }
                Ok(())
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
