use std::cmp::{min, max};
use std::fmt::Display;

use indexmap::IndexSet;

use crate::{DiagnosticHandler, Diagnostic, Value};
use crate::diagnostic::{Span, ErrorReported};
use crate::syntax::Number;
use crate::tree::Tree;

/// A type represents a set of possible values
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Symbol(IndexSet<String>),
    Number(Number, Number),
    Complex, // TODO: bounds?
    Vector(u32, Box<Type>),

    /// The type of the Ignore expression `_`. No-op union with any type.
    Ignored,
}

impl Type {
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
            (Number (l1, h1), Number (l2, h2)) => Ok(Number (min(l1, l2), max(h1, h2))),
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
            (Type::Number(lo, hi), Value::Number(n)) =>
                lo <= n && n <= hi,
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
            (Type::Number(lo1, hi1), Type::Number(lo2, hi2)) =>
                lo2 <= lo1 && hi1 <= hi2,
            (Type::Number(_, _), Type::Complex) => true,
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
            },
            Type::Number(lo, hi) => {
                if lo == hi {
                    write!(f, "{lo}")
                } else {
                    write!(f, "{lo}..{hi}")
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
    pub fn report_at(self, ctx: &dyn DiagnosticHandler, span: Span) -> ErrorReported {
        ctx.report(Diagnostic::IncompatibleTypes { span, t1: self.0, t2: self.1 })
    }
}

pub type TypeTree = Tree<Type>;
