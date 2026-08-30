use std::fmt::Display;
use std::ops::Range;

use indexmap::IndexSet;
use itertools::Itertools;
use num_traits::One;

use crate::Value;
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

    #[expect(unused)]
    pub(crate) fn add(&self, c: Number) -> Option<NumberType> {
        if let Some(v) = as_integer(c / self.scale()) {
            Some(NumberType { min: self.min + v, max: self.max + v, ..*self })
        } else {
            None
        }
    }

    #[expect(unused)]
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
}

impl Type {
    pub fn bit() -> Type {
        Type::Enum([Value::Number(0.into()), Value::Number(1.into())].into_iter().collect())
    }

    pub fn bits(width: u32) -> Type {
        Type::Vector(width, Box::new(Type::bit()))
    }

    /// Test whether a value is a member of the type
    pub fn test(&self, v: &Value) -> bool {
        match (self, v) {
            (Type::Complex, Value::Number(_)) => true,
            (Type::Complex, Value::Complex(_)) => true,
            (Type::Number(nt), Value::Number(n)) => nt.contains(*n),
            (Type::Enum(s), v) => s.contains(v),
            (Type::Vector(w, t), Value::Vector(v)) =>
                v.len() == *w as usize && v.iter().all(|x| t.test(x)),
            _ => false,
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
        }
    }
}

pub type TypeTree = Tree<Type>;
