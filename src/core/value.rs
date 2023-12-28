use std::fmt;
use num_complex::Complex;
use num_traits::ToPrimitive;

use crate::{syntax::{Number, ast::Literal}, Type};

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum Value {
    Number(Number),
    Complex(Complex<Number>),
    Symbol(String),
    Vector(Vec<Value>),
}

impl Value {
    pub fn from_literal(value: &Literal) -> Value {
        match value {
            Literal::Number(n) => Value::Number(n.clone()),
            Literal::Symbol(s) => Value::Symbol(s.clone()),
            Literal::Hex(digits) => {
                Value::Vector(digits.iter().flat_map(|i| {
                    (0..4).rev().map(move |b| Value::from_bit((i & 1<<b) != 0))
                }).collect())
            },
            Literal::Bin(v) => {
                Value::Vector(v.iter().map(|&b| Value::from_bit(b)).collect())
            }
        }
    }

    pub fn from_bit(b: bool) -> Value {
        Value::Number((b as i64).into())
    }

    pub fn as_bit(&self) -> Option<bool> {
        match self {
            Value::Number(n) if n == &Number::from(0) => Some(false),
            Value::Number(n) if n == &Number::from(1) => Some(true),
            _ => None,
        }
    }

    pub fn from_byte(b: u8) -> Value {
        Value::Vector((0..8).rev().map(|i| Value::from_bit((b & 1<<i) != 0)).collect())
    }

    /// Get MSB-first bit vector as a u64
    pub fn as_bits(&self, n: usize) -> Option<u64> {
        assert!(n <= 64);
        match self {
            Value::Vector(v) if v.len() == n => {
                (0..n).rev().zip(v.iter()).try_fold(0, |byte, (i, e)| {
                    e.as_bit().map(|bit| byte | ((bit as u64)<<i))
                })
            }
            _ => None
        }
    }

    pub fn as_byte(&self) -> Option<u8> {
        self.as_bits(8).map(|x| x as u8)
    }

    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Value::Symbol(v) => Some(v.as_str()),
            _ => None
        }
    }

    pub fn get_type(&self) -> Type {
        match *self {
            Value::Number(v) => Type::NumberSet([v].into()),
            Value::Complex(_) => Type::Complex,
            Value::Symbol(ref v) => Type::Symbol([v.clone()].into()),
            Value::Vector(ref n) => {
                let len = n.len() as u32;
                let Ok(inner) = Type::union_iter(n.iter().map(Value::get_type)) else {
                    // Only a concatenation expression can create a vector
                    // containing different types, and it reports an error,
                    // so this should be unreachable.
                    panic!("Incompatible types in vector: {self:?}")
                };
                Type::Vector(len, Box::new(inner))
            }
        }
    }
}

#[test]
fn byte() {
    assert_eq!(Value::from_byte(0x9A).to_string(), "'10011010");
    for i in 0..=255 {
        assert_eq!(Value::from_byte(i).as_byte(), Some(i));
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Complex(c) => write!(f, "{}+{}i", c.re, c.im),
            Value::Symbol(ref s) => write!(f, "#{}", *s),
            Value::Vector(ref bits) if bits.iter().all(|x| x.as_bit().is_some()) => {
                write!(f, "'")?;
                for bit in bits {
                    write!(f, "{bit}")?;
                }
                Ok(())
            }
            Value::Vector(ref n) => write!(f, "{:?}", n),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl TryFrom<Value> for i64 {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(i) => i.to_i64().ok_or(()),
            _ => Err(())
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Number(value.into())
    }
}

impl TryFrom<&Value> for u32 {
    type Error = ();

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(i) => i.to_u32().ok_or(()),
            _ => Err(())
        }
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Number((value as i64).into())
    }
}

impl TryFrom<Value> for Number {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(i) => Ok(i),
            _ => Err(())
        }
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Self {
        Value::Number(n)
    }
}

impl TryFrom<Value> for Vec<Value> {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Vector(v) => Ok(v),
            _ => Err(())
        }
    }
}

