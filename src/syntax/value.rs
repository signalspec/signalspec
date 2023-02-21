use std::fmt;
use num_complex::Complex;
use num_traits::ToPrimitive;

pub type Number = num_rational::Rational64;

#[derive(PartialEq, Eq, Clone, Hash)]
pub enum Value {
    Number(Number),
    Complex(Complex<Number>),
    Symbol(String),
    Vector(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Complex(c) => write!(f, "{}+{}i", c.re, c.im),
            Value::Symbol(ref s) => write!(f, "#{}", *s),
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

impl TryFrom<&Value> for u8 {
    type Error = ();

    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::Number(i) => i.to_u8().ok_or(()),
            _ => Err(())
        }
    }
}

impl From<u8> for Value {
    fn from(value: u8) -> Self {
        Value::Number((value as i64).into())
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

impl TryFrom<Value> for Vec<Value> {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Vector(v) => Ok(v),
            _ => Err(())
        }
    }
}

