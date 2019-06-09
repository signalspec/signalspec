use std::fmt;
use num_complex::Complex;

#[derive(PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Integer(i64),
    Complex(Complex<f64>),
    Symbol(String),
    Vector(Vec<Value>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Complex(c) => write!(f, "{}+{}i", c.re, c.im),
            Value::Integer(n) => write!(f, "#{}", n),
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
