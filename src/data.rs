use std::collections::HashSet;
use std::cmp::{min, max};
use num_complex::Complex;
use std::fmt;

#[derive(PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Integer(i64),
    Complex(Complex<f64>),
    Symbol(String),
    Vector(Vec<Value>),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match *self {
            Value::Number(v) => Type::Number(v, v),
            Value::Complex(_) => Type::Complex,
            Value::Integer(v) => Type::Integer(v, v),
            Value::Symbol(ref v) => Type::Symbol(Some(v.clone()).into_iter().collect()),
            Value::Vector(ref n) => Type::Vector(n.len(),
                box n.first().map_or(Type::Bottom, Value::get_type)),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl ::std::hash::Hash for Value {
    fn hash<H>(&self, state: &mut H) where H: ::std::hash::Hasher {
        match *self {
            Value::Number(_) | Value::Complex(_) => (),
            Value::Integer(n) => n.hash(state),
            Value::Symbol(ref s) => s.hash(state),
            Value::Vector(ref n) => n.hash(state),
        }
    }
}

impl ::std::cmp::Eq for Value {}

/// A type represents a set of possible values
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Symbol(HashSet<String>),
    Integer(i64, i64),
    Vector(usize, Box<Type>),
    Number(f64, f64),
    Complex, // TODO: bounds?
    /// Type containing no values. No-op union with any type
    Bottom,
}

impl Type {
    pub fn union(t1: Type, t2: Type) -> Type {
        use self::Type::*;
        match (t1, t2) {
            (Bottom, x) | (x, Bottom) => x,
            (Symbol(a), Symbol(b)) => Symbol(a.union(&b).cloned().collect()),
            (Vector(n1, box t1), Vector(n2, box t2)) => {
                assert_eq!(n1, n2);
                Vector(n1, box Type::union(t1, t2))
            }
            (Integer(l1, h1), Integer(l2, h2)) => Integer(min(l1, l2), max(h1, h2)),
            (Number (l1, h1), Number (l2, h2)) => Number (f64::min(l1, l2), f64::max(h1, h2)),
            (Complex, Complex) => Complex,
            (a, b) => panic!("Incompatible types: {:?} and {:?}", a, b)
        }
    }

    pub fn union_iter<T: Iterator<Item=Type>>(i: T) -> Type {
        i.fold(Type::Bottom, Type::union)
    }

    /// Returns whether the passed value is a member of this type
    pub fn includes(&self, val: &Value) -> bool {
        match (self, val) {
            (&Type::Bottom, _) => false,
            (&Type::Symbol(ref _t), &Value::Symbol(ref _v)) => true, //TODO: t.contains(v),
            (&Type::Vector(len, ref t), &Value::Vector(ref v)) => {
                (v.len() == len) && v.iter().all(|i| t.includes(i))
            }
            (&Type::Integer(_lo, _hi), &Value::Integer(_v)) => true, //TODO: (v >= lo && v < hi),
            (&Type::Number(_lo, _hi), &Value::Number(_v)) => true, //TODO: (v >= lo && v < hi),
            (&Type::Complex, &Value::Complex(_)) => true,
            _ => false,
        }
    }

    pub fn includes_type(&self, other: &Type) -> bool {
        match (self, other) {
            (_, &Type::Bottom) => true,
            (&Type::Symbol(ref _v1), &Type::Symbol(ref _v2)) => true, //TODO: v1.is_superset(v2),
            (&Type::Vector(len1, ref t1), &Type::Vector(len2, ref t2)) => {
                (len1 == len2) && t1.includes_type(t2)
            }
            (&Type::Integer(_lo1, _hi1), &Type::Integer(_lo2, _hi2)) => true, //TODO: (lo2 >= lo1 && hi2 <= hi1),
            (&Type::Number(_lo1, _hi1), &Type::Number(_lo2, _hi2)) => true, //TODO: (lo2 >= lo1 && hi2 <= hi1),
            (&Type::Complex, &Type::Complex) => true,
            _ => false,
        }
    }
}

/// Flags indicating the directions data flows
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct DataMode {
    pub down: bool,
    pub up: bool,
}
