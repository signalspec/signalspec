use std::cmp::{min, max};
use std::fmt::Display;

use indexmap::IndexSet;
use crate::syntax::{Value, Number};
use crate::tree::Tree;

impl Value {
    pub fn get_type(&self) -> Type {
        match *self {
            Value::Number(v) => Type::Number(v, v),
            Value::Complex(_) => Type::Complex,
            Value::Symbol(ref v) => Type::Symbol(Some(v.clone()).into_iter().collect()),
            Value::Vector(ref n) => Type::Vector(n.len() as u32,
                Box::new(n.first().map_or(Type::Ignored, Value::get_type))),
        }
    }
}

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
    pub fn union(t1: Type, t2: Type) -> Type {
        use self::Type::*;
        match (t1, t2) {
            (Ignored, x) | (x, Ignored) => x,
            (Symbol(a), Symbol(b)) => Symbol(a.union(&b).cloned().collect()),
            (Vector(n1, t1), Vector(n2, t2)) => {
                assert_eq!(n1, n2);
                Vector(n1, Box::new(Type::union(*t1, *t2)))
            }
            (Number (l1, h1), Number (l2, h2)) => Number (min(l1, l2), max(h1, h2)),
            (Complex, Complex) => Complex,
            (a, b) => panic!("Incompatible types: {:?} and {:?}", a, b)
        }
    }

    pub fn union_iter<T: Iterator<Item=Type>>(i: T) -> Type {
        i.fold(Type::Ignored, Type::union)
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

pub type TypeTree = Tree<Type>;

/// Flags indicating the directions data flows
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct DataMode {
    pub down: bool,
    pub up: bool,
}

impl DataMode {
    pub fn constrain(&mut self, other: DataMode) {
        self.up &= other.up;
        self.down |= other.down;
    }
}
