use std::collections::HashSet;
use std::cmp::{min, max};

use crate::syntax::{Value, Number};
use crate::tree::Tree;

impl Value {
    pub fn get_type(&self) -> Type {
        match *self {
            Value::Number(v) => Type::Number(v, v),
            Value::Complex(_) => Type::Complex,
            Value::Symbol(ref v) => Type::Symbol(Some(v.clone()).into_iter().collect()),
            Value::Vector(ref n) => Type::Vector(n.len() as u32,
                Box::new(n.first().map_or(Type::Bottom, Value::get_type))),
        }
    }
}

/// A type represents a set of possible values
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Symbol(HashSet<String>),
    Number(Number, Number),
    Complex, // TODO: bounds?
    Vector(u32, Box<Type>),
    /// Type containing no values. No-op union with any type
    Bottom,
}

impl Type {
    pub fn union(t1: Type, t2: Type) -> Type {
        use self::Type::*;
        match (t1, t2) {
            (Bottom, x) | (x, Bottom) => x,
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
        i.fold(Type::Bottom, Type::union)
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
