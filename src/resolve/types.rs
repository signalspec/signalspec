pub use self::Type::*;
use eval::DataMode;
use std::collections::HashSet;
use std::cmp::{min, max};

#[derive(Debug, PartialEq, Clone)]
/// A type represents a set of possible values
pub enum Type {
    Symbol(HashSet<String>),
    Integer(i64, i64),
    Vector(usize, Box<Type>),
    Number(f64, f64),
    /// Type containing no values. No-op union with any type
    Bottom,
}

impl Type {
    pub fn union(t1: Type, t2: Type) -> Type {
        match (t1, t2) {
            (Bottom, x) | (x, Bottom) => x,
            (Symbol(a), Symbol(b)) => Symbol(a.union(&b).cloned().collect()),
            (Vector(n1, box t1), Vector(n2, box t2)) => {
                assert_eq!(n1, n2);
                Vector(n1, box Type::union(t1, t2))
            }
            (Integer(l1, h1), Integer(l2, h2)) => Integer(min(l1, l2), max(h1, h2)),
            (Number (l1, h1), Number (l2, h2)) => Number (f64::min(l1, l2), f64::max(h1, h2)),
            (a, b) => panic!("Incompatible types: {:?} and {:?}", a, b)
        }
    }

    pub fn union_iter<T: Iterator<Item=Type>>(i: T) -> Type {
        i.fold(Type::Bottom, Type::union)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Shape {
    Tup(Vec<Shape>),
    Val(Type, DataMode),
}

// TODO: should be fully empty, not a no-direction member
pub static NULL_SHAPE: Shape = Shape::Val(Bottom, DataMode { down: false, up: false });

impl Shape {
    pub fn data_mode(&self) -> DataMode {
        match *self {
            Shape::Val(_, mode) => mode,
            Shape::Tup(ref items) => items.iter().fold(
                DataMode { down: false, up: false },
                |am, b| {
                    let bm = b.data_mode();
                    DataMode { down: am.down || bm.down, up: am.up || bm.up  }
                }
            ),
        }
    }
}
