pub use self::Type::*;
use eval::DataMode;
use std::collections::HashSet;

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
