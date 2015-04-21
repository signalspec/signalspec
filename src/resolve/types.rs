pub use self::Type::*;
use eval::DataMode;

#[derive(Copy, Debug, PartialEq, Clone)]
/// A type represents a set of possible values
pub enum Type {
    Symbol, // TODO: include variants?
    Integer, // TODO: range
    Vector(usize), //TODO: element type
    Number,
    /// Type containing no values. No-op union with any type
    Bottom,
}

/// Return the union of the two types, if it exists
pub fn common(a: Type, b: Type) -> Option<Type>{
    match (a, b) {
        (Bottom, x) | (x, Bottom) => Some(x),
        (a, b) if a == b => Some(a),
        _ => None
    }
}

/// Return the intersection of a vector of types, or None if the types don't intersect
pub fn common_all<T:Iterator<Item=Type>>(l: T) -> Option<Type> {
    l.fold(Some(Bottom), |opt_a, b|{ opt_a.and_then(|a| common(a, b)) })
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
