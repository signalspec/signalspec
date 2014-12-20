pub use self::Type::*;

#[deriving(Copy, Show, PartialEq, Clone)]
/// A type represents a set of possible values
pub enum Type {
    Symbol, // TODO: include variants?
    Integer, // TODO: range
    Bits(uint),
    Vector(uint), //TODO: element type
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
pub fn common_all<T:Iterator<Type>>(l: T) -> Option<Type> {
    l.fold(Some(Bottom), |opt_a, b|{ opt_a.and_then(|a| common(a, b)) })
}

#[deriving(Clone, Show, PartialEq)]
pub enum Shape {
    Unknown(bool, bool),
    Tup(Vec<Shape>),
    Val(Type, bool, bool),
}
