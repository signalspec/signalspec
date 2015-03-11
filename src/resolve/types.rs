pub use self::Type::*;

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
    Unknown(bool, bool),
    Tup(Vec<Shape>),
    Val(Type, bool, bool),
}

impl Shape {
    pub fn contains_direction(&self) -> (bool, bool) {
        match *self {
            Shape::Unknown(d, u) | Shape::Val(_, d, u) => (d, u),
            Shape::Tup(ref x) => {
                x.iter().map(Shape::contains_direction)
                        .fold((false, false), |(d1,u1),(d2,u2)| (d1|d2, u1|u2))
            }
        }
    }
}

#[derive(Debug)]
pub struct SignalInfo {
    pub downwards: Shape,
    pub upwards: Shape,
}

#[test]
fn shape_contains_direction() {
    assert_eq!(Shape::Tup(vec![Shape::Unknown(true, false)])
        .contains_direction(), (true, false));

    assert_eq!(Shape::Tup(vec![
            Shape::Unknown(true, false),
            Shape::Unknown(false, true)
        ]).contains_direction(), (true, true));

    assert_eq!(Shape::Tup(vec![
            Shape::Unknown(false, false),
            Shape::Unknown(false, true)
        ]).contains_direction(), (false, true));
}
