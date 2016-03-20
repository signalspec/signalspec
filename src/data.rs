use std::collections::HashSet;
use std::cmp::{min, max};
use std::slice;
use ref_slice::{ref_slice, mut_ref_slice};
use std::fmt;

#[derive(PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Integer(i64),
    Symbol(String),
    Vector(Vec<Value>),
}

impl Value {
    pub fn get_type(&self) -> Type {
        match *self {
            Value::Number(v) => Type::Number(v, v),
            Value::Integer(v) => Type::Integer(v, v),
            Value::Symbol(ref v) => Type::Symbol(Some(v.clone()).into_iter().collect()),
            Value::Vector(ref n) => Type::Vector(n.len(),
                box n.first().map_or(Type::Bottom, Value::get_type)),
        }
    }

    pub fn matches(&self, other: &Value) -> bool {
        *self == *other
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Number(n) => write!(f, "{}", n),
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

/// A type represents a set of possible values
#[derive(Debug, PartialEq, Clone)]
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
            (&Type::Integer(_lo, _hi), &Value::Integer(_v)) => true, //TODO: (v >= lo && v <= hi),
            (&Type::Number(_lo, _hi), &Value::Number(_v)) => true, //TODO: (v >= lo && v <= hi),
            _ => false,
        }
    }

    pub fn includes_type(&self, other: &Type) -> bool {
        match (self, other) {
            (&Type::Bottom, &Type::Bottom) => true,
            (&Type::Symbol(ref _v1), &Type::Symbol(ref _v2)) => true, //TODO: v1.is_superset(v2),
            (&Type::Vector(len1, ref t1), &Type::Vector(len2, ref t2)) => {
                (len1 == len2) && t1.includes_type(t2)
            }
            (&Type::Integer(_lo1, _hi1), &Type::Integer(_lo2, _hi2)) => true, //TODO: (lo2 >= lo1 && hi2 <= hi1),
            (&Type::Number(_lo1, _hi1), &Type::Number(_lo2, _hi2)) => true, //TODO: (lo2 >= lo1 && hi2 <= hi1),
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

/// Representation of token alphabet between state machine layers of abstraction.
/// Produced from an Interface by name resolution and direction inference.
#[derive(Clone, Debug, PartialEq)]
pub struct Shape {
    pub variants: Vec<ShapeVariant>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShapeVariant {
    pub data: ShapeData,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ShapeData {
    Tup(Vec<ShapeData>),
    Val(Type, DataMode),
    Const(Value),
}

impl Shape {
    /// Produces a shape with no variants
    pub fn null() -> Shape {
        Shape { variants: vec![] }
    }

    /// Produces a shape for a stream of bytes in the specified direction
    pub fn bytes(dir: DataMode) -> Shape {
        Shape { variants: vec![
            ShapeVariant {
                data: ShapeData::Val(Type::Integer(0, 255), dir),
            }
        ]}
    }

    /// If the shape represents a stream of bytes, returns Some(data direction)
    pub fn match_bytes(&self) -> Option<DataMode> {
        if self.variants.len() != 1 {
            return None;
        }

        match self.variants[0] {
            ShapeVariant { data: ShapeData::Val(Type::Integer(0, 255), dir), .. } => Some(dir),
            _ => None,
        }
    }

    pub fn data_mode(&self) -> DataMode {
        self.variants.iter().map(ShapeVariant::data_mode).fold(
            DataMode { down: false, up: false },
            |am, bm| { DataMode { down: am.down || bm.down, up: am.up || bm.up  }}
        )
    }
}

impl ShapeVariant {
    pub fn data_mode(&self) -> DataMode {
        self.values().fold(
            DataMode { down: false, up: false },
            |am, (_, bm)| { DataMode { down: am.down || bm.down, up: am.up || bm.up  }}
        )
    }

    /// Iterator that produces (&Type, &DataMode) pairs in an in-order traversal over the leaf
    /// nodes of the ShapeData tree.
    pub fn values(&self) -> ShapeValIterator {
        ShapeValIterator { stack: vec![ref_slice(&self.data).iter()] }
    }

    /// Iterator that produces (&mut Type, &mut DataMode) pairs in an in-order traversal over the
    /// leaf nodes of the ShapeData tree.
    pub fn values_mut(&mut self) -> ShapeValIteratorMut {
        ShapeValIteratorMut { stack: vec![mut_ref_slice(&mut self.data).iter_mut()] }
    }
}

/// `Shape::values` iterator
pub struct ShapeValIterator<'shape> {
    stack: Vec<slice::Iter<'shape, ShapeData>>
}

impl<'shape> Iterator for ShapeValIterator<'shape> {
    type Item = (&'shape Type, &'shape DataMode);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = match self.stack.last_mut() {
                Some(iter) => iter.next(),
                None => return None,
            };

            match next {
                Some(&ShapeData::Tup(ref items)) => { self.stack.push(items.iter()); }
                Some(&ShapeData::Val(ref ty, ref dir)) => { return Some((ty, dir)); }
                Some(&ShapeData::Const(..)) => (),
                None => { self.stack.pop(); }
            }
        }
    }
}

/// `Shape::values_mut` iterator
pub struct ShapeValIteratorMut<'shape> {
    stack: Vec<slice::IterMut<'shape, ShapeData>>
}

impl<'shape> Iterator for ShapeValIteratorMut<'shape> {
    type Item = (&'shape mut Type, &'shape mut DataMode);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let next = match self.stack.last_mut() {
                Some(iter) => iter.next(),
                None => return None,
            };

            match next {
                Some(&mut ShapeData::Tup(ref mut items)) => { self.stack.push(items.iter_mut()); }
                Some(&mut ShapeData::Val(ref mut ty, ref mut dir)) => { return Some((ty, dir)); }
                Some(&mut ShapeData::Const(..)) => (),
                None => { self.stack.pop(); }
            }
        }
    }
}

#[test]
fn test_shape_iter() {
    let dm = DataMode { down: true, up: false };
    let shape = ShapeVariant {
        data: ShapeData::Tup(vec![
            ShapeData::Val(Type::Integer(1, 1), dm),
            ShapeData::Tup(vec![
                ShapeData::Val(Type::Integer(2, 2), dm),
                ShapeData::Val(Type::Integer(3, 3), dm),
            ]),
            ShapeData::Val(Type::Integer(4, 4), dm),
        ]),
    };

    let mut iter = shape.values();
    assert_eq!(iter.next(), Some((&Type::Integer(1, 1), &dm)));
    assert_eq!(iter.next(), Some((&Type::Integer(2, 2), &dm)));
    assert_eq!(iter.next(), Some((&Type::Integer(3, 3), &dm)));
    assert_eq!(iter.next(), Some((&Type::Integer(4, 4), &dm)));
    assert_eq!(iter.next(), None);
}
