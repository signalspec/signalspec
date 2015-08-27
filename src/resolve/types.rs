pub use self::Type::*;
use eval::DataMode;
use std::collections::HashSet;
use std::cmp::{min, max};
use std::slice;

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

/// Representation of token alphabet between state machine layers of abstraction.
/// Produced from an Interface by name resolution and direction inference.
#[derive(Clone, Debug, PartialEq)]
pub struct Shape {
    pub data: ShapeData,
    pub child: Option<Box<Shape>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ShapeData {
    Tup(Vec<ShapeData>),
    Val(Type, DataMode),
}

// TODO: should be fully empty, not a no-direction member
pub static NULL_SHAPE: Shape = Shape {
    data: ShapeData::Val(Bottom, DataMode { down: false, up: false }),
    child: None,
};

impl Shape {
    pub fn data_mode(&self) -> DataMode {
        self.values().fold(
            DataMode { down: false, up: false },
            |am, (_, bm)| { DataMode { down: am.down || bm.down, up: am.up || bm.up  }}
        )
    }

    /// Iterator that produces (&Type, &DataMode) pairs in an in-order traversal over the leaf
    /// nodes of the ShapeData tree.
    pub fn values(&self) -> ShapeValIterator {
        ShapeValIterator { stack: vec![slice::ref_slice(&self.data).iter()] }
    }

    /// Iterator that produces (&mut Type, &mut DataMode) pairs in an in-order traversal over the
    /// leaf nodes of the ShapeData tree.
    pub fn values_mut(&mut self) -> ShapeValIteratorMut {
        ShapeValIteratorMut { stack: vec![slice::mut_ref_slice(&mut self.data).iter_mut()] }
    }
}

/// `Shape::values` iterator
struct ShapeValIterator<'shape> {
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
                None => { self.stack.pop(); }
            }
        }
    }
}

/// `Shape::values_mut` iterator
struct ShapeValIteratorMut<'shape> {
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
                None => { self.stack.pop(); }
            }
        }
    }
}

#[test]
fn test_shape_iter() {
    let dm = DataMode { down: true, up: false };
    let shape = Shape {
        data: ShapeData::Tup(vec![
            ShapeData::Val(Type::Integer(1, 1), dm),
            ShapeData::Tup(vec![
                ShapeData::Val(Type::Integer(2, 2), dm),
                ShapeData::Val(Type::Integer(3, 3), dm),
            ]),
            ShapeData::Val(Type::Integer(4, 4), dm),
        ]),
        child: None,
    };

    let mut iter = shape.values();
    assert_eq!(iter.next(), Some((&Type::Integer(1, 1), &dm)));
    assert_eq!(iter.next(), Some((&Type::Integer(2, 2), &dm)));
    assert_eq!(iter.next(), Some((&Type::Integer(3, 3), &dm)));
    assert_eq!(iter.next(), Some((&Type::Integer(4, 4), &dm)));
    assert_eq!(iter.next(), None);
}
