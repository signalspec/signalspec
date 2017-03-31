use std::iter;
use data::{Type, Value, DataMode};
use language::Item;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ProtocolId(pub usize);

impl From<usize> for ProtocolId {
    fn from(i: usize) -> ProtocolId { ProtocolId(i) }
}

impl From<ProtocolId> for usize {
    fn from(i: ProtocolId) -> usize { i.0 }
}

/// Representation of token alphabet between state machine layers of abstraction.
/// Produced from an Protocol by name resolution and direction inference.
#[derive(Clone, Debug, PartialEq)]
pub enum Shape {
    Tup(Vec<Shape>),
    Protocol {
        def: ProtocolId,
        //params: Vec<Item<'a>>, // TODO: remove type parameter from Item
        messages: Vec<Shape>,
    },
    Val(Type, DataMode),
    Const(Value),
}

impl Shape {
    /// Produces a shape with no variants
    pub fn null() -> Shape {
        Shape::Tup(vec![]) //TODO: this should be a specific protocol defined in the prelude
     }

    /// Produces a shape for a stream of bytes in the specified direction
    pub fn bytes(dir: DataMode) -> Shape {
        Shape::Val(Type::Integer(0, 255), dir)
    }

    /// Produces a shape for a stream of bytes in the specified direction and range
    pub fn number(dir: DataMode, min: f64, max: f64) -> Shape {
        Shape::Val(Type::Number(min, max), dir)
    }

    /// Produces a shape for a stream of complex number in the specified direction
    pub fn complex(dir: DataMode) -> Shape {
        Shape::Val(Type::Complex, dir)
    }

    /// If the shape represents a stream of bytes, returns Some(data direction)
    pub fn match_bytes(&self) -> Option<DataMode> {
        match *self {
            Shape::Val(Type::Integer(0, 255), dir) => Some(dir),
            _ => None
        }
    }

    /// If the shape represents a stream of complex number, returns Some(data direction)
    pub fn match_complex(&self) -> Option<DataMode> {
        match *self {
            Shape::Val(Type::Complex, dir) => Some(dir),
            _ => None
        }
    }

    pub fn data_mode(&self) -> DataMode {
        let base_data_mode = DataMode { down: false, up: false };
        let fold_data_mode = |am: DataMode, bm: DataMode| { DataMode { down: am.down || bm.down, up: am.up || bm.up  }};
        match *self {
            Shape::Tup(ref x) => x.iter().map(|x| x.data_mode()).fold(base_data_mode, fold_data_mode),
            Shape::Protocol { ref messages, .. } => messages.iter().map(|x| x.data_mode()).fold(base_data_mode, fold_data_mode),
            Shape::Val(_, mode) => mode,
            Shape::Const(..) => base_data_mode,
        }
    }

    pub fn count_fields(&self) -> usize {
        match *self {
            Shape::Tup(ref x) => x.iter().map(Shape::count_fields).sum(),
            Shape::Protocol { ref messages, .. } => {
                let tag = if messages.len() <= 1 { 0usize } else { 1usize };
                let inner: usize = messages.iter().map(Shape::count_fields).sum();
                tag + inner
            }
            Shape::Val(..) => 1,
            Shape::Const(..) => 0,
        }
    }

    pub fn fields(&self) -> Fields {
        match *self {
            Shape::Tup(ref x) => Fields::new(x.iter().flat_map(Shape::fields).collect()),
            Shape::Protocol { ref messages, .. } => {
                if messages.len() <= 1 {
                    messages[0].fields()
                } else {
                    let len = messages.len() as i64;
                    Fields::new(
                        iter::once(Field { ty: Type::Integer(len, len), is_tag: true, dir: DataMode{ up: true, down: true }})
                        .chain(messages.iter().flat_map(Shape::fields)).collect()
                    )
                }
            }
            Shape::Val(ref ty, dir) => Fields::new(vec![Field { ty: ty.clone(), is_tag: false, dir: dir }]),
            Shape::Const(..) => Fields::new(vec![]),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Field {
    pub ty: Type,
    pub is_tag: bool,
    pub dir: DataMode
}

#[derive(Clone, PartialEq, Debug)]
pub struct Fields {
    fields: Vec<Field>
}

impl IntoIterator for Fields {
    type Item = Field;
    type IntoIter = ::std::vec::IntoIter<Field>;
    fn into_iter(self) -> Self::IntoIter { self.fields.into_iter() }
}

impl Fields {
    pub fn new(fields: Vec<Field>) -> Fields { Fields { fields } }
    pub fn len(&self) -> usize { self.fields.len() }
    pub fn iter(&self) -> ::std::slice::Iter<Field> { self.fields.iter() }
    pub fn iter_mut(&mut self) -> ::std::slice::IterMut<Field> { self.fields.iter_mut() }
    pub fn direction(&self) -> DataMode {
        DataMode {
            up: self.iter().any(|f| f.dir.up && !f.is_tag),
            down: self.iter().any(|f| f.dir.down && !f.is_tag)
        }
    }
}
