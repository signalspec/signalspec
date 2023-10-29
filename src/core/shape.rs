use indexmap::IndexMap;

use super::{DataMode, Dir, Item, ProtocolRef, TypeTree};

#[derive(Clone, Debug)]
pub struct ShapeMsg {
    pub name: String,
    pub tag: usize,
    pub params: Vec<ShapeMsgParam>,
    pub child: Option<Shape>,
}

#[derive(Clone, Debug)]
pub struct ShapeMsgParam {
    pub ty: TypeTree,
    pub direction: Dir,
}

/// Representation of token alphabet between state machine layers of abstraction.
#[derive(Clone, Debug)]
pub struct Shape {
    pub def: ProtocolRef,
    pub dir: Dir,
    pub param: Item,
    pub tag_offset: usize,
    pub tag_count: usize,
    pub messages: Vec<ShapeMsg>,
    pub children: IndexMap<String, Shape>,
}


impl Shape {
    pub fn variant_named(&self, name: &str) -> Option<&ShapeMsg> {
        self.messages.iter().find(|m| m.name == name)
    }

    pub fn child_named(&self, name: &str) -> Option<&Shape> {
        self.children.get(name)
    }

    pub fn mode(&self) -> Dir {
        self.dir
    }

    /// Returns a structure indicating which channel directions must exist to
    /// implement this shape
    pub fn direction(&self) -> DataMode {
        if self.messages.is_empty() && self.children.is_empty() {
            return DataMode { down: false, up: false }
        }
        match self.dir {
            Dir::Up => DataMode { down: false, up: true },
            Dir::Dn => {
                let has_up_data = self.messages.iter()
                    .flat_map(|m| m.params.iter())
                    .any(|f| f.direction == Dir::Up);
                let child_has_up_data = self.children.values().any(|s| s.direction().up);
                DataMode { down: true, up: has_up_data || child_has_up_data}
            }
        }
    }
}

