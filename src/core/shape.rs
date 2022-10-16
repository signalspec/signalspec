use super::{DataMode, Dir, Item, ProtocolRef};

#[derive(Clone, Debug)]
pub struct ShapeMsg {
    pub name: String,
    pub params: Vec<ShapeMsgParam>,
}

#[derive(Clone, Debug)]
pub struct ShapeMsgParam {
    pub item: Item,
    pub direction: Dir,
}

impl ShapeMsg {
    pub fn new(name: String, params: Vec<ShapeMsgParam>) -> ShapeMsg {
        ShapeMsg { name, params }
    }
}

/// Representation of token alphabet between state machine layers of abstraction.
#[derive(Clone, Debug)]
pub struct Shape {
    pub def: ProtocolRef,
    pub dir: Dir,
    pub param: Item,
    pub messages: Vec<ShapeMsg>,
}


impl Shape {
    pub fn variant_named(&self, name: &str) -> Option<(usize, &ShapeMsg)> {
        self.messages.iter().enumerate().find(|&(_, m)| m.name == name)
    }

    pub fn direction(&self) -> DataMode {
        DataMode {
            up: self.messages.iter().flat_map(|m| m.params.iter()).any(|f| f.direction == Dir::Up),
            down: self.messages.iter().flat_map(|m| m.params.iter()).any(|f| f.direction == Dir::Dn)
        }
    }
}

