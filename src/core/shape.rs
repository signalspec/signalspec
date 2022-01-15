use super::{DataMode, Dir, Expr, Item, ProtocolRef};

fn count_item_fields(i: &Item) -> usize {
    match *i {
        Item::Value(Expr::Const(_)) => 0,
        Item::Value(_) => 1,
        Item::Tuple(ref t) => t.iter().map(count_item_fields).sum(),
        _ => panic!("Item {:?} not allowed in shape", i),
    }
}

#[derive(Clone, Debug)]
pub struct ShapeMsg {
    pub name: String,
    pub params: Vec<ShapeMsgParam>,
}

#[derive(Clone, Debug)]
pub struct ShapeMsgParam {
    pub item: Item,
    pub direction: DataMode,
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
            up: self.messages.iter().flat_map(|m| m.params.iter()).any(|f| f.direction.up),
            down: self.messages.iter().flat_map(|m| m.params.iter()).any(|f| f.direction.down)
        }
    }
}

