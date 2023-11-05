use std::fmt::Display;

use indexmap::IndexMap;

use crate::Value;

use super::{Dir, Item, ProtocolRef, TypeTree, TryFromConstant};

#[derive(Copy, Clone, Debug)]
pub enum ShapeMode {
    /// No data movement.
    Null,

    /// Tag and data in down direction, no data in up direction.
    Dn,

    /// No data in down direction, tag and data up.
    Up,

    /// Tag in down direction, bidirectional data. The lower process
    /// must eventually respond to each message if the upper
    /// process stops sending messages at any point.
    Sync,

    /// Tag in down direction, bidirectional data. No refutable
    /// patterns allowed, and the lower process might not respond
    /// until the upper process signals completion of all down data.
    Async,
}

impl ShapeMode {
    pub fn allows_data(self, dir: Dir) -> bool {
        match (self, dir) {
            (ShapeMode::Dn, Dir::Dn) => true,
            (ShapeMode::Up, Dir::Up) => true,
            (ShapeMode::Sync, _) => true,
            (ShapeMode::Async, _) => true,
            _ => false,
        }
    }

    pub fn allows_child(self, child: ShapeMode) -> bool {
        use ShapeMode::*;
        match (self, child) {
            (Null, Null) => true,
            (Dn, Dn) => true,
            (Up, Up) => true,
            (Sync, Dn | Sync | Async) => true,
            (Async, Dn | Async) => true,
            _ => false
        }
    }

    /// Returns whether this shape requires a downward channel
    pub fn has_dn_channel(self) -> bool {
        match self {
            ShapeMode::Null => false,
            ShapeMode::Dn => true,
            ShapeMode::Up => false,
            ShapeMode::Sync => true,
            ShapeMode::Async => true,
        }
    }

    /// Returns whether this shape requires a downward channel
    pub fn has_up_channel(self) -> bool {
        match self {
            ShapeMode::Null => false,
            ShapeMode::Dn => false,
            ShapeMode::Up => true,
            ShapeMode::Sync => true,
            ShapeMode::Async => true,
        }
    }
}

impl TryFrom<Value> for ShapeMode {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, ()> {
        match value.as_symbol() {
            Some("null") => Ok(ShapeMode::Null),
            Some("up") => Ok(ShapeMode::Up),
            Some("dn") => Ok(ShapeMode::Dn),
            Some("sync") => Ok(ShapeMode::Sync),
            Some("async") => Ok(ShapeMode::Async),
            _ => Err(()),
        }
    }
}

impl TryFromConstant for ShapeMode {
    const EXPECTED_MSG: &'static str = "#up | #dn | #sync | #async | #null";
}

impl Display for ShapeMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ShapeMode::Null => "#null",
            ShapeMode::Dn => "#dn",
            ShapeMode::Up => "#up",
            ShapeMode::Sync => "sync",
            ShapeMode::Async => "#async",
        })
    }
}

impl From<Dir> for ShapeMode {
    fn from(value: Dir) -> Self {
        match value {
            Dir::Up => Self::Up,
            Dir::Dn => Self::Dn,
        }
    }
}

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
    pub mode: ShapeMode,
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
}

