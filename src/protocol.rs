use data::{Type, Value, DataMode};
use language::Item;

type Identifier = String;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ProtocolId(pub usize);

impl From<usize> for ProtocolId {
    fn from(i: usize) -> ProtocolId { ProtocolId(i) }
}

impl From<ProtocolId> for usize {
    fn from(i: ProtocolId) -> usize { i.0 }
}

#[derive(Default, Clone)]
pub struct ProtocolDef {
    pub name: Identifier,
    pub params: Vec<Identifier>,
    pub messages: Vec<ProtocolMessageDef>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ProtocolMessageDef {
    Tup(Vec<ProtocolMessageDef>),
    Protocol(ProtocolId),
    Val(Type),
    Const(Value),
}

