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
