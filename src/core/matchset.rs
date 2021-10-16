use super::{Expr, Message};

#[derive(Clone, Debug)]
pub struct MatchSet {
    pub send: MatchSend,
    pub options: Vec<Message>
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchSend {
    None,
    Process,
    MessageUp,
    MessageDn(usize, Vec<Expr>),
}

impl MatchSet {
    pub fn null() -> MatchSet { MatchSet { send: MatchSend::None, options: vec![] } }
    pub fn proc() -> MatchSet { MatchSet { send: MatchSend::Process, options: vec![] } }
    pub fn lower(m: Message) -> MatchSet { MatchSet { send: MatchSend::MessageDn(m.variant, m.dn.clone()), options: vec![ m ] } }
    pub fn upper(m: Message) -> MatchSet { MatchSet { send: MatchSend::MessageUp, options: vec![ m ] } }

    pub fn merge(&mut self, other: &MatchSet) {
        if self.send == MatchSend::None {
            self.send = other.send.clone()
        } else if other.send != MatchSend::None && self.send != other.send {
            panic!("Send conflict: {:?} <> {:?}", self, other)
        }

        // TODO: check overlap

        self.options.extend(other.options.iter().cloned());
    }
}
