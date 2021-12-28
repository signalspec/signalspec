use super::{Expr, Message};

#[derive(Clone, Debug, PartialEq)]
pub enum MatchSet {
    None,
    Process,
    MessageUp { receive: Vec<(usize, Vec<Expr>)> },
    MessageDn { variant: usize, send: Vec<Expr>, receive: Vec<Vec<Expr>> },
}

impl MatchSet {
    pub fn null() -> MatchSet { MatchSet::None }
    pub fn proc() -> MatchSet { MatchSet::Process }
    pub fn lower(m: Message) -> MatchSet {
        MatchSet::MessageDn {
            variant: m.variant,
            send: m.dn.clone(),
            receive: vec![m.up.clone()]
        }
    }
    pub fn upper(m: Message) -> MatchSet {
        MatchSet::MessageUp {
            receive: vec![(m.variant, m.dn.clone())],
        }
    }

    pub fn merge(self, other: &MatchSet) -> MatchSet {
        match (self, other) {
            (MatchSet::None, o) => o.clone(),

            (o, MatchSet::None) => o,

            (
              MatchSet::MessageDn { variant: v1, send: s1, receive: mut r1 },
              MatchSet::MessageDn { variant: v2, send: s2, receive: r2 }
            ) if v1 == *v2 && &s1 == s2 => {
                r1.extend_from_slice(r2); //TODO: check for overlap
                MatchSet::MessageDn { variant: v1, send: s1, receive: r1 }
            }

            (
              MatchSet::MessageUp { receive: mut r1 },
              MatchSet::MessageUp { receive: r2 }
            ) => {
                r1.extend_from_slice(r2);  //TODO: check for overlap
                MatchSet::MessageUp { receive: r1 }
            }
            (s, o) => panic!("Send conflict: {:?} <> {:?}", s, o)
        }
    }
}
 