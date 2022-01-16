use super::{Expr, ExprDn};

#[derive(Clone, Debug)]
pub enum MatchSet {
    None,
    Process,
    MessageUp { receive: MessagePatternSet },
    MessageDn { variant: usize, send: Vec<ExprDn>, receive: MessagePatternSet },
}

#[derive(Clone, Debug)]
pub struct MessagePattern {
    pub variant: usize,
    pub fields: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct MessagePatternSet {
    alts: Vec<MessagePattern>,
}

impl MessagePatternSet {
    fn one(p: MessagePattern) -> MessagePatternSet {
        MessagePatternSet { alts: vec![p] }
    }

    fn merge(mut self, other: &Self) -> Self {
         for x in &self.alts {
            for y in &other.alts {
                if x.variant != y.variant { continue; }
                assert_eq!(x.fields.len(), y.fields.len(), "mismatched pattern lengths");

                if !x.fields.iter().zip(y.fields.iter()).any(|(e1, e2)| e1.excludes(e2)) {
                    panic!("Patterns may overlap: {:?} {:?}", x, y);
                }
            }
        }

        self.alts.extend_from_slice(&other.alts);
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = &MessagePattern> {
        self.alts.iter()
    }
}

impl MatchSet {
    pub fn null() -> MatchSet { MatchSet::None }
    pub fn proc() -> MatchSet { MatchSet::Process }
    pub fn lower(variant: usize, dn: Vec<ExprDn>, up: Vec<Expr>) -> MatchSet {
        MatchSet::MessageDn {
            variant,
            send: dn,
            receive: MessagePatternSet::one(MessagePattern { variant, fields: up })
        }
    }
    pub fn upper(variant: usize, dn: Vec<Expr>) -> MatchSet {
        MatchSet::MessageUp {
            receive: MessagePatternSet::one(MessagePattern { variant, fields: dn }),
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

               
                MatchSet::MessageDn { variant: v1, send: s1, receive: r1.merge(r2) }
            }

            (
              MatchSet::MessageUp { receive: mut r1 },
              MatchSet::MessageUp { receive: r2 }
            ) => {//TODO: check for overlap
                MatchSet::MessageUp { receive: r1.merge(r2) }
            }

            (s, o) => panic!("Send conflict: {:?} <> {:?}", s, o)
        }
    }
}
 