use super::{Expr, ExprDn};

#[derive(Clone, Debug)]
pub enum MatchSet {
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

    /// Combine the two sets, with no check for overlap
    fn union(mut self, other: &Self) -> Self {
        self.alts.extend_from_slice(&other.alts);
        self
    }

    /// Merge the sets, with an error if there is any overlap
    fn merge_disjoint(self, other: &Self) -> Self {
         for x in &self.alts {
            for y in &other.alts {
                if x.variant != y.variant { continue; }
                assert_eq!(x.fields.len(), y.fields.len(), "mismatched pattern lengths");

                if !x.fields.iter().zip(y.fields.iter()).any(|(e1, e2)| e1.excludes(e2)) {
                    panic!("Patterns may overlap: {:?} {:?}", x, y);
                }
            }
        }

        self.union(other)
    }

    pub fn iter(&self) -> impl Iterator<Item = &MessagePattern> {
        self.alts.iter()
    }
}

impl MatchSet {
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

    pub fn check_compatible(prev_followlast: &Option<MatchSet>, next_first: &MatchSet) {
        match (prev_followlast, next_first) {
            (None, _) => {}
            (
              Some(MatchSet::MessageDn { variant: v1, send: s1, .. }),
              MatchSet::MessageDn { variant: v2, send: s2, .. }
            ) if v1 == v2 && s1 == s2 => {}

            (
              Some(MatchSet::MessageUp { .. }),
              MatchSet::MessageUp { .. }
            ) => {}

            (s, o) => panic!("Follow conflict: {:?} <> {:?}", s, o)
        }
    }

    pub fn merge_first<'a, I: Iterator<Item=&'a MatchSet>>(mut i: I) -> MatchSet {
        let first = i.next().unwrap();
        i.fold(first.clone(), |m, v| {
            match (m, v) {
                (
                MatchSet::MessageDn { variant: v1, send: s1, receive:  r1 },
                MatchSet::MessageDn { variant: v2, send: s2, receive: r2 }
                ) if v1 == *v2 && &s1 == s2 => {
                    MatchSet::MessageDn { variant: v1, send: s1, receive: MessagePatternSet::merge_disjoint(r1, r2) }
                }

                (
                MatchSet::MessageUp { receive: r1 },
                MatchSet::MessageUp { receive: r2 }
                ) => {
                    MatchSet::MessageUp { receive: MessagePatternSet::merge_disjoint(r1, r2) }
                }

                (s, o) => panic!("First conflict: {:?} <> {:?}", s, o)
            }
        })
    }

    pub fn merge_followlast<'a, I: Iterator<Item=&'a Option<MatchSet>>>(mut i: I) -> Option<MatchSet> {
        let first = i.next()?;
        i.fold(first.clone(), |m, v| {
            match (m, v) {
                (None, None) => None,

                (
                    Some(MatchSet::MessageDn { variant: v1, send: s1, receive:  r1 }),
                    Some(MatchSet::MessageDn { variant: v2, send: s2, receive: r2 })
                ) if v1 == *v2 && &s1 == s2 => {
                    Some(MatchSet::MessageDn { variant: v1, send: s1, receive: MessagePatternSet::union(r1, r2) })
                }

                (
                    Some(MatchSet::MessageUp { receive: r1 }),
                    Some(MatchSet::MessageUp { receive: r2 })
                ) => {
                    Some(MatchSet::MessageUp { receive: MessagePatternSet::union(r1, r2) })
                }

                (s, o) => panic!("Followlast conflict: {:?} <> {:?}", s, o)
            }
        })
    }
}
 