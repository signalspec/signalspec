use super::step::{Step, Message};
use data::Type;

#[derive(Clone, Debug)]
pub struct MatchSet {
    pub options: Vec<MatchSetItem>
}

#[derive(Clone, Debug)]
pub struct MatchSetItem {
    pub lower: Option<Message>,
    pub upper: Option<Message>,
}

impl MatchSet {
    pub fn null() -> MatchSet { MatchSet { options: vec![] } }
    pub fn epsilon() -> MatchSet { MatchSet { options: vec![ MatchSetItem { lower: None, upper: None }] } }
    fn lower(m: Message) -> MatchSet { MatchSet { options: vec![ MatchSetItem { lower: Some(m), upper: None }] } }
    fn upper(m: Message) -> MatchSet { MatchSet { options: vec![ MatchSetItem { lower: None, upper: Some(m) }] } }

    fn followed_by(&mut self, other: MatchSet) {
        let mut new = Vec::new();
        for a in self.options.drain(..) {
            if a.lower.is_some() {
                new.push(a);
            } else if a.upper.is_some() {
                for b in &other.options {
                    new.push(MatchSetItem { lower: b.lower.clone(), upper: a.upper.clone() });
                }
            } else {
                new.extend(other.options.iter().cloned())
            }
        }
        self.options = new;
    }

    fn alternative(&mut self, other: MatchSet) {
        self.options.extend(other.options.into_iter());
    }
}

pub fn first(step: &Step) -> MatchSet {
    use self::Step::*;
    match *step {
        Nop => MatchSet::epsilon(),
        Token(ref msg) => MatchSet::lower(msg.clone()),
        TokenTop(ref msg, ref inner) => {
            let mut first = MatchSet::upper(msg.clone());
            first.followed_by(inner.first.clone());
            first
        }
        Seq(ref steps) => {
            let mut first = MatchSet::epsilon();
            for step in steps {
                //TODO: check that followlast and d.first are non-overlapping

                first.followed_by(step.first.clone());
            }
            first
        },
        Repeat(ref count, ref inner) => {
            // TODO: not nullable if count won't match 0
            let mut first = inner.first.clone();

            let count_type = count.get_type();
            match count_type {
                Type::Integer(lo, hi) => {
                    if lo <= 0 && hi >= 0 {
                        first.alternative(MatchSet::epsilon());
                    }
                }
                _ => warn!("Loop count type is {:?} not int", count_type)
            }

            first
            // TODO: check that followlast and first are nonoverlapping
            // TODO: require that inner is non-nullable?
        },
        Foreach(_, _, ref inner) => {
            inner.first.clone()
            //TODO: check that d.followlast and d.first are non-overlapping
        }
        Alt(ref opts) => {
            let mut first = MatchSet::null();
            for &(_, ref inner) in opts {
                // TODO: check that first is nonoverlapping
                first.alternative(inner.first.clone());
            }
            first
        }
        Fork(ref bottom, _, ref top) => {
            let mut options = Vec::new();
            for b in &bottom.first.options {
                for t in &top.first.options {
                    options.push(MatchSetItem { lower: b.lower.clone(), upper: t.upper.clone() });
                }
            }
            MatchSet { options }
        }
        Primitive(_) => {
            MatchSet::epsilon() //TODO: should also include all tokens
        }
    }
}
