use super::step::Message;

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
    pub fn lower(m: Message) -> MatchSet { MatchSet { options: vec![ MatchSetItem { lower: Some(m), upper: None }] } }
    pub fn upper(m: Message) -> MatchSet { MatchSet { options: vec![ MatchSetItem { lower: None, upper: Some(m) }] } }

    pub fn followed_by(mut self, other: MatchSet) -> Self {
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
        self
    }

    pub fn alternative(mut self, other: MatchSet) -> Self {
        self.options.extend(other.options.into_iter());
        self
    }

    pub fn join(bottom: &MatchSet, top: &MatchSet) -> Self {
        let mut options = Vec::new();
        for b in &bottom.options {
            for t in &top.options {
                options.push(MatchSetItem { lower: b.lower.clone(), upper: t.upper.clone() });
            }
        }
        MatchSet { options }
    }
}
