use std::fmt::{Debug, Display};

use indexmap::IndexMap;

#[derive(Clone, Debug)]
pub struct TupleFields<T> {
    pub positional: Vec<T>,
    pub named: IndexMap<String, T>,
}

impl<T> TupleFields<T> {
    pub fn format(&self, f: &mut std::fmt::Formatter<'_>, mut fmt_item: impl FnMut(&T, &mut std::fmt::Formatter<'_>) -> std::fmt::Result) -> std::fmt::Result {
        write!(f, "(")?;
        let mut first = true;
        for e in &self.positional {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            fmt_item(e, f)?;
        }
        for (k, e) in &self.named {
            if !first {
                write!(f, ", ")?;
            }
            first = false;
            write!(f, "{k}=")?;
            fmt_item(e, f)?;
        }
        write!(f, ")")
    } 
}

impl<T> Default for TupleFields<T> {
    fn default() -> Self {
        Self { positional: Default::default(), named: Default::default() }
    }
}

impl<T> From<Vec<T>> for TupleFields<T> {
    fn from(positional: Vec<T>) -> Self {
        TupleFields { positional, named: Default::default() }
    }
}

impl<T> FromIterator<(Option<String>, T)> for TupleFields<T> {
    fn from_iter<I: IntoIterator<Item = (Option<String>, T)>>(iter: I) -> Self {
        let mut t = TupleFields::default();

        for (name, value) in iter {
            if let Some(name) = name {
                t.named.insert(name, value);
            } else {
                t.positional.push(value);
            }
        }

        t
    }
}

#[derive(Clone)]
pub enum Tree<T> {
    Leaf(T),
    Tuple(TupleFields<Tree<T>>),
}

impl<T> Display for Tree<T> where T: Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tree::Leaf(t) => t.fmt(f),
            Tree::Tuple(fields) => {
                fields.format(f, Display::fmt)
            }
        }
    }
}

impl<T> Debug for Tree<T> where T: Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tree::Leaf(t) => t.fmt(f),
            Tree::Tuple(fields) => {
                fields.format(f, Debug::fmt)
            }
        }
    }
}

impl<T> From<T> for Tree<T> {
    fn from(value: T) -> Self {
        Tree::Leaf(value)
    }
}

pub enum Zip<'a, T, U> {
    Both(&'a Tree<T>, &'a Tree<U>),
    Left(&'a Tree<T>),
    Right(&'a Tree<U>)
}

impl<T> Tree<T> {
    pub fn for_each(&self, f: &mut impl FnMut(&T)) {
        match self {
            Tree::Leaf(i) => f(i),
            Tree::Tuple(TupleFields { positional, named }) => {
                positional.iter().for_each(|s| s.for_each(f));
                named.values().for_each(|s| s.for_each(f));
            }
        }
    }

    pub fn flatten<U>(&self, f: &mut impl FnMut(&T) -> U) -> Vec<U> {
        let mut r = Vec::new();
        self.for_each(&mut |e| r.push(f(e)));
        r
    }

    pub fn map_leaf<U>(&self, f: &mut impl FnMut(&T) -> U) -> Tree<U> {
        match self {
            Tree::Leaf(i) => Tree::Leaf(f(i)),
            Tree::Tuple(TupleFields { positional, named }) => {
                Tree::Tuple(TupleFields {
                    positional: positional.iter().map(|s| s.map_leaf(f)).collect(),
                    named: named.iter().map(|(k, v)| (k.clone(), v.map_leaf(f))).collect()
                })
            }
        }
    }

    pub fn try_map_leaf<U, E>(&self, f: &mut impl FnMut(&T) -> Result<U, E>) -> Result<Tree<U>, E> {
        match self {
            Tree::Leaf(i) => Ok(Tree::Leaf(f(i)?)),
            Tree::Tuple(t) => {
                Ok(Tree::Tuple(TupleFields {
                    positional: t.positional.iter().map(|s| s.try_map_leaf(f)).collect::<Result<_,_>>()?,
                    named: t.named.iter().map(|(k, v)| Ok((k.clone(), v.try_map_leaf(f)?))).collect::<Result<_,_>>()?,
                 }))
            }
        }
    }

    pub fn zip<U>(&self, other: &Tree<U>, f: &mut impl FnMut(Zip<T, U>)) {
        match (self, other) {
            (Tree::Tuple(l), Tree::Tuple(r)) => {
                let mut lp = l.positional.iter();
                let mut rp = r.positional.iter();
                loop {
                    match (lp.next(), rp.next()) {
                        (None, None) => break,
                        (Some(li), Some(ri)) => li.zip(ri, f),
                        (None, Some(ri)) => f(Zip::Right(ri)),
                        (Some(li), None) => f(Zip::Left(li)),
                    }
                }
                for (k, lv) in &l.named {
                    if let Some(rv) = r.named.get(k) {
                        lv.zip(rv, f)
                    } else {
                        f(Zip::Left(lv))
                    }
                }
                for (k, rv) in &r.named {
                    if !l.named.contains_key(k) {
                        f(Zip::Right(rv))
                    }
                }
            }
            (l, r) => f(Zip::Both(l, r)),
        }
    }
}
