use std::fmt::{Debug, Display};

use indexmap::IndexMap;

#[derive(Clone)]
pub enum Tree<T> {
    Leaf(T),
    Positional(Vec<Tree<T>>),
    Named(IndexMap<String, Tree<T>>),
}

impl<T> Display for Tree<T> where T: Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format(f, &mut Display::fmt)
    }
}

impl<T> Debug for Tree<T> where T: Debug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.format(f, &mut Debug::fmt)
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
            Tree::Positional(c) => c.iter().for_each(|s| s.for_each(f)),
            Tree::Named(c) => c.values().for_each(|s| s.for_each(f)),
        }
    }

    pub fn for_each_mut(&mut self, f: &mut impl FnMut(&mut T)) {
        match self {
            Tree::Leaf(i) => f(i),
            Tree::Positional(c) => c.iter_mut().for_each(|s| s.for_each_mut(f)),
            Tree::Named(c) => c.values_mut().for_each(|s| s.for_each_mut(f)),
        }
    }

    pub fn for_each_owned(self, f: &mut impl FnMut(T)) {
        match self {
            Tree::Leaf(i) => f(i),
            Tree::Positional(c) => c.into_iter().for_each(|s| s.for_each_owned(f)),
            Tree::Named(c) => c.into_iter().for_each(|(_, s)| s.for_each_owned(f)),
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
            Tree::Positional(c) => Tree::Positional(c.iter().map(|s| s.map_leaf(f)).collect()),
            Tree::Named(c) => Tree::Named(c.iter().map(|(k, v)| (k.clone(), v.map_leaf(f))).collect()),
        }
    }

    pub fn try_map_leaf<U, E>(&self, f: &mut impl FnMut(&T) -> Result<U, E>) -> Result<Tree<U>, E> {
        match self {
            Tree::Leaf(i) => Ok(Tree::Leaf(f(i)?)),
            Tree::Positional(c) => Ok(Tree::Positional(c.iter().map(|s| s.try_map_leaf(f)).collect::<Result<_,_>>()?)),
            Tree::Named(c) => Ok(Tree::Named(c.iter().map(|(k, v)| Ok((k.clone(), v.try_map_leaf(f)?))).collect::<Result<_,_>>()?)),
        }
    }

    pub fn zip<U>(&self, other: &Tree<U>, f: &mut impl FnMut(Zip<T, U>)) {
        match (self, other) {
            (Tree::Positional(l), Tree::Positional(r)) => {
                let mut lp = l.iter();
                let mut rp = r.iter();
                loop {
                    match (lp.next(), rp.next()) {
                        (None, None) => break,
                        (Some(li), Some(ri)) => li.zip(ri, f),
                        (None, Some(ri)) => f(Zip::Right(ri)),
                        (Some(li), None) => f(Zip::Left(li)),
                    }
                }
            }
            (Tree::Named(l), Tree::Named(r)) => {
                for (k, lv) in l {
                    if let Some(rv) = r.get(k) {
                        lv.zip(rv, f)
                    } else {
                        f(Zip::Left(lv))
                    }
                }
                for (k, rv) in r {
                    if !l.contains_key(k) {
                        f(Zip::Right(rv))
                    }
                }
            }
            (l, r) => f(Zip::Both(l, r)),
        }
    }

    pub fn format(&self, f: &mut std::fmt::Formatter<'_>, fmt_item: &mut impl FnMut(&T, &mut std::fmt::Formatter<'_>) -> std::fmt::Result) -> std::fmt::Result {
        match self {
            Tree::Leaf(n) => fmt_item(n, f),
            Tree::Positional(c) => {
                write!(f, "(")?;
                for (i, e) in c.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    e.format(f, fmt_item)?;
                }
                write!(f, ")")
            }
            Tree::Named(c) => {
                write!(f, "(")?;
                for (i, (k, e)) in Box::new(c.iter()).enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{k}=")?;
                    e.format(f, fmt_item)?;
                }
                write!(f, ")")
            }
        }
    }
}
