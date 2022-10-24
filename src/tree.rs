#[derive(Clone)]
pub enum Tree<T> {
    Leaf(T),
    Tuple(Vec<Tree<T>>),
}

pub enum Zip<'a, T, U> {
    Both(&'a Tree<T>, &'a Tree<U>),
    Left(&'a Tree<T>),
    Right(&'a Tree<U>)
}

impl<T> Tree<T> {
    pub fn map_leaf<U>(&self, f: &mut impl FnMut(&T) -> U) -> Tree<U> {
        match self {
            Tree::Leaf(i) => Tree::Leaf(f(i)),
            Tree::Tuple(t) => {
                Tree::Tuple(t.iter().map(|s| s.map_leaf(f)).collect())
            }
        }
    }

    pub fn try_map_leaf<U, E>(&self, f: &mut impl FnMut(&T) -> Result<U, E>) -> Result<Tree<U>, E> {
        match self {
            Tree::Leaf(i) => Ok(Tree::Leaf(f(i)?)),
            Tree::Tuple(t) => {
                Ok(Tree::Tuple(t.iter().map(|s| s.try_map_leaf(f)).collect::<Result<_,_>>()?))
            }
        }
    }

    pub fn zip<U>(&self, other: &Tree<U>, f: &mut impl FnMut(Zip<T, U>)) {
        match (self, other) {
            (Tree::Tuple(ref l), Tree::Tuple(ref r)) => {
                let mut l = l.iter();
                let mut r = r.iter();
                loop {
                    match (l.next(), r.next()) {
                        (None, None) => break,
                        (Some(li), Some(ri)) => li.zip(ri, f),
                        (None, Some(ri)) => f(Zip::Right(ri)),
                        (Some(li), None) => f(Zip::Left(li)),
                    }
                }
            }
            (l, r) => f(Zip::Both(l, r)),
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Tree<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Leaf(v) => v.fmt(f),
            Self::Tuple(v) => {
                write!(f, "(")?;
                for i in v.iter() {
                    i.fmt(f)?;
                    write!(f, ", ")?;
                }
                write!(f, ")")
            }
        }
    }
}
