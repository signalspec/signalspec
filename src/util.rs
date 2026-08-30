use itertools::{put_back, PutBack};

pub trait IterUtil: Iterator {
    fn at_least_one(self) -> Option<PutBack<Self>> where Self: Sized;
}

impl<T: Iterator> IterUtil for T {
    fn at_least_one(mut self) -> Option<PutBack<Self>> {
        self.next().map(|first| put_back(self).with_value(first))
    }
}
