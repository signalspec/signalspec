use std::fmt::Debug;
use std::sync::RwLock;
use std::marker::PhantomData;
use std::mem::transmute;

pub struct Index<T, K> {
    items: RwLock<Vec<Option<Box<T>>>>,
    _phantom: PhantomData<K>,
}

impl<T, K: Copy + Debug + From<usize>> Index<T, K> where usize: From<K> {
    pub fn new() -> Self {
        Index {
            items: RwLock::new(Vec::new()),
            _phantom: PhantomData,
        }
    }

    pub fn create(&self) -> K {
        let mut locked = self.items.write().unwrap();
        let id = locked.len();
        locked.push(None);
        K::from(id)
    }

    pub fn define(&self, k: K, v: T) {
        let mut locked = self.items.write().unwrap();
        let pos = &mut locked[usize::from(k)];
        assert!(pos.is_none());
        *pos = Some(Box::new(v));
    }

    pub fn get<'s>(&'s self, k: K) -> &'s T {
        let locked = self.items.read().unwrap();
        if let Some(x) = locked[usize::from(k)].as_ref() {
            unsafe {
                // Items are never removed from the vec, boxed objects have stable addresses
                transmute::<&T, &'s T>(x)
            }
        } else {
            panic!("{:?} was not defined", k);
        }
    }
}
