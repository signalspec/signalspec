use std::{marker::PhantomData, ops::Range};

pub trait EntityKey: Copy + Eq {
    fn new(id: usize) -> Self;
    fn index(self) -> usize;
}

macro_rules! entity_key {
    ($visibility:vis $entity:ident) => {
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        $visibility struct $entity(u32);

        impl $crate::entitymap::EntityKey for $entity {
            #[inline]
            fn new(index: usize) -> Self {
                assert!(index < (u32::MAX as usize));
                $entity(index as u32)
            }

            #[inline]
            fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl From<u32> for $entity {
            fn from(i: u32) -> $entity {
                $entity(i)
            }
        }

        impl From<$entity> for u32 {
            fn from(i: $entity) -> u32 {
                i.0
            }
        }

        impl $entity {
            // TODO: remove when impl const trait is stable
            #[allow(unused)]
            const fn from(i: u32) -> $entity {
                $entity(i)
            }
        }
    }
}

pub(crate) use entity_key;

#[derive(Clone)]
pub struct EntityMap<K, V> {
    k: PhantomData<K>,
    v: Vec<V>,
}

impl <K:EntityKey, V> EntityMap<K, V> {
    pub fn new() -> Self { 
        Self { k: PhantomData, v:Vec::new() }
    }

    pub fn with_capacity(cap: usize) -> Self { 
        Self { k: PhantomData, v:Vec::with_capacity(cap) }
    }

    pub fn len(&self) -> usize {
        self.v.len()
    }

    pub(crate) fn next_key(&self) -> K {
        K::new(self.len())
    }

    pub fn push(&mut self, v: V) -> K {
        let k = K::new(self.v.len());
        self.v.push(v);
        k
    }

    pub fn keys(&self) -> Keys<K> {
        (0..self.len()).map(K::new)
    }

    pub fn iter(&self) -> Iter<K, V> {
        self.v.iter().enumerate().map(|(k, v)| (K::new(k), v))
    }

    pub fn iter_mut(&mut self) -> IterMut<K, V> {
        self.v.iter_mut().enumerate().map(|(k, v)| (K::new(k), v))
    }
}

impl <K: EntityKey, V> std::ops::Index<K> for EntityMap<K, V> {
    type Output = V;

    fn index(&self, index: K) -> &Self::Output {
        &self.v[index.index()]
    }
}

impl <K: EntityKey, V> std::ops::IndexMut<K> for EntityMap<K, V> {
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        &mut self.v[index.index()]
    }
}

type Keys<K> = std::iter::Map<Range<usize>, fn(usize) -> K>;

type Iter<'a, K, V> = std::iter::Map<std::iter::Enumerate<std::slice::Iter<'a, V>>, fn((usize, &'a V)) -> (K, &'a V)>;

impl <'a, K: EntityKey, V> std::iter::IntoIterator for &'a EntityMap<K, V> {
    type Item = (K, &'a V);

    type IntoIter = Iter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

type IterMut<'a, K, V> = std::iter::Map<std::iter::Enumerate<std::slice::IterMut<'a, V>>, fn((usize, &'a mut V)) -> (K, &'a mut V)>;

impl <'a, K: EntityKey, V> std::iter::IntoIterator for &'a mut EntityMap<K, V> {
    type Item = (K, &'a mut V);

    type IntoIter = IterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl <K: EntityKey, V> std::iter::FromIterator<V> for EntityMap<K, V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Vec::from_iter(iter).into()
    }
}

impl <K, V> From<Vec<V>> for EntityMap<K, V> {
    fn from(v: Vec<V>) -> Self {
        EntityMap { k: PhantomData, v }
    }
}

impl <K, V> From<EntityMap<K, V>> for Vec<V> {
    fn from(m: EntityMap<K, V>) -> Self {
        m.v
    }
}

