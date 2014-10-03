use arena::TypedArena;
use std::collections::HashMap;
use std::cell::RefCell;

#[deriving(Show, PartialEq, Eq, Hash, Clone)]
pub struct Name(u32);

pub struct Interner<'s> {
    arena: TypedArena<String>,
    forward: RefCell<HashMap<&'s str, Name>>,
    backward: RefCell<Vec<&'s str>>,
}

impl<'s> Interner<'s> {
    fn new() -> Interner<'s> {
        Interner {
            arena: TypedArena::new(),
            forward: RefCell::new(HashMap::new()),
            backward: RefCell::new(Vec::new()),
        }
    }

    fn intern(&'s self, s: &str) -> Name {
        let mut forward = self.forward.borrow_mut();
        match forward.find_equiv(&s) {
            Some(&name) => return name,
            None => {}
        }

        let s = self.arena.alloc(s.to_string()).as_slice();
        let mut backward = self.backward.borrow_mut();
        let name = Name(backward.len() as u32);
        backward.push(s);
        forward.insert(s, name);
        name
    }

    fn get(&'s self, n: Name) -> &'s str {
        let Name(i) = n;
        (*self.backward.borrow())[i as uint]
    }
}

#[test]
fn test() {
    let i = Interner::new();
    let n1 = i.intern("foo");
    let n2 = i.intern("bar");
    let n3 = i.intern("foo");
    assert!(n1 == n3);
    assert!(n1 != n2);
    assert_eq!(i.get(n1), "foo");
    assert_eq!(i.get(n2), "bar");
}
