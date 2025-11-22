
use std::collections::BTreeSet;

use crate::{Value, core::op::ConcatElem, syntax::Number};

use itertools::Itertools;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Predicate {
    /// Always matches
    Any,

    /// Number: in range (bottom inclusive, top-exclusive)
    Range(Number, Number),

    /// Number: equal
    /// Used for matching a single number, since range is exclusive
    Number(Number),

    /// Symbol: in set
    SymbolSet(BTreeSet<String>),

    /// Vector: test each slice / element
    Vector(Vec<Predicate>),
}

impl Predicate {
    pub fn from_value(v: &Value) -> Option<Predicate> {
        match *v {
            Value::Number(n) => Some(Predicate::Number(n)),
            Value::Symbol(ref s) => Some(Predicate::SymbolSet([s.clone()].into())),
            Value::Vector(ref v) => v.iter()
                .map(|v| Predicate::from_value(v))
                .collect::<Option<Vec<_>>>().map(Predicate::Vector),
            Value::Complex(_) => None,
        }
    }

    pub fn vector(components: impl Iterator<Item = ConcatElem<Predicate>>) -> Predicate {
        let mut parts = Vec::with_capacity(components.size_hint().0);
        for p in components {
            match p {
                ConcatElem::Elem(e) => parts.push(e),
                ConcatElem::Slice(Predicate::Vector(inner), _) => {
                    parts.extend(inner)
                }
                ConcatElem::Slice(Predicate::Any, len) => {
                    parts.extend(std::iter::repeat_n(Predicate::Any, len as usize))
                }
                ConcatElem::Slice(s, _) => panic!("Invalid predicate slice in vector: {s:?}"),
            }
        }
        Predicate::Vector(parts)
    }

    pub fn test(&self, v: &Value) -> bool {
        match (self, v) {
            (Predicate::Any, _) => true,
            (Predicate::Range(lo, hi), Value::Number(n)) => n>=lo && n<hi,
            (Predicate::Number(x), Value::Number(n)) => x == n,
            (Predicate::SymbolSet(set), Value::Symbol(s)) => set.contains(s),
            (Predicate::Vector(components), Value::Vector(vec)) => {
                vec.iter().zip_eq(components.iter()).all(|(elem, component)| {
                    component.test(elem)
                })
            },
            _ => panic!("Type mismatch: {} against predicate {:?}", v, self)
        }
    }

    pub fn excludes(&self, other: &Self) -> bool {
        match (self, other) {
            (Predicate::Any, _) | (_, Predicate::Any) => false,
            (Predicate::Range(lo1, hi1), Predicate::Range(lo2, hi2)) => hi1 <= lo2 || hi2 <= lo1,
            (Predicate::Range(lo, hi), Predicate::Number(n))
            | (Predicate::Number(n), Predicate::Range(lo, hi))  => n < lo || n >= hi,
            (Predicate::Number(n1), Predicate::Number(n2)) => n1 != n2,
            (Predicate::SymbolSet(s1), Predicate::SymbolSet(s2)) => s1.is_disjoint(s2),
            (Predicate::Vector(c1), Predicate::Vector(c2)) => {
               c1.iter().zip_eq(c2.iter()).any(|(p1, p2)| p1.excludes(p2))
            },
            _ => panic!("Predicate type mismatch in `excludes`: {:?}, {:?}", self, other),
        }
    }

    pub fn index(&self, index: u32) -> Predicate {
        match self {
            Predicate::Any => Predicate::Any,
            Predicate::Vector(components) => {
                components[index as usize].clone()
            }
            _ => panic!("Indexing into non-vector predicate: {:?}", self),
        }
    }

    pub fn slice(&self, offset: u32, width: u32) -> Predicate {
        match self {
            Predicate::Any => Predicate::Any,
            Predicate::Vector(components) => {
                Predicate::Vector(components[offset as usize..][..width as usize].to_vec())
            }
            _ => panic!("Slicing non-vector predicate: {:?}", self),
        }
    }
}

#[test]
fn test_predicate() {
    use crate::core::resolve::expr::test_expr_parse;
    use crate::core::step::expr_lower::ExprLower;
    use crate::entitymap::EntityMap;

    let c = ExprLower::new(&EntityMap::new());

    let range = c.predicate(&test_expr_parse("1 ! 1..10").inner()).unwrap();
    assert_eq!(range, Predicate::Range(1.into(), 10.into()));
    assert_eq!(range.test(&Value::Number(1.into())), true);
    assert_eq!(range.test(&Value::Number(2.into())), true);
    assert_eq!(range.test(&Value::Number(10.into())), false);
    assert_eq!(range.excludes(&Predicate::Range(10.into(), 20.into())), true);
    assert_eq!(range.excludes(&Predicate::Range(0.into(), 1.into())), true);
    assert_eq!(range.excludes(&Predicate::Range(0.into(), 11.into())), false);
    assert_eq!(range.excludes(&Predicate::Range(3.into(), 5.into())), false);
    assert_eq!(range.excludes(&Predicate::Range(0.into(), 5.into())), false);
    assert_eq!(range.excludes(&Predicate::Range(5.into(), 20.into())), false);
    assert_eq!(range.excludes(&Predicate::Number(10.into())), true);
    assert_eq!(range.excludes(&Predicate::Number(0.into())), true);
    assert_eq!(range.excludes(&Predicate::Number(5.into())), false);

    let any = c.predicate(&test_expr_parse("<: 5").inner()).unwrap();
    assert_eq!(any, Predicate::Any);
    assert_eq!(any.test(&Value::Number(1.into())), true);
    assert_eq!(any.excludes(&Predicate::Range(5.into(), 20.into())), false);

    let int = c.predicate(&test_expr_parse(":> 5").inner()).unwrap();
    assert_eq!(int, Predicate::Number(5.into()));
    assert_eq!(int.test(&Value::Number(5.into())), true);
    assert_eq!(int.test(&Value::Number(4.into())), false);
    assert_eq!(int.excludes(&int), false);
    assert_eq!(int.excludes(&Predicate::Number(0.into())), true);

    let sym = c.predicate(&test_expr_parse("#h | #l").inner()).unwrap();
    assert_eq!(sym, Predicate::SymbolSet(["h".into(), "l".into()].into()));
    assert_eq!(sym.test(&Value::Symbol("h".into())), true);
    assert_eq!(sym.test(&Value::Symbol("z".into())), false);
    assert_eq!(sym.excludes(&Predicate::SymbolSet(["h".into(), "x".into()].into())), false);
    assert_eq!(sym.excludes(&Predicate::SymbolSet(["z".into(), "x".into()].into())), true);

    let vec = c.predicate(&test_expr_parse("[0..2, 2, 3:_]").inner()).unwrap();
    assert_eq!(vec, Predicate::Vector(vec![
        Predicate::Range(0.into(), 2.into()),
        Predicate::Number(2.into()),
        Predicate::Any,
        Predicate::Any,
        Predicate::Any,
    ]));
    assert_eq!(vec.test(&Value::Vector(vec![
        Value::Number(0.into()),
        Value::Number(2.into()),
        Value::Number(9.into()),
        Value::Number(9.into()),
        Value::Number(9.into()),
    ])), true);
    assert_eq!(vec.test(&Value::Vector(vec![
        Value::Number(0.into()),
        Value::Number(3.into()),
        Value::Number(9.into()),
        Value::Number(9.into()),
        Value::Number(9.into()),
    ])), false);
    assert_eq!(vec.excludes(&vec), false);
    assert_eq!(vec.excludes(&c.predicate(&test_expr_parse("[0..5, 9, 3:_]").inner()).unwrap()), true);
    assert_eq!(vec.excludes(&c.predicate(&test_expr_parse("[_, _, 3:_]").inner()).unwrap()), false);
    assert_eq!(vec.excludes(&c.predicate(&test_expr_parse("[5:_]").inner()).unwrap()), false);

    let nested_vec = c.predicate(&test_expr_parse("[3:[2:[#a, #b], #c], #d]").inner()).unwrap();
    assert_eq!(nested_vec.test(&Value::Vector(vec![
        Value::Symbol("a".into()),
        Value::Symbol("b".into()),
        Value::Symbol("c".into()),
        Value::Symbol("d".into()),
    ])), true);
}
