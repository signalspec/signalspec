use crate::{Value, core::op::ConcatElem, syntax::Number};

use itertools::Itertools;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Predicate {
    /// Always matches
    Any,

    /// Number: in range (bottom inclusive, top-exclusive)
    Range(Number, Number),

    /// Value in set
    AnyOf(Vec<Value>),

    /// Vector: test each slice / element
    Vector(Vec<Predicate>),
}

impl Predicate {
    pub fn from_value(v: &Value) -> Predicate {
        match v {
            Value::Vector(v) => Predicate::Vector(v.iter()
                .map(|v| Predicate::from_value(v))
                .collect::<Vec<_>>()),
            v => Predicate::AnyOf([v.clone()].into_iter().collect())
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
            (Predicate::AnyOf(set), v) => set.contains(v),
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
            (Predicate::AnyOf(set), other) | (other, Predicate::AnyOf(set)) => set.iter().all(|v| !other.test(v)),
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
    assert_eq!(range.excludes(&Predicate::from_value(&Value::Number(10.into()))), true);
    assert_eq!(range.excludes(&Predicate::from_value(&Value::Number(0.into()))), true);
    assert_eq!(range.excludes(&Predicate::from_value(&Value::Number(5.into()))), false);

    let any = c.predicate(&test_expr_parse("<: 5").inner()).unwrap();
    assert_eq!(any, Predicate::Any);
    assert_eq!(any.test(&Value::Number(1.into())), true);
    assert_eq!(any.excludes(&Predicate::Range(5.into(), 20.into())), false);

    let int = c.predicate(&test_expr_parse(":> 5").inner()).unwrap();
    assert_eq!(int, Predicate::from_value(&Value::Number(5.into())));
    assert_eq!(int.test(&Value::Number(5.into())), true);
    assert_eq!(int.test(&Value::Number(4.into())), false);
    assert_eq!(int.excludes(&int), false);
    assert_eq!(int.excludes(&Predicate::from_value(&Value::Number(0.into()))), true);

    let sym = c.predicate(&test_expr_parse("#h | #l").inner()).unwrap();
    assert_eq!(sym, Predicate::AnyOf(["h".into(), "l".into()].into_iter().map(Value::Symbol).collect()));
    assert_eq!(sym.test(&Value::Symbol("h".into())), true);
    assert_eq!(sym.test(&Value::Symbol("z".into())), false);
    assert_eq!(sym.excludes(&Predicate::AnyOf(["h".into(), "x".into()].into_iter().map(Value::Symbol).collect())), false);
    assert_eq!(sym.excludes(&Predicate::AnyOf(["z".into(), "x".into()].into_iter().map(Value::Symbol).collect())), true);

    let vec = c.predicate(&test_expr_parse("[0..2, 2, 3:_]").inner()).unwrap();
    assert_eq!(vec, Predicate::Vector(vec![
        Predicate::Range(0.into(), 2.into()),
        Predicate::AnyOf([Value::Number(2.into())].into()),
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
