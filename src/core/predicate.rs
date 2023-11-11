
use std::collections::HashSet;
use crate::{syntax::Number, Value};
use super::ConcatElem;

#[derive(Clone, PartialEq, Debug)]
pub enum Predicate {
    /// Always matches
    Any,

    /// Number: in range (bottom inclusive, top-exclusive)
    Range(Number, Number),

    /// Number: equal
    /// Used for matching a single number, since range is exclusive
    Number(Number),

    /// Symbol: in set
    SymbolSet(HashSet<String>),

    /// Vector: test each slice / element
    Vector(Vec<ConcatElem<Predicate>>),
}

impl Predicate {
    pub fn from_value(v: &Value) -> Option<Predicate> {
        match *v {
            Value::Number(n) => Some(Predicate::Number(n)),
            Value::Symbol(ref s) => Some(Predicate::SymbolSet([s.clone()].into())),
            Value::Vector(ref v) => v.iter()
                .map(|v| Predicate::from_value(v).map(ConcatElem::Elem))
                .collect::<Option<Vec<_>>>().map(Predicate::Vector),
            Value::Complex(_) => None,
        }
    }

    pub fn test(&self, v: &Value) -> bool {
        match (self, v) {
            (Predicate::Any, _) => true,
            (Predicate::Range(lo, hi), Value::Number(n)) => n>=lo && n<hi,
            (Predicate::Number(x), Value::Number(n)) => x == n,
            (Predicate::SymbolSet(set), Value::Symbol(s)) => set.contains(s),
            (Predicate::Vector(components), Value::Vector(vec)) => {
                let mut elems = vec.iter();
                let mut take = || elems.next().expect("not enough elements in slice");

                components.iter().all(|component| {
                    match *component {
                        ConcatElem::Elem(ref e) => e.test(take()),
                        ConcatElem::Slice(ref e, w) => e.test(&Value::Vector((0..w).map(|_| take().clone()).collect())),
                    }
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
                c1.iter().zip(c2.iter()).map_while(|(e1, e2)| {
                    match (e1, e2) {
                        (ConcatElem::Elem(p1), ConcatElem::Elem(p2)) => Some((p1, p2)),
                        (ConcatElem::Slice(p1, w1), ConcatElem::Slice(p2, w2)) if w1 == w2 => Some((p1, p2)),
                        _ => None,
                    }
                }).any(|(p1, p2)| p1.excludes(p2))
            },
            _ => panic!("Predicate type mismatch in `excludes`: {:?}, {:?}", self, other),
        }
    }
}

#[test]
fn test_predicate() {
    use super::expr::test_expr_parse;

    let range = test_expr_parse("1 ! 1..10").predicate().unwrap();
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

    let any = test_expr_parse("<: 5").predicate().unwrap();
    assert_eq!(any, Predicate::Any);
    assert_eq!(any.test(&Value::Number(1.into())), true);
    assert_eq!(any.excludes(&Predicate::Range(5.into(), 20.into())), false);

    let int = test_expr_parse(":> 5").predicate().unwrap();
    assert_eq!(int, Predicate::Number(5.into()));
    assert_eq!(int.test(&Value::Number(5.into())), true);
    assert_eq!(int.test(&Value::Number(4.into())), false);
    assert_eq!(int.excludes(&int), false);
    assert_eq!(int.excludes(&Predicate::Number(0.into())), true);

    let sym = test_expr_parse("#h | #l").predicate().unwrap();
    assert_eq!(sym, Predicate::SymbolSet(["h".into(), "l".into()].into()));
    assert_eq!(sym.test(&Value::Symbol("h".into())), true);
    assert_eq!(sym.test(&Value::Symbol("z".into())), false);
    assert_eq!(sym.excludes(&Predicate::SymbolSet(["h".into(), "x".into()].into())), false);
    assert_eq!(sym.excludes(&Predicate::SymbolSet(["z".into(), "x".into()].into())), true);

    let vec = test_expr_parse("[0..2, 2, 3:_]").predicate().unwrap();
    assert_eq!(vec, Predicate::Vector(vec![
        ConcatElem::Elem(Predicate::Range(0.into(), 2.into())),
        ConcatElem::Elem(Predicate::Number(2.into())),
        ConcatElem::Slice(Predicate::Any, 3),
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
    assert_eq!(vec.excludes(&test_expr_parse("[0..5, 9, 3:_]").predicate().unwrap()), true);
    assert_eq!(vec.excludes(&test_expr_parse("[_, _, 3:_]").predicate().unwrap()), false);
    assert_eq!(vec.excludes(&test_expr_parse("[5:_]").predicate().unwrap()), false);

    let nested_vec = test_expr_parse("[3:[2:[#a, #b], #c], #d]").predicate().unwrap();
    assert_eq!(nested_vec.test(&Value::Vector(vec![
        Value::Symbol("a".into()),
        Value::Symbol("b".into()),
        Value::Symbol("c".into()),
        Value::Symbol("d".into()),
    ])), true);
}
