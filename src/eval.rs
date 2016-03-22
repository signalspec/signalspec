use data::{ Value, Type };
use session::{ValueID};
use std::fmt;

/// Element of Expr::Concat
#[derive(PartialEq, Debug, Clone)]
pub enum ConcatElem {
    /// An `Expr` included directly in a concatenation (`[a]`)
    Elem(Expr),

    /// An `Expr` for a `Vector` value whose elements are included in a concatenation (`[8:a]`)
    Slice(Expr, usize),
}

impl ConcatElem {
    fn elem_type(&self) -> Type {
        match *self {
            ConcatElem::Elem(ref v) => v.get_type(),
            ConcatElem::Slice(ref e, count) => {
                if let Type::Vector(c, box ref t) = e.get_type() {
                    assert!(c == count);
                    t.clone()
                } else {
                    panic!("Concatenated slice is not a vector")
                }
            }
        }
    }

    fn elem_count(&self) -> usize {
        match *self {
            ConcatElem::Elem(..) => 1,
            ConcatElem::Slice(_, s) => s,
        }
    }
}

/// An expression representing a runtime computation
#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ignored,
    Const(Value),
    Variable(ValueID, Type),

    Range(f64, f64),
    RangeInt(i64, i64),

    Union(Vec<Expr>),
    Flip(Box<Expr>, Box<Expr>),
    Choose(Box<Expr>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem>),
    BinaryConst(Box<Expr>, BinOp, f64),
}

impl Expr {
    /// Call `f` with the ID of each runtime variable referenced in the expression
    pub fn each_var(&self, f: &mut FnMut(ValueID)) {
        match *self {
            Expr::Variable(id, _) => f(id),
            Expr::Ignored | Expr::Const(..) | Expr::Range(..) | Expr::RangeInt(..) => (),
            Expr::Choose(ref i, _)
            | Expr::BinaryConst(ref i, _, _) => i.each_var(f),
            Expr::Flip(ref a, ref b) => {
                a.each_var(f);
                b.each_var(f);
            }
            Expr::Union(ref u) => for i in u { i.each_var(f) },
            Expr::Concat(ref l) => {
                for src in l.iter() {
                    match *src {
                        ConcatElem::Elem(ref i) | ConcatElem::Slice(ref i, _) => i.each_var(f),
                    }
                }
            }
        }
    }

    /// Down-evaluate the expression with variables from the given value function.
    pub fn eval_down(&self, state: &Fn(ValueID)->Value) -> Value {
        match *self {
            Expr::Ignored | Expr::Range(..) | Expr::RangeInt(..) | Expr::Union(..) => {
                panic!("{:?} can't be down-evaluated", self)
            }
            Expr::Variable(id, _) => state(id),
            Expr::Const(ref v) => v.clone(),

            Expr::Flip(ref d, _) => d.eval_down(state),
            Expr::Choose(ref e, ref c) => eval_choose(&e.eval_down(state), c).unwrap(),

            Expr::Concat(_) => unimplemented!(),

            Expr::BinaryConst(ref e, op, c) => {
                let v = match e.eval_down(state) {
                    Value::Number(v) => v,
                    _ => panic!("Math on non-number value"),
                };
                Value::Number(op.eval(v, c))
            }
        }
    }

    /// Check whether the expression might fail to match a value on up-evaluation.
    pub fn refutable(&self) -> bool {
        match *self {
            Expr::Ignored => false,
            Expr::Range(..) | Expr::RangeInt(..) => true,
            Expr::Variable(..) => false,
            Expr::Const(..) => true,
            Expr::Flip(_, ref u) => u.refutable(),
            Expr::Union(ref u) => u.iter().all(Expr::refutable),
            Expr::Choose(ref e, _) => e.refutable(),
            Expr::Concat(_) => unimplemented!(),
            Expr::BinaryConst(ref e, _, _) => e.refutable(),
        }
    }

    /// Check whether the expression is ignored in all cases
    pub fn ignored(&self) -> bool {
        match *self {
            Expr::Ignored => true,
            Expr::Range(..) | Expr::RangeInt(..) => false,
            Expr::Union(ref u) => u.iter().all(Expr::ignored),
            Expr::Variable(..) => false,
            Expr::Const(..) => false,
            Expr::Flip(_, ref u) => u.ignored(),
            Expr::Choose(ref e, _) => e.ignored(),
            Expr::Concat(_) => unimplemented!(),
            Expr::BinaryConst(ref e, _, _) => e.ignored(),
        }
    }

    /// Up-evaluate a value. This accepts a value and may write variables
    /// via the passed function. It returns whether the expression matched the value.
    pub fn eval_up(&self, state: &mut FnMut(ValueID, Value) -> bool, v: Value) -> bool {
        match *self {
            Expr::Ignored => true,
            Expr::Range(a, b) => match v {
                Value::Number(n) => n>a && n<b,
                _ => false,
            },
            Expr::RangeInt(a, b) => match v {
                Value::Integer(n) => n>=a && n<=b,
                _ => false,
            },
            Expr::Union(ref u) => {
                u.iter().any(|i| i.eval_up(state, v.clone()))
            }
            Expr::Variable(id, _) => state(id, v),
            Expr::Const(ref p) => &v == p,

            Expr::Flip(_, ref u) => u.eval_up(state, v),
            Expr::Choose(ref e, ref choices) => {
                let r = choices.iter()
                    .find(|& &(_, ref b)|{ *b == v })
                    .map(|&(ref a, _)| a.clone());

                if let Some(v) = r {
                    e.eval_up(state, v)
                } else {
                    false
                }
            },
            Expr::Concat(_) => unimplemented!(),

            Expr::BinaryConst(ref e, op, c) => {
                let n = match v {
                    Value::Number(n) => n,
                    _ => return false,
                };
                e.eval_up(state, Value::Number(op.invert().eval(n, c)))
            }
        }
    }

    /// Return the `Type` for the set of possible values this expression may down-evaluate to or
    /// match on up-evaluation.
    pub fn get_type(&self) -> Type {
        match *self {
            Expr::Ignored => Type::Bottom,
            Expr::Range(lo, hi) => Type::Number(lo, hi),
            Expr::RangeInt(lo, hi) => Type::Integer(lo, hi),
            Expr::Union(ref u) => Type::union_iter(u.iter().map(Expr::get_type)),
            Expr::Variable(_, ref ty) => ty.clone(),
            Expr::Const(ref v) => v.get_type(),
            Expr::Flip(ref d, ref u) => Type::union(d.get_type(), u.get_type()),
            Expr::Choose(_, ref choices) => {
                Type::union_iter(choices.iter().map(|&(_, ref r)| r.get_type()))
            },
            Expr::Concat(ref elems) => {
                let t = box Type::union_iter(elems.iter().map(ConcatElem::elem_type));
                let c = elems.iter().map(ConcatElem::elem_count).sum();
                Type::Vector(c, t)
            },
            Expr::BinaryConst(ref e, op, c) => {
                match e.get_type() {
                    Type::Number(min, max) => {
                        assert!(min <= max);
                        let (a, b) = (op.eval(min, c), op.eval(max, c));
                        if op == BinOp::SubSwap || op == BinOp::DivSwap {
                            assert!(b <= a);
                            Type::Number(b, a)
                        } else {
                            assert!(a <= b);
                            Type::Number(a, b)
                        }
                    }
                    _ => panic!("Arithmetic on non-number type")
                }
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Ignored => write!(f, "_"),
            Expr::Range(a, b) => write!(f, "{}..{}", a, b),
            Expr::RangeInt(a, b) => write!(f, "#{}..#{}", a, b),
            Expr::Variable(id, _) => write!(f, "${}", id),
            Expr::Const(ref p) => write!(f, "{}", p),
            Expr::Flip(box Expr::Ignored, box ref u) => write!(f, ":> {}", u),
            Expr::Flip(box ref d, box Expr::Ignored) => write!(f, "<: {}", d),
            Expr::Flip(box ref d, box ref u) => write!(f, "{}!{}", d, u),
            Expr::Union(ref t) => {
                for (k, i) in t.iter().enumerate() {
                    if k > 0 { try!(write!(f, "|")) }
                    try!(write!(f, "{}", i));
                }
                Ok(())
            }
            Expr::Choose(box ref e, ref choices) => {
                try!(write!(f, "{}[", e));
                for (i, &(ref a, ref b)) in choices.iter().enumerate()  {
                    if i != 0 { try!(write!(f, ", ")); }
                    try!(write!(f, "{}={}", a, b));
                }
                write!(f, "]")
            },
            Expr::Concat(_) => unimplemented!(),

            Expr::BinaryConst(ref e, op, c) => {
                match op {
                    BinOp::Add => write!(f, "{} + {}", e, c),
                    BinOp::Sub => write!(f, "{} - {}", e, c),
                    BinOp::SubSwap => write!(f, "{} - {}", c, e),
                    BinOp::Mul => write!(f, "{} * {}", c, e),
                    BinOp::Div => write!(f, "{} / {}", e, c),
                    BinOp::DivSwap => write!(f, "{} / {}", c, e),
                }
            }
        }
    }
}

/// Binary numeric operators
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BinOp {
    /// a + b
    Add,
    /// a - b
    Sub,
    /// b - a
    SubSwap,
    /// a * b
    Mul,
    /// a / b
    Div,
    /// b / a
    DivSwap,
}

impl BinOp {
    /// a `op` b
    pub fn eval(&self, a: f64, b: f64) -> f64 {
        match *self {
            BinOp::Add     => a + b,
            BinOp::Sub     => a - b,
            BinOp::SubSwap => b - a,
            BinOp::Mul     => a * b,
            BinOp::Div     => a / b,
            BinOp::DivSwap => b / a,
        }
    }

    /// (a `op` b) == (b `op.swap()` a)
    pub fn swap(&self) -> BinOp {
        match *self {
            BinOp::Add     => BinOp::Add,
            BinOp::Sub     => BinOp::SubSwap,
            BinOp::SubSwap => BinOp::Sub,
            BinOp::Mul     => BinOp::Mul,
            BinOp::Div     => BinOp::DivSwap,
            BinOp::DivSwap => BinOp::Div,
        }
    }

    /// ((a `op` b) `op.invert()` b) == a
    pub fn invert(&self) -> BinOp {
        match *self {
            BinOp::Add     => BinOp::Sub,
            BinOp::Sub     => BinOp::Add,
            BinOp::SubSwap => BinOp::SubSwap,
            BinOp::Mul     => BinOp::Div,
            BinOp::Div     => BinOp::Mul,
            BinOp::DivSwap => BinOp::DivSwap,
        }
    }
}


fn eval_choose(v: &Value, choices: &[(Value, Value)]) -> Option<Value> {
    choices.iter()
        .find(|& &(ref a, _)|{ a == v })
        .map(|&(_, ref b)| b.clone())
}

#[test]
fn exprs() {
    use session::Session;
    use grammar;
    use resolve;
    let sess = Session::new(None);
    let scope = resolve::Scope::new();
    let state = |_| unimplemented!();

    fn expr(sess: &Session, scope: &resolve::Scope, e: &str) -> Expr {
        resolve::value(sess, scope, &grammar::valexpr(e).unwrap())
    }

    let two = expr(&sess, &scope, "2");
    assert_eq!(two.eval_down(&state), Value::Number(2.0));
    assert_eq!(two.get_type(), Type::Number(2.0, 2.0));

    let ignore = expr(&sess, &scope, "_");
    assert_eq!(ignore.get_type(), Type::Bottom);

    let down = expr(&sess, &scope, "<: #h");
    assert_eq!(down.get_type(), Type::Symbol(Some("h".to_string()).into_iter().collect()));

    let range = expr(&sess, &scope, "0..5");
    assert_eq!(range.get_type(), Type::Number(0.0, 5.0));

}
