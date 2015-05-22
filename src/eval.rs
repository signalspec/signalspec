use std::collections::VecMap;
use ast::Value;
use session::{ValueID};
use resolve::types::Type;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct DataMode {
    pub down: bool,
    pub up: bool,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ConcatElem {
    Elem(Expr),
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

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ignored,
    Const(Value),
    Variable(ValueID, Type),

    Range(f64, f64),

    Flip(Box<Expr>, Box<Expr>),
    Choose(Box<Expr>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem>),
    BinaryConst(Box<Expr>, BinOp, f64),
}

impl Expr {
    pub fn each_var(&self, f: &mut FnMut(ValueID)) {
        match *self {
            Expr::Variable(id, _) => f(id),
            Expr::Ignored | Expr::Const(..) | Expr::Range(..) => (),
            Expr::Choose(ref i, _)
            | Expr::BinaryConst(ref i, _, _) => i.each_var(f),
            Expr::Flip(ref a, ref b) => {
                a.each_var(f);
                b.each_var(f);
            }
            Expr::Concat(ref l) => {
                for src in l.iter() {
                    match *src {
                        ConcatElem::Elem(ref i) | ConcatElem::Slice(ref i, _) => i.each_var(f),
                    }
                }
            }
        }
    }

    pub fn eval_down(&self, state: &State) -> Value {
        match *self {
            Expr::Ignored | Expr::Range(..) => panic!("{:?} can't be down-evaluated", self),
            Expr::Variable(id, _) => state.get(id).clone(),
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

    pub fn refutable(&self) -> bool {
        match *self {
            Expr::Ignored => false,
            Expr::Range(..) => true,
            Expr::Variable(..) => false,
            Expr::Const(..) => true,
            Expr::Flip(_, ref u) => u.refutable(),
            Expr::Choose(ref e, _) => e.refutable(),
            Expr::Concat(_) => unimplemented!(),
            Expr::BinaryConst(ref e, _, _) => e.refutable(),
        }
    }

    pub fn eval_up(&self, state: &mut State, v: Value) -> bool {
        match *self {
            Expr::Ignored => true,
            Expr::Range(a, b) => match v {
                Value::Number(n) => n>a && n<b,
                _ => false,
            },
            Expr::Variable(id, _) => state.set(id, v),
            Expr::Const(ref p) => &v == p,

            Expr::Flip(_, ref u) => u.eval_up(state, v),
            Expr::Choose(ref e, ref choices) => {
                let r = choices.iter()
                    .find(|& &(_, ref b)|{ value_match(b, &v) })
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

    pub fn get_type(&self) -> Type {
        match *self {
            Expr::Ignored => Type::Bottom,
            Expr::Range(lo, hi) => Type::Number(lo, hi),
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

/// Binary numeric operators
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum BinOp {
    Add,       // a + b
    Sub,       // a - b
    SubSwap,   // b - a

    Mul,       // a * b
    Div,       // a / b
    DivSwap,   // b / a
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


pub struct State {
    registers: VecMap<Value>,
}

impl State {
    pub fn new() -> State {
        let mut s = State {
            registers: VecMap::new()
        };
        // TODO: shouldn't be necessary (we currently read from 0 on ignores)
        s.registers.insert(0, ::ast::Value::Symbol("invalid".to_string()));
        s
    }

    pub fn get(&self, reg: ValueID) -> &Value {
        debug!("get {}: {:?}", reg, self.registers.get(&reg));
        &self.registers[reg]
    }

    pub fn set(&mut self, reg: ValueID, v: Value) -> bool {
        debug!("set {}: {:?}", reg, v);
        // TODO: should assert the register is not already set, once exec doesn't run things twice
        self.registers.insert(reg, v);
        true
    }

    pub fn get_var(&self, v: &::session::Var) -> &Value {
        self.get(v.id)
    }

    fn get_num(&self, reg: ValueID) -> f64 {
        match *self.get(reg) {
            Value::Number(v) => v,
            Value::Integer(v) => v as f64, // TODO: explicit conversion?
            _ => panic!(),
        }
    }

    fn get_vec<'s>(&'s self, reg: ValueID) -> &'s [Value] {
        match *self.get(reg) {
            Value::Vector(ref v) => v,
            _ => panic!(),
        }
    }

}

fn value_match(a: &Value, b: &Value) -> bool {
    a == b
}

pub fn eval_choose(v: &Value, choices: &[(Value, Value)]) -> Option<Value> {
    choices.iter().find(|& &(ref a, _)|{ value_match(a, v) }).map(|&(_, ref b)| b.clone())
}

#[test]
fn exprs() {
    use session::Session;
    use grammar;
    use resolve;
    let sess = Session::new();
    let scope = resolve::scope::Scope::new();
    let state = State::new();

    fn expr<'s>(sess: &'s Session<'s>, scope: &resolve::scope::Scope<'s>, e: &str) -> Expr {
        resolve::expr::value(sess, scope, &grammar::valexpr(e).unwrap())
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
