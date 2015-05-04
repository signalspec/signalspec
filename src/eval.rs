use std::collections::VecMap;
use ast::Value;
use session::{ValueID};

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

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ignored,
    Const(Value),
    Variable(ValueID),

    Range(f64, f64),

    Flip(Box<Expr>, Box<Expr>),
    Choose(Box<Expr>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem>),
    BinaryConst(Box<Expr>, BinOp, f64),
}

impl Expr {
    pub fn each_var(&self, f: &mut FnMut(ValueID)) {
        match *self {
            Expr::Variable(id) => f(id),
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
            Expr::Variable(id) => state.get(id).clone(),
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

    pub fn mode(&self) -> DataMode {
        match *self {
            Expr::Ignored => DataMode { down: false, up: false },
            Expr::Range(..) => DataMode { down: false, up: true },
            Expr::Variable(..) => DataMode { down: true, up: true }, //TODO: results after inference?
            Expr::Const(..) => DataMode { down: true, up: true },
            Expr::Flip(ref d, ref u) => {
                let d = d.mode();
                let u = u.mode();
                DataMode { down: d.down, up: u.up }
            },
            Expr::Choose(ref e, _) => e.mode(),
            Expr::Concat(_) => unimplemented!(),
            Expr::BinaryConst(ref e, _, _) => e.mode()
        }
    }

    pub fn exists_down(&self) -> bool { self.mode().down }
    pub fn exists_up(&self) -> bool { self.mode().up }

    pub fn limit_direction(self, target: DataMode) -> Expr {
        let mode = self.mode();

        if !target.down && !target.up {
            Expr::Ignored
        } else if !target.up && mode.up {
            Expr::Flip(box self, box Expr::Ignored)
        } else if !target.down && mode.down {
            Expr::Flip(box Expr::Ignored, box self)
        } else {
            self
        }
    }

    pub fn eval_up(&self, state: &mut State, v: Value) -> bool {
        match *self {
            Expr::Ignored => true,
            Expr::Range(a, b) => match v {
                Value::Number(n) => n>a && n<b,
                _ => false,
            },
            Expr::Variable(id) => state.set(id, v),
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

    pub fn get_var(&self, v: ::session::Var) -> &Value {
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
