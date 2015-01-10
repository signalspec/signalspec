use std::collections::VecMap;
use ast::Value;
use resolve::context::{ValueID};

#[derive(PartialEq, Show)]
pub enum ValueSrc {
    ConstSlice(Vec<Value>),
    DynElem(ValueID),
    DynSlice(ValueID, usize),
}

#[derive(PartialEq, Show)]
pub enum ValOp {
    Const(Value),
    Check(ValueID, Value),
    RangeCheck(ValueID, f64, f64),

    Choose(ValueID, Vec<(Value, Value)>),

    Slice(ValueID, /*offset*/ usize, /*length*/ usize),
    Elem(ValueID, usize),
    Concat(Vec<ValueSrc>),

    Binary(ValueID, BinOp, ValueID),
    BinaryConst(ValueID, BinOp, f64),
}

impl ValOp {
    pub fn each_dep<F: FnMut(ValueID)>(&self, mut f: F) {
        match *self {
            ValOp::Const(..) => (),
            ValOp::Check(i, _) => f(i),
            ValOp::RangeCheck(i, _, _) => f(i),
            ValOp::Choose(i, _) => f(i),
            ValOp::Slice(i, _, _) => f(i),
            ValOp::Elem(i, _) => f(i),
            ValOp::Concat(ref l) => {
                for src in l.iter() {
                    match *src {
                        ValueSrc::ConstSlice(..) => (),
                        ValueSrc::DynElem(i) => f(i),
                        ValueSrc::DynSlice(i, _) => f(i)
                    }
                }
            }
            ValOp::Binary(l, _, r) => { f(l); f(r) }
            ValOp::BinaryConst(i, _, _) => f(i),
        }
    }

    pub fn all_deps<F: Fn(ValueID) -> bool>(&self, f: F) -> bool {
        let mut r = true;
        self.each_dep(|id| r &= f(id));
        r
    }
}

/// Binary numeric operators
#[derive(Copy, PartialEq, Eq, Show)]
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

#[derive(Show)]
pub struct Ops {
    pub entry: Vec<(ValueID, ValOp)>,
    pub exit: Vec<(ValueID, ValOp)>,
}

impl Ops {
    pub fn new() -> Ops { Ops{ entry: Vec::new(), exit: Vec::new() } }
    pub fn count(&self) -> usize { self.entry.len() + self.exit.len() }
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
        match v.up {
            ::resolve::scope::Dynamic(id) => self.get(id),
            _ => panic!("Reading var that is not set in the up direction"),
        }
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
            Value::Vector(ref v) => v.as_slice(),
            _ => panic!(),
        }
    }

    fn execute(&mut self, dest: ValueID, op: &ValOp) -> bool {
        debug!("execute: {} = {:?}", dest, op);
        let v = match *op {
            ValOp::Const(ref v) => v.clone(),
            ValOp::Check(reg, ref v) => return self.get(reg) == v,
            ValOp::RangeCheck(reg, min, max) => {
                let v = self.get_num(reg);
                return v >= min && v <= max;
            }

            ValOp::Choose(reg, ref arms) =>
                eval_choose(self.get(reg), arms.as_slice()).unwrap(),

            ValOp::Elem(reg, index) => self.get_vec(reg)[index].clone(),

            ValOp::Slice(reg, offset, length) =>
                Value::Vector(self.get_vec(reg).slice(offset, offset+length).to_vec()),

            ValOp::Concat(ref parts) => {
                let mut v = Vec::new();
                for part in parts.iter() {
                    match *part {
                        ValueSrc::ConstSlice(ref s) => v.extend(s.iter().map(|x| x.clone())),
                        ValueSrc::DynElem(reg) => v.push(self.get(reg).clone()),
                        ValueSrc::DynSlice(reg, len) => {
                            let s = self.get_vec(reg);
                            assert!(s.len() == len);
                            v.extend(s.iter().map(|x| x.clone()))
                        }
                    }
                }
                Value::Vector(v)
            },

            ValOp::Binary(a, op, b) =>
                Value::Number(op.eval(self.get_num(a), self.get_num(b))),
            ValOp::BinaryConst(a, op, b)  =>
                Value::Number(op.eval(self.get_num(a), b)),
        };
        self.set(dest, v);
        true
    }

    pub fn enter(&mut self, ops: &Ops) {
        for &(dest, ref op) in ops.entry.iter() {
            assert!(self.execute(dest, op));
        }
    }

    pub fn exit(&mut self, ops: &Ops) -> bool {
        let mut success = true;
        for &(dest, ref op) in ops.exit.iter().rev() {
                success &= self.execute(dest, op);
        }
        debug!("success: {:?}", success);
        success
    }
}

fn value_match(a: &Value, b: &Value) -> bool {
    a == b
}

pub fn eval_choose(v: &Value, choices: &[(Value, Value)]) -> Option<Value> {
    choices.iter().find(|& &(ref a, _)|{ value_match(a, v) }).map(|&(_, ref b)| b.clone())
}
