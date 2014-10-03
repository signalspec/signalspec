use std::collections::SmallIntMap;
use ast:: {
    Value,
        NumberValue,
        VectorValue,
};
use resolve::context::{ValueID};

#[deriving(PartialEq, Show)]
pub enum ValueSrc {
    ConstSlice(Vec<Value>),
    DynElem(ValueID),
    DynSlice(ValueID, uint),
}

#[deriving(PartialEq, Show)]
pub enum ValOp {
    ConstOp(Value),
    CheckOp(ValueID, Value),
    RangeCheckOp(ValueID, f64, f64),

    ChooseOp(ValueID, Vec<(Value, Value)>),

    SliceOp(ValueID, /*offset*/ uint, /*length*/ uint),
    ElemOp(ValueID, uint),
    ConcatOp(Vec<ValueSrc>),

    BinaryOp(ValueID, BinOp, ValueID),
    BinaryConstOp(ValueID, BinOp, f64),
}

/// Binary numeric operators
#[deriving(PartialEq, Eq, Show)]
pub enum BinOp {
    BiAdd,       // a + b
    BiSub,       // a - b
    BiSubSwap,   // b - a

    BiMul,       // a * b
    BiDiv,       // a / b
    BiDivSwap,   // b / a
}

impl BinOp {
    /// a `op` b
    pub fn eval(&self, a: f64, b: f64) -> f64 {
        match *self {
            BiAdd     => a + b,
            BiSub     => a - b,
            BiSubSwap => b - a,
            BiMul     => a * b,
            BiDiv     => a / b,
            BiDivSwap => b / a,
        }
    }

    /// (a `op` b) == (b `op.swap()` a)
    pub fn swap(&self) -> BinOp {
        match *self {
            BiAdd     => BiAdd,
            BiSub     => BiSubSwap,
            BiSubSwap => BiSub,
            BiMul     => BiMul,
            BiDiv     => BiDivSwap,
            BiDivSwap => BiDiv,
        }
    }

    /// ((a `op` b) `op.invert()` b) == a
    pub fn invert(&self) -> BinOp {
        match *self {
            BiAdd     => BiSub,
            BiSub     => BiAdd,
            BiSubSwap => BiSubSwap,
            BiMul     => BiDiv,
            BiDiv     => BiMul,
            BiDivSwap => BiDivSwap,
        }
    }
}

#[deriving(Show)]
pub struct Ops {
    pub entry: Vec<(ValueID, ValOp)>,
    pub exit: Vec<(ValueID, ValOp)>,
}

impl Ops {
    pub fn new() -> Ops { Ops{ entry: Vec::new(), exit: Vec::new() } }
    pub fn count(&self) -> uint { self.entry.len() + self.exit.len() }
}

pub struct State {
    registers: SmallIntMap<Value>,
}

impl State {
    pub fn new() -> State {
        let mut s = State {
            registers: SmallIntMap::new()
        };
        // TODO: shouldn't be necessary (we currently read from 0 on ignores)
        s.registers.insert(0, ::ast::SymbolValue("invalid".to_string()));
        s
    }

    pub fn get(&self, reg: ValueID) -> &Value {
        debug!("get {}: {}", reg, self.registers.find(&reg));
        &self.registers[reg]
    }

    pub fn set(&mut self, reg: ValueID, v: Value) -> bool {
        debug!("set {}: {}", reg, v);
        // TODO: should assert the register is not already set, once exec doesn't run things twice
        self.registers.insert(reg, v);
        true
    }

    fn get_num(&self, reg: ValueID) -> f64 {
        match *self.get(reg) {
            NumberValue(v) => v,
            _ => fail!(),
        }
    }

    fn get_vec<'s>(&'s self, reg: ValueID) -> &'s [Value] {
        match *self.get(reg) {
            VectorValue(ref v) => v.as_slice(),
            _ => fail!(),
        }
    }

    fn execute(&mut self, dest: ValueID, op: &ValOp) -> bool {
        debug!("execute: {} = {}", dest, op);
        let v = match *op {
            ConstOp(ref v) => v.clone(),
            CheckOp(reg, ref v) => return self.get(reg) == v,
            RangeCheckOp(reg, min, max) => {
                let v = self.get_num(reg);
                return v >= min && v <= max;
            }

            ChooseOp(reg, ref arms) =>
                eval_choose(self.get(reg), arms.as_slice()).unwrap(),

            ElemOp(reg, index) => self.get_vec(reg)[index].clone(),

            SliceOp(reg, offset, length) =>
                VectorValue(self.get_vec(reg).slice(offset, offset+length).to_vec()),

            ConcatOp(ref parts) => {
                let mut v = Vec::new();
                for part in parts.iter() {
                    match *part {
                        ConstSlice(ref s) => v.extend(s.iter().map(|x| x.clone())),
                        DynElem(reg) => v.push(self.get(reg).clone()),
                        DynSlice(reg, len) => {
                            let s = self.get_vec(reg);
                            assert!(s.len() == len);
                            v.extend(s.iter().map(|x| x.clone()))
                        }
                    }
                }
                VectorValue(v)
            },

            BinaryOp(a, op, b) =>
                NumberValue(op.eval(self.get_num(a), self.get_num(b))),
            BinaryConstOp(a, op, b)  =>
                NumberValue(op.eval(self.get_num(a), b)),
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
        debug!("success: {}", success);
        success
    }

    pub fn tx_message(&self, down: &[ValueID]) -> Vec<Value> {
        down.iter().map(|&x| self.get(x).clone()).collect()
    }

    pub fn rx_message(&mut self, up: &[ValueID], received: Vec<Value>) {
        for (&id, value) in up.iter().zip(received.iter()) {
            self.set(id, value.clone());
        }
    }
}

fn value_match(a: &Value, b: &Value) -> bool {
    a == b
}

pub fn eval_choose(v: &Value, choices: &[(Value, Value)]) -> Option<Value> {
    choices.iter().find(|& &(ref a, _)|{ value_match(a, v) }).map(|&(_, ref b)| b.clone())
}
