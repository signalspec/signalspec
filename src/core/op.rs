use num_complex::Complex;
use num_traits::ToPrimitive;

use crate::syntax::{BinOp, Number};

use crate::Value;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum UnaryOp {
    BinaryConstNumber(BinOp, Number),
    Mapping(Vec<(Value, Value)>),
    IntToBits { width: u32, signed: SignMode },
    BitsToInt { width: u32, signed: SignMode },
    Chunks { width: u32 },
    Merge { width: u32 },
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum SignMode {
    None,
    TwosComplement,
}

impl UnaryOp {
    pub fn eval(&self, v: Value) -> Value {
        match *self {
            UnaryOp::BinaryConstNumber(op, r) => {
                match v {
                    Value::Number(l) => Value::Number(op.eval(l, r)),
                    l => panic!("Invalid types {} {} in BinaryConst", l, r)
                }
            }
            UnaryOp::Mapping(ref m) => {
                eval_choose(&v, m).unwrap()
            }
            UnaryOp::IntToBits { width, .. } => {
                match v {
                    Value::Number(i) => eval_int_to_bits(width, i),
                    e => panic!("Invalid value {} in IntToBits", e)
                }
            },
            UnaryOp::BitsToInt { width, signed } => {
                match v {
                    Value::Vector(bits) => {
                        assert_eq!(bits.len(), width as usize);

                        let v = bits.iter().fold(0, |acc, x| {
                            match *x {
                                Value::Number(bit) => ((acc << 1) | (bit.to_i64().unwrap())).into(),
                                _ => panic!("Expected bit")
                            }
                        });

                        let v = match signed {
                            SignMode::TwosComplement => ((v << (64-width)) as i64) >> (64-width), // sign-extend
                            SignMode::None => v as i64
                        };

                        Value::Number(v.into())
                    }
                    e => panic!("Invalid value {} in BitsToInt", e)
                }

            }
            UnaryOp::Chunks { width } => {
                match v {
                    Value::Vector(i) => eval_chunks(width, i),
                    e => panic!("Invalid value {} in Chunks", e)
                }
            },
            UnaryOp::Merge { .. } => {
                match v {
                    Value::Vector(chunks) => {
                        Value::Vector(chunks.into_iter().flat_map(|c| {
                            match c {
                                Value::Vector(cv) => cv.into_iter(),
                                e => panic!("Expected vector in merge, found {}", e)
                            }
                        }).collect())
                    }
                    e => panic!("Expected outer vector in Merge {}", e)
                }
            }
        }
    }

    pub fn invert(&self) -> UnaryOp {
        match *self {
            UnaryOp::BinaryConstNumber(op, r) => {
                UnaryOp::BinaryConstNumber(op.invert(), r)
            }
            UnaryOp::Mapping(ref m) => {
                UnaryOp::Mapping(m.iter().map(|(v1,v2)| (v2.clone(), v1.clone())).collect())
            }
            UnaryOp::IntToBits { width, signed } => UnaryOp::BitsToInt { width, signed },
            UnaryOp::BitsToInt { width, signed } => UnaryOp::IntToBits { width, signed },
            UnaryOp::Chunks { width } => UnaryOp::Merge { width },
            UnaryOp::Merge { width } => UnaryOp::Chunks { width },
        }
    }
}

pub(crate) fn eval_chunks(width: u32, i: Vec<Value>) -> Value {
    Value::Vector(i.chunks(width as usize).map(|s| Value::Vector(s.to_vec())).collect())
}

pub(crate) fn eval_int_to_bits(width: u32, i: Number) -> Value {
    Value::Vector((0..width).rev().map(|bit| Value::Number((((i.to_i64().unwrap()) >> bit) & 1).into()) ).collect())
}

pub fn eval_choose(v: &Value, choices: &[(Value, Value)]) -> Option<Value> {
    choices.iter()
        .find(|& &(ref a, _)|{ a == v })
        .map(|&(_, ref b)| b.clone())
}

pub fn eval_binary(l: Value, op: BinOp, r: Value) -> Option<Value> {
    Some(match (l, r) {
        (Value::Number(a),  Value::Number(b)) => Value::Number(op.eval(a, b)),
        (Value::Complex(a), Value::Complex(b)) => Value::Complex(op.eval(a, b)),
        (Value::Complex(a), Value::Number(b)) => Value::Complex(op.eval(a, Complex::from(b))),
        (Value::Number(a),  Value::Complex(b)) => Value::Complex(op.eval(Complex::from(a), b)),
        _ => return None
    })
}
