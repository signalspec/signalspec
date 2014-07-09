use ast::{
	Value,
};
use resolve::context::{ValueID};

pub enum ValueSrc {
	Const(Vec<Value>),
	Dyn(ValueID)
}

pub enum ValOp {
	RangeCheckOp(ValueID, f64, f64),

	ChooseOp(ValueID, Vec<(Value, Value)>),

	CheckOp(ValueID, Value),

	SliceOp(ValueID, /*offset*/ uint, /*length*/ uint),
	ConcatOp(Vec<ValueSrc>),

	BinaryOp(ValueID, BinOp, ValueID),
	BinaryConstOp(ValueID, BinOp, f64),
}

/// Binary numeric operators
#[deriving(PartialEq, Eq)]
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

fn value_match(a: &Value, b: &Value) -> bool {
	a == b
}

pub fn eval_choose(v: &Value, choices: &[(Value, Value)]) -> Option<Value> {
	choices.iter().find(|& &(ref a, _)|{ value_match(a, v) }).map(|&(_, ref b)| b.clone())
}
