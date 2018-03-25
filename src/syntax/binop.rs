use std::ops::{Add, Sub, Mul, Div};

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
    pub fn eval<A, B, C>(&self, a: A, b: B) -> C where
        A: Add<B, Output=C>,
        A: Sub<B, Output=C>,
        B: Sub<A, Output=C>,
        A: Mul<B, Output=C>,
        A: Div<B, Output=C>,
        B: Div<A, Output=C> {
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
