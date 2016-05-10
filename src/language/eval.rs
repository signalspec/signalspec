use super::Item;
use data::{ Value, Type };
use session::{ValueID};
use std::fmt;

/// Element of Expr::Concat
#[derive(PartialEq, Debug, Clone)]
pub enum ConcatElem {
    /// An `Expr` included directly in a concatenation (`[a]`)
    Elem(Expr),

    /// An `Expr` for a `Vector` value whose elements are included in a concatenation (`[8:a]`)
    #[allow(dead_code)]
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

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum SignMode {
    None,
    TwosComplement,
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

    FloatToInt(Box<Expr>),
    IntToBits { width: usize, expr: Box<Expr>, signed: SignMode },
    Chunks { width: usize, expr: Box<Expr> },
}

impl Expr {
    /// Call `f` with the ID of each runtime variable referenced in the expression
    pub fn each_var(&self, f: &mut FnMut(ValueID)) {
        use self::Expr::*;
        match *self {
            Variable(id, _) => f(id),
            Ignored | Const(..) | Range(..) | RangeInt(..) => (),
            Flip(ref a, ref b) => {
                a.each_var(f);
                b.each_var(f);
            }
            Union(ref u) => for i in u { i.each_var(f) },
            Concat(ref l) => {
                for src in l.iter() {
                    match *src {
                        ConcatElem::Elem(ref i) | ConcatElem::Slice(ref i, _) => i.each_var(f),
                    }
                }
            }
            Choose(ref expr, _)
            | BinaryConst(ref expr, _, _)
            | FloatToInt(ref expr)
            | IntToBits { ref expr, .. }
            | Chunks { ref expr, .. } => expr.each_var(f),
        }
    }

    /// Check whether the expression might fail to match a value on up-evaluation.
    pub fn refutable(&self) -> bool {
        use self::Expr::*;
        match *self {
            Ignored => false,
            Range(..) | RangeInt(..) => true,
            Variable(..) => false,
            Const(..) => true,
            Flip(_, ref u) => u.refutable(),
            Union(ref u) => u.iter().all(Expr::refutable),
            Concat(ref e) => e.iter().any(|x| {
                match *x {
                    ConcatElem::Elem(ref e) | ConcatElem::Slice(ref e, _) => e.refutable(),
                }
            }),
            Choose(ref expr, _)
            | BinaryConst(ref expr, _, _)
            | FloatToInt( ref expr)
            | IntToBits { ref expr, .. }
            | Chunks { ref expr, .. } => expr.refutable(),
        }
    }

    /// Check whether the expression is ignored in all cases
    pub fn ignored(&self) -> bool {
        use self::Expr::*;
        match *self {
            Ignored => true,
            Range(..) | RangeInt(..) => false,
            Union(ref u) => u.iter().all(Expr::ignored),
            Variable(..) => false,
            Const(..) => false,
            Flip(_, ref u) => u.ignored(),
            Concat(ref e) => e.iter().all(|x| {
                match *x {
                    ConcatElem::Elem(ref e) | ConcatElem::Slice(ref e, _) => e.ignored(),
                }
            }),
            Choose(ref expr, _)
            | BinaryConst(ref expr, _, _)
            | FloatToInt(ref expr)
            | IntToBits { ref expr, .. }
            | Chunks { ref expr, .. } => expr.ignored(),
        }
    }

    /// Check whether the expression can be down-evaluated
    pub fn down_evaluable(&self) -> bool {
        use self::Expr::*;
        match *self {
            Ignored => false,
            Range(..) | RangeInt(..) => false,
            Union(ref u) => u.iter().all(Expr::down_evaluable),
            Variable(..) => true,
            Const(..) => true,
            Flip(ref d, _) => d.down_evaluable(),
            Concat(ref e) => e.iter().all(|x| {
                match *x {
                    ConcatElem::Elem(ref e) | ConcatElem::Slice(ref e, _) => e.down_evaluable(),
                }
            }),
            Choose(ref expr, _)
            | BinaryConst(ref expr, _, _)
            | FloatToInt(ref expr)
            | IntToBits { ref expr, .. }
            | Chunks { ref expr, .. } => expr.down_evaluable(),
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

            Expr::FloatToInt(ref expr) => {
                match expr.get_type() {
                    Type::Number(min, max) => Type::Integer(min as i64, max as i64),
                    _ => panic!("int() requires float argument")
                }
            }
            Expr::IntToBits { width, .. } => {
                Type::Vector(width, Box::new(Type::Integer(0, 1)))
            }

            Expr::Chunks { ref expr, width } => {
                if let Type::Vector(c, t) = expr.get_type() {
                    Type::Vector(c/width, Box::new(Type::Vector(width, t)))
                } else {
                    panic!("Chunks argument must be a vector");
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

            Expr::FloatToInt(ref e) =>
                write!(f, "int({})", e),

            Expr::IntToBits { ref expr, width, signed } =>
                write!(f, "convert({:?}, {}, {})", signed, width, expr),

            Expr::Chunks { ref expr, width } =>
                write!(f, "chunks({}, {})", width, expr),
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

pub trait PrimitiveFn {
    fn call(&self, super::Item) -> Result<Expr, &'static str>;
}

pub struct FnInt;
impl PrimitiveFn for FnInt {
    fn call(&self, arg: Item) -> Result<Expr, &'static str> {
        match arg {
            Item::Value(v) => {
                Ok(Expr::FloatToInt(Box::new(v)))
            }
            _ => return Err("Invalid arguments to chunks()")
        }
    }
}
pub static FNINT: FnInt = FnInt;


pub struct FnSigned;
impl PrimitiveFn for FnSigned {
    fn call(&self, arg: Item) -> Result<Expr, &'static str> {
        match arg {
            Item::Tuple(mut t) => {
                match (t.pop(), t.pop()) { //TODO: cleaner way to move out of vec without reversing order?
                    (Some(Item::Value(v)), Some(Item::Value(Expr::Const(Value::Integer(width))))) => {
                        Ok(Expr::IntToBits {
                            width: width as usize,
                            signed: SignMode::TwosComplement,
                            expr: Box::new(v)
                        })
                    }
                    _ => return Err("Invalid arguments to signed()")
                }
            }
            _ => return Err("Invalid arguments to signed()")
        }
    }
}
pub static FNSIGNED: FnSigned = FnSigned;

pub struct FnChunks;
impl PrimitiveFn for FnChunks {
    fn call(&self, arg: Item) -> Result<Expr, &'static str> {
        match arg {
            Item::Tuple(mut t) => {
                match (t.pop(), t.pop()) { //TODO: cleaner way to move out of vec without reversing order?
                    (Some(Item::Value(v)), Some(Item::Value(Expr::Const(Value::Integer(width))))) => {
                        Ok(Expr::Chunks {
                            width: width as usize,
                            expr: Box::new(v)
                        })
                    }
                    _ => return Err("Invalid arguments to chunks()")
                }
            }
            _ => return Err("Invalid arguments to chunks()")
        }
    }
}
pub static FNCHUNKS: FnChunks = FnChunks;


#[test]
fn exprs() {
    use session::Session;
    use super::grammar;
    use super::scope::Scope;
    let sess = Session::new(None);
    let scope = Scope::new();

    fn expr(sess: &Session, scope: &Scope, e: &str) -> Expr {
        super::expr::value(sess, scope, &grammar::valexpr(e).unwrap())
    }

    let two = expr(&sess, &scope, "2");
    assert_eq!(two.get_type(), Type::Number(2.0, 2.0));

    let ignore = expr(&sess, &scope, "_");
    assert_eq!(ignore.get_type(), Type::Bottom);

    let down = expr(&sess, &scope, "<: #h");
    assert_eq!(down.get_type(), Type::Symbol(Some("h".to_string()).into_iter().collect()));

    let range = expr(&sess, &scope, "0..5");
    assert_eq!(range.get_type(), Type::Number(0.0, 5.0));

}
