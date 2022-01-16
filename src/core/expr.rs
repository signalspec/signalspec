use std::fmt;
use num_complex::Complex;
use crate::syntax::{ Value, BinOp };
use super::{ Item, Type, ValueId, DataMode };

/// Element of Expr::Concat
#[derive(PartialEq, Debug, Clone)]
pub enum ConcatElem<E> {
    /// An `Expr` included directly in a concatenation (`[a]`)
    Elem(E),

    /// An `Expr` for a `Vector` value whose elements are included in a concatenation (`[8:a]`)
    Slice(E, usize),
}

impl<E> ConcatElem<E> {
    fn map<T>(&self, f: impl FnOnce(&E) -> T) -> ConcatElem<T> {
        match *self {
            ConcatElem::Elem(ref e) => ConcatElem::Elem(f(e)),
            ConcatElem::Slice(ref e, s) => ConcatElem::Slice(f(e), s),
        }
    }

    fn expr(&self) -> &E {
        match self {
            ConcatElem::Elem(e) | ConcatElem::Slice(e, _) => e,
        }
    }
}

impl ConcatElem<Expr> {
    fn elem_type(&self) -> Type {
        match *self {
            ConcatElem::Elem(ref v) => v.get_type(),
            ConcatElem::Slice(ref e, count) => {
                if let Type::Vector(c, t) = e.get_type() {
                    assert!(c == count);
                    (*t).clone()
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

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOp {
    FloatToInt,
    IntToBits { width: usize, signed: SignMode },
    Chunks { width: usize },
}

impl UnaryOp {
    pub fn eval(&self, v: Value) -> Value {
        match *self {
            UnaryOp::FloatToInt => {
                match v {
                    Value::Number(n) => Value::Integer(n as i64),
                    e => panic!("Invalid value {} in FloatToInt", e)
                }
            }
            UnaryOp::IntToBits { width, .. } => {
                match v {
                    Value::Integer(i) => {
                        Value::Vector((0..width).rev().map(|bit| Value::Integer((((i as u64) >> bit) & 1) as i64) ).collect())
                    }
                    e => panic!("Invalid value {} in IntToBits", e)
                }
            },
            UnaryOp::Chunks { width } => {
                match v {
                    Value::Vector(i) => {
                        Value::Vector(i.chunks(width).map(|s| Value::Vector(s.to_vec())).collect())
                    }
                    e => panic!("Invalid value {} in Chunks", e)
                }
            },
        }
    }
}

/// An expression representing a runtime computation
#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ignored,
    Const(Value),
    Variable(ValueId, Type, DataMode),

    Range(f64, f64),
    RangeInt(i64, i64),

    Union(Vec<Expr>),
    Flip(Box<Expr>, Box<Expr>),
    Choose(Box<Expr>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem<Expr>>),
    BinaryConst(Box<Expr>, BinOp, Value),
    Unary(Box<Expr>, UnaryOp),
}

impl Expr {
    pub fn dir(&self) -> DataMode {
        use self::Expr::*;
        match *self {
            Ignored => DataMode { down: false, up: true },
            Range(..) | RangeInt(..) => DataMode { down: false, up: true },
            Union(..) => DataMode { down: false, up: true },
            Variable(_, _, dir) => dir,
            Const(..) => DataMode { down: true, up: true },
            Flip(ref d, ref u) => DataMode{ down: d.dir().down, up: u.dir().up },
            Concat(ref e) => e.iter().fold(DataMode { down: true, up: true }, |d, x| {
                match *x {
                    ConcatElem::Elem(ref e) | ConcatElem::Slice(ref e, _) => {
                        let dir = e.dir();
                        DataMode { down: dir.down && d.down, up: dir.up && d.up}
                    },
                }
            }),
            Choose(ref expr, _)
            | BinaryConst(ref expr, _, _)
            | Unary(ref expr, _) => expr.dir(),
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
            Expr::Variable(_, ref ty, _) => ty.clone(),
            Expr::Const(ref v) => v.get_type(),
            Expr::Flip(ref d, ref u) => Type::union(d.get_type(), u.get_type()),
            Expr::Choose(_, ref choices) => {
                Type::union_iter(choices.iter().map(|&(_, ref r)| r.get_type()))
            },
            Expr::Concat(ref elems) => {
                let t = Box::new(Type::union_iter(elems.iter().map(ConcatElem::elem_type)));
                let c = elems.iter().map(ConcatElem::elem_count).sum();
                Type::Vector(c, t)
            },
            Expr::BinaryConst(ref e, op, ref c) => {
                match (e.get_type(), c) {
                    (Type::Number(min, max), &Value::Number(c)) => {
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
                    (Type::Integer(min, max), &Value::Integer(c)) => {
                        assert!(min <= max);
                        let (a, b) = (op.eval(min, c), op.eval(max, c));
                        if op == BinOp::SubSwap || op == BinOp::DivSwap {
                            assert!(b <= a);
                            Type::Integer(b, a)
                        } else {
                            assert!(a <= b);
                            Type::Integer(a, b)
                        }
                    }
                    (Type::Complex, &Value::Number(..)) => Type::Complex,
                    (Type::Number(..), &Value::Complex(..)) => Type::Complex,
                    _ => panic!("Arithmetic type error {} {:?} {}", e, op, c)
                }
            }

            Expr::Unary(ref expr, UnaryOp::FloatToInt) => {
                match expr.get_type() {
                    Type::Number(min, max) => Type::Integer(min as i64, max as i64),
                    _ => panic!("int() requires float argument")
                }
            }
            Expr::Unary(_, UnaryOp::IntToBits { width, .. }) => {
                Type::Vector(width, Box::new(Type::Integer(0, 1)))
            }

            Expr::Unary(ref expr, UnaryOp::Chunks { width }) => {
                if let Type::Vector(c, t) = expr.get_type() {
                    Type::Vector(c/width, Box::new(Type::Vector(width, t)))
                } else {
                    panic!("Chunks argument must be a vector");
                }
            }
        }
    }

    pub fn down(&self) -> ExprDn {
        match *self {
            Expr::Ignored | Expr::Range(..) | Expr::RangeInt(..) | Expr::Union(..) => {
                panic!("{:?} can't be down-evaluated", self)
            }
            Expr::Variable(id, ..) => ExprDn::Variable(id),
            Expr::Const(ref v) => ExprDn::Const(v.clone()),
            Expr::Flip(ref d, _) => d.down(),
            Expr::Choose(ref e, ref c) => ExprDn::Choose(Box::new(e.down()), c.clone()),
            Expr::Concat(ref c) => ExprDn::Concat(c.iter().map(|l| l.map(Self::down)).collect()),
            Expr::BinaryConst(ref e, op, ref c) => ExprDn::BinaryConst(Box::new(e.down()), op, c.clone()),
            Expr::Unary(ref e, ref op) => ExprDn::Unary(Box::new(e.down()), op.clone()),
        }
    }

    pub fn eval_const(&self) -> Value {
        self.down().eval(&|_| panic!("Runtime variable not expected here"))
    }

    /// Up-evaluate a value. This accepts a value and may write variables
    /// via the passed function. It returns whether the expression matched the value.
    pub fn eval_up(&self, state: &mut dyn FnMut(ValueId, Value) -> bool, v: Value) -> bool {
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
            Expr::Variable(id, _, _) => state(id, v),
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

            Expr::Concat(ref components) => {
                let mut elems = match v {
                    Value::Vector(e) => e.into_iter(),
                    other => panic!("Concat expected a vector, found {}", other)
                };

                components.iter().all(|component| {
                    match *component {
                        ConcatElem::Elem(ref e) => e.eval_up(state, elems.next().expect("not enough elements in slice")),
                        ConcatElem::Slice(ref e, w) => e.eval_up(state, Value::Vector(elems.by_ref().take(w).collect()))
                    }
                })
            }

            Expr::BinaryConst(ref e, op, ref c) => {
                match (v, c) {
                    (Value::Number(v), &Value::Number(c)) => e.eval_up(state, Value::Number(op.invert().eval(v, c))),
                    (Value::Integer(v), &Value::Integer(c)) => e.eval_up(state, Value::Integer(op.invert().eval(v, c))),
                    _ => false, // TODO: or type error
                }
            }

            Expr::Unary(ref e, UnaryOp::FloatToInt) => {
                match v {
                    Value::Integer(n) => e.eval_up(state, Value::Number(n as f64)),
                    e => panic!("Invalid value {} up-evaluating FloatToInt", e)
                }
            }

            Expr::Unary(ref expr, UnaryOp::IntToBits { width, signed }) => {
                match v {
                    Value::Vector(bits) => {
                        assert_eq!(bits.len(), width);

                        let v = bits.iter().fold(0, |acc, x| {
                            match *x {
                                Value::Integer(bit) => (acc << 1) | (bit as u64),
                                _ => panic!("Expected bit")
                            }
                        });

                        let v = match signed {
                            SignMode::TwosComplement => ((v << (64-width)) as i64) >> (64-width), // sign-extend
                            SignMode::None => v as i64
                        };

                        expr.eval_up(state, Value::Integer(v))
                    }
                    e => panic!("Invalid value {} up-evaluating IntToBits", e)
                }
            }

            Expr::Unary(ref expr, UnaryOp::Chunks {.. }) => {
                match v {
                    Value::Vector(chunks) => {
                        let concat = chunks.into_iter().flat_map(|c| {
                            match c { Value::Vector(cv) => cv.into_iter(), e => panic!("Expected vector in chunks {}", e)}
                        }).collect();
                        expr.eval_up(state, Value::Vector(concat))
                    }
                    e => panic!("Expected outer vector in Chunks {}", e)
                }
            }
        }
    }

    pub fn excludes(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Const(v1), Expr::Const(v2)) => v1 != v2,
            (Expr::Range(lo1, hi1), Expr::Range(lo2, hi2)) => hi1 < lo2 || hi2 < lo1,
            (Expr::RangeInt(lo1, hi1), Expr::RangeInt(lo2, hi2)) =>  hi1 < lo2 || hi2 < lo1,
            (Expr::Union(u), e2) => u.iter().all(|e1| e1.excludes(e2)),
            (e1, Expr::Union(u)) => u.iter().all(|e2| e1.excludes(e2)),
            (e1, Expr::Flip(_, e2)) => e1.excludes(e2),
            (Expr::Flip(_, e1), e2) => e1.excludes(e2),
            _ => false
        }
    }

    pub fn refutable(&self) -> bool {
        match self {
            Expr::Ignored => false,
            Expr::Const(_) => true,
            Expr::Variable(_, _, _) => false,
            Expr::Range(_, _) => true,
            Expr::RangeInt(_, _) => true,
            Expr::Union(u) => u.iter().all(|e| e.refutable()),
            Expr::Flip(_, u) => u.refutable(),
            Expr::Choose(_, _) => true,
            Expr::Concat(u) => u.iter().any(|e| e.expr().refutable()),
            Expr::BinaryConst(e, _, _) => e.refutable(),
            Expr::Unary(e, _) => e.refutable(),
        }
    }
}

/// An expression representing a runtime computation
#[derive(PartialEq, Debug, Clone)]
pub enum ExprDn {
    Const(Value),
    Variable(ValueId),
    Choose(Box<ExprDn>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem<ExprDn>>),
    BinaryConst(Box<ExprDn>, BinOp, Value),
    Unary(Box<ExprDn>, UnaryOp),
}

impl ExprDn {
    /// Down-evaluate the expression with variables from the given value function.
    pub fn eval(&self, state: &dyn Fn(ValueId)->Value) -> Value {
        match *self {
            ExprDn::Variable(id) => state(id),
            ExprDn::Const(ref v) => v.clone(),

            ExprDn::Choose(ref e, ref c) => eval_choose(&e.eval(state), c).unwrap(),

            ExprDn::Concat(ref components) => {
                let mut result = vec![];
                for component in components {
                    match component {
                        ConcatElem::Elem(e) => result.push(e.eval(state)),
                        ConcatElem::Slice(e, w) => {
                            match e.eval(state) {
                                Value::Vector(v) => result.extend(v.into_iter()),
                                other => panic!("Slice splat expected vector of length {}, found {}", w, other)
                            }
                        }
                    }
                }
                Value::Vector(result)
            },

            ExprDn::BinaryConst(ref e, op, ref c) => {
                match (e.eval(state), c) {
                    (Value::Number(l), &Value::Number(r)) => Value::Number(op.eval(l, r)),
                    (Value::Integer(l), &Value::Integer(r)) => Value::Integer(op.eval(l, r)),
                    (l, r) => panic!("Invalid types {} {} in BinaryConst", l, r)
                }
            }

            ExprDn::Unary(ref e, ref op) => {
                op.eval(e.eval(state))
            }
        }
    }
}


fn eval_choose(v: &Value, choices: &[(Value, Value)]) -> Option<Value> {
    choices.iter()
        .find(|& &(ref a, _)|{ a == v })
        .map(|&(_, ref b)| b.clone())
}


impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Ignored => write!(f, "_"),
            Expr::Range(a, b) => write!(f, "{}..{}", a, b),
            Expr::RangeInt(a, b) => write!(f, "#{}..#{}", a, b),
            Expr::Variable(id, _, _) => write!(f, "${}", id),
            Expr::Const(ref p) => write!(f, "{}", p),
            Expr::Flip(ref d, ref u) if **d == Expr::Ignored => write!(f, ":> {}", u),
            Expr::Flip(ref d, ref u) if **u == Expr::Ignored => write!(f, "<: {}", d),
            Expr::Flip(d, u) => write!(f, "{}!{}", d, u),
            Expr::Union(ref t) => {
                for (k, i) in t.iter().enumerate() {
                    if k > 0 { write!(f, "|")? }
                    write!(f, "{}", i)?;
                }
                Ok(())
            }
            Expr::Choose(e, choices) => {
                write!(f, "{}[", e)?;
                for (i, &(ref a, ref b)) in choices.iter().enumerate()  {
                    if i != 0 { write!(f, ", ")?; }
                    write!(f, "{}={}", a, b)?;
                }
                write!(f, "]")
            },
            Expr::Concat(_) => unimplemented!(),

            Expr::BinaryConst(ref e, op, ref c) => {
                match op {
                    BinOp::Add => write!(f, "{} + {}", e, c),
                    BinOp::Sub => write!(f, "{} - {}", e, c),
                    BinOp::SubSwap => write!(f, "{} - {}", c, e),
                    BinOp::Mul => write!(f, "{} * {}", c, e),
                    BinOp::Div => write!(f, "{} / {}", e, c),
                    BinOp::DivSwap => write!(f, "{} / {}", c, e),
                }
            }

            Expr::Unary(ref e, UnaryOp::FloatToInt) =>
                write!(f, "int({})", e),

            Expr::Unary(ref expr, UnaryOp::IntToBits { width, signed }) =>
                write!(f, "convert({:?}, {}, {})", signed, width, expr),

            Expr::Unary(ref expr, UnaryOp::Chunks { width }) =>
                write!(f, "chunks({}, {})", width, expr),
        }
    }
}

fn fn_int(arg: Item) -> Result<Item, &'static str> {
    match arg {
        Item::Value(v) => {
            Ok(Item::Value(Expr::Unary(Box::new(v), UnaryOp::FloatToInt)))
        }
        _ => return Err("Invalid arguments to chunks()")
    }
}

fn fn_unsigned(arg: Item) -> Result<Item, &'static str> {
    fn_signed_unsigned(arg, SignMode::None)
}

fn fn_signed(arg: Item) -> Result<Item, &'static str> {
    fn_signed_unsigned(arg, SignMode::TwosComplement)
}

fn fn_signed_unsigned(arg: Item, sign_mode: SignMode) -> Result<Item, &'static str> {
    match arg {
        Item::Tuple(mut t) => {
            match (t.pop(), t.pop()) { //TODO: cleaner way to move out of vec without reversing order?
                (Some(Item::Value(v)), Some(Item::Value(Expr::Const(Value::Integer(width))))) => {
                    Ok(Item::Value(Expr::Unary(Box::new(v), UnaryOp::IntToBits {
                        width: width as usize,
                        signed: sign_mode,
                    })))
                }
                _ => return Err("Invalid arguments to signed()")
            }
        }
        _ => return Err("Invalid arguments to signed()")
    }
}

fn fn_chunks(arg: Item) -> Result<Item, &'static str> {
    match arg {
        Item::Tuple(mut t) => {
            match (t.pop(), t.pop()) { //TODO: cleaner way to move out of vec without reversing order?
                (Some(Item::Value(v)), Some(Item::Value(Expr::Const(Value::Integer(width))))) => {
                    Ok(Item::Value(Expr::Unary(Box::new(v), UnaryOp::Chunks {
                        width: width as usize,
                    })))
                }
                _ => return Err("Invalid arguments to chunks()")
            }
        }
        _ => return Err("Invalid arguments to chunks()")
    }
}

fn fn_complex(arg: Item) -> Result<Item, &'static str> {
    match arg {
        Item::Tuple(t) => {
            assert_eq!(t.len(), 2, "complex(re, im) requires two arguments");
            match (&t[0], &t[1]) {
                (&Item::Value(Expr::Const(Value::Number(re))), &Item::Value(Expr::Const(Value::Number(im)))) => {
                    Ok(Item::Value(Expr::Const(Value::Complex(Complex::new(re, im)))))
                }
                _ => return Err("Invalid arguments to complex()")
            }
        }
        _ => return Err("Invalid arguments to complex()")
    }
}

pub fn add_primitive_fns(loader: &mut super::Index) {
    loader.add_primitive_fn("int", fn_int);
    loader.add_primitive_fn("signed", fn_signed);
    loader.add_primitive_fn("unsigned", fn_unsigned);
    loader.add_primitive_fn("chunks", fn_chunks);
    loader.add_primitive_fn("complex", fn_complex);
}

#[test]
fn exprs() {
    use crate::syntax::parse_valexpr;
    use crate::core::{Index, value};

    let mut index = Index::new();

    index.add_primitive_fn("complex", fn_complex);
    let scope = index.prelude.child();

    let expr = |e: &str| {
        let ast = parse_valexpr(e).unwrap();
        value(&scope, &ast)
    };

    let two = expr("2.");
    assert_eq!(two.get_type(), Type::Number(2.0, 2.0));

    let four = expr("2. + 2.");
    assert_eq!(four.get_type(), Type::Number(4.0, 4.0));

    let fiveint = expr("2 + 3");
    assert_eq!(fiveint.get_type(), Type::Integer(5, 5));

    let one_one_i = expr("complex(1., 0.) + complex(0., 1.)");
    assert_eq!(one_one_i.get_type(), Type::Complex);

    let two_two_i = expr("complex(1., 1.) * 2.");
    assert_eq!(two_two_i.get_type(), Type::Complex);

    let ignore = expr("_");
    assert_eq!(ignore.get_type(), Type::Bottom);

    let down = expr("<: #h");
    assert_eq!(down.get_type(), Type::Symbol(Some("h".to_string()).into_iter().collect()));

    let range = expr("0.0..5.0");
    assert_eq!(range.get_type(), Type::Number(0.0, 5.0));

    let fncall = expr("((a) => a+3.0)(2.0)");
    assert_eq!(fncall.get_type(), Type::Number(5., 5.));
}
