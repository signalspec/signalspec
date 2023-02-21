use std::{fmt, collections::HashSet};
use num_complex::Complex;
use num_traits::cast::ToPrimitive;
use crate::{syntax::{ Value, BinOp, Number }, Dir};
use super::{ Item, Type, VarId, LeafItem, predicate::Predicate };

/// Element of Expr::Concat
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum ConcatElem<E> {
    /// An `Expr` included directly in a concatenation (`[a]`)
    Elem(E),

    /// An `Expr` for a `Vector` value whose elements are included in a concatenation (`[8:a]`)
    Slice(E, u32),
}

impl<E> ConcatElem<E> {
    fn map_elem<T>(&self, f: impl FnOnce(&E) -> Option<T>) -> Option<ConcatElem<T>> {
        match *self {
            ConcatElem::Elem(ref e) => Some(ConcatElem::Elem(f(e)?)),
            ConcatElem::Slice(ref e, w) => Some(ConcatElem::Slice(f(e)?, w)),
        }
    }

    pub fn elem_count(&self) -> u32 {
        match *self {
            ConcatElem::Elem(..) => 1,
            ConcatElem::Slice(_, s) => s,
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
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum SignMode {
    None,
    TwosComplement,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum UnaryOp {
    IntToBits { width: u32, signed: SignMode },
    Chunks { width: u32 },
}

impl UnaryOp {
    pub fn eval(&self, v: Value) -> Value {
        match *self {
            UnaryOp::IntToBits { width, .. } => {
                match v {
                    Value::Number(i) => {
                        Value::Vector((0..width).rev().map(|bit| Value::Number((((i.to_i64().unwrap()) >> bit) & 1).into()) ).collect())
                    }
                    e => panic!("Invalid value {} in IntToBits", e)
                }
            },
            UnaryOp::Chunks { width } => {
                match v {
                    Value::Vector(i) => {
                        Value::Vector(i.chunks(width as usize).map(|s| Value::Vector(s.to_vec())).collect())
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
    Variable(VarId, Type, Dir),
    Range(Number, Number),
    Union(Vec<Expr>),
    Flip(Box<Expr>, Box<Expr>),
    Choose(Box<Expr>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem<Expr>>),
    BinaryConst(Box<Expr>, BinOp, Value),
    Unary(Box<Expr>, UnaryOp),
}

impl Expr {
    /// Return the `Type` for the set of possible values this expression may down-evaluate to or
    /// match on up-evaluation.
    pub fn get_type(&self) -> Type {
        match *self {
            Expr::Ignored => Type::Ignored,
            Expr::Range(lo, hi) => Type::Number(lo, hi),
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
                    (Type::Complex, &Value::Number(..)) => Type::Complex,
                    (Type::Number(..), &Value::Complex(..)) => Type::Complex,
                    _ => panic!("Arithmetic type error {} {:?} {}", e, op, c)
                }
            }
            Expr::Unary(_, UnaryOp::IntToBits { width, .. }) => {
                Type::Vector(width, Box::new(Type::Number(0.into(), 1.into())))
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

    pub fn down(&self) -> Option<ExprDn> {
        match *self {
            Expr::Ignored | Expr::Range(..) | Expr::Union(..) | Expr::Variable(_, _, Dir::Up) => None,
            Expr::Variable(id, _, Dir::Dn) => Some(ExprDn::Variable(id)),
            Expr::Const(ref v) => Some(ExprDn::Const(v.clone())),
            Expr::Flip(ref d, _) => d.down(),
            Expr::Choose(ref e, ref c) => Some(ExprDn::Choose(Box::new(e.down()?), c.clone())),
            Expr::Concat(ref c) => Some(ExprDn::Concat(
                c.iter().map(|l| l.map_elem(|e| e.down())).collect::<Option<Vec<_>>>()?
            )),
            Expr::BinaryConst(ref e, op, ref c) => Some(ExprDn::BinaryConst(Box::new(e.down()?), op, c.clone())),
            Expr::Unary(ref e, ref op) => Some(ExprDn::Unary(Box::new(e.down()?), op.clone())),
        }
    }

    pub fn predicate(&self) -> Option<Predicate> {
        match *self {
            Expr::Ignored | Expr::Variable(_, _, Dir::Up) => Some(Predicate::Any),
            Expr::Variable(id, _, Dir::Dn) => Some(Predicate::EqualToDn(ExprDn::Variable(id))),
            Expr::Flip(_, ref up) => up.predicate(),
            Expr::Const(ref v) => Predicate::from_value(v),
            Expr::Range(lo, hi) => Some(Predicate::Range(lo, hi)),
            Expr::Union(ref u) => {
                let mut set = HashSet::new();
                for e in u {
                    match e.predicate()? {
                        Predicate::SymbolSet(s) => set.extend(s),
                        _ => return None,
                    }
                }
                Some(Predicate::SymbolSet(set))
            },
            Expr::Concat(ref parts) => {
                let mut predicates = Vec::with_capacity(parts.len());
                for part in parts {
                    match part.map_elem(|e| e.predicate())? {
                        ConcatElem::Slice(Predicate::Vector(inner), _) => predicates.extend(inner),
                        e => predicates.push(e),
                    }
                }
                Some(Predicate::Vector(predicates))
            }
            Expr::Choose(ref e, _) | Expr::BinaryConst(ref e, _, _) | Expr::Unary(ref e, _) => {
                match e.predicate() {
                    Some(Predicate::Any) => Some(Predicate::Any),
                    Some(Predicate::EqualToDn(..)) => e.down().map(Predicate::EqualToDn), // TODO: accidentally quadratic
                    _ => None
                }
            },
        }
    }

    pub fn eval_const(&self) -> Value {
        self.down().unwrap().eval(&|_| panic!("Runtime variable not expected here"))
    }

    /// Up-evaluate a value. This accepts a value and may write variables
    /// via the passed function. It returns whether the expression matched the value.
    pub fn eval_up(&self, state: &mut dyn FnMut(Dir, VarId, Value) -> bool, v: Value) -> bool {
        match *self {
            Expr::Ignored => true,
            Expr::Range(a, b) => match v {
                Value::Number(n) => n>=a && n<b,
                _ => false,
            },
            Expr::Union(ref u) => {
                u.iter().any(|i| i.eval_up(state, v.clone()))
            }
            Expr::Variable(id, _, dir) => state(dir, id, v),
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
                        ConcatElem::Slice(ref e, w) => e.eval_up(state, Value::Vector(elems.by_ref().take(w as usize).collect()))
                    }
                })
            }

            Expr::BinaryConst(ref e, op, ref c) => {
                match (v, c) {
                    (Value::Number(v), &Value::Number(c)) => e.eval_up(state, Value::Number(op.invert().eval(v, c))),
                    _ => false, // TODO: or type error
                }
            }

            Expr::Unary(ref expr, UnaryOp::IntToBits { width, signed }) => {
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

                        expr.eval_up(state, Value::Number(v.into()))
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
}

/// An expression representing a runtime computation
#[derive(PartialEq, Debug, Clone)]
pub enum ExprDn {
    Const(Value),
    Variable(VarId),
    Choose(Box<ExprDn>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem<ExprDn>>),
    BinaryConst(Box<ExprDn>, BinOp, Value),
    Unary(Box<ExprDn>, UnaryOp),
}

impl ExprDn {
    /// Down-evaluate the expression with variables from the given value function.
    pub fn eval(&self, state: &dyn Fn(VarId)->Value) -> Value {
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
            Expr::Variable(id, _, _) => write!(f, "${}", u32::from(*id)),
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

            Expr::Unary(ref expr, UnaryOp::IntToBits { width, signed }) =>
                write!(f, "convert({:?}, {}, {})", signed, width, expr),

            Expr::Unary(ref expr, UnaryOp::Chunks { width }) =>
                write!(f, "chunks({}, {})", width, expr),
        }
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
                (Some(Item::Leaf(LeafItem::Value(v))), Some(Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(width)))))) => {
                    Ok(Item::Leaf(LeafItem::Value(Expr::Unary(Box::new(v), UnaryOp::IntToBits {
                        width: width.to_u32().ok_or("signed() width must be integer")?,
                        signed: sign_mode,
                    }))))
                }
                _ => Err("Invalid arguments to signed()")
            }
        }
        _ => Err("Invalid arguments to signed()")
    }
}

fn fn_chunks(arg: Item) -> Result<Item, &'static str> {
    match arg {
        Item::Tuple(mut t) => {
            match (t.pop(), t.pop()) { //TODO: cleaner way to move out of vec without reversing order?
                (Some(Item::Leaf(LeafItem::Value(v))), Some(Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(width)))))) => {
                    Ok(Item::Leaf(LeafItem::Value(Expr::Unary(Box::new(v), UnaryOp::Chunks {
                        width: width.to_u32().ok_or("chunks() width must be integer")?,
                    }))))
                }
                _ => Err("Invalid arguments to chunks()")
            }
        }
        _ => Err("Invalid arguments to chunks()")
    }
}

fn fn_complex(arg: Item) -> Result<Item, &'static str> {
    match arg {
        Item::Tuple(t) => {
            assert_eq!(t.len(), 2, "complex(re, im) requires two arguments");
            match (&t[0], &t[1]) {
                (&Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(re)))), &Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(im))))) => {
                    Ok(Item::Leaf(LeafItem::Value(Expr::Const(Value::Complex(Complex::new(re, im))))))
                }
                _ => Err("Invalid arguments to complex()")
            }
        }
        _ => Err("Invalid arguments to complex()")
    }
}

pub fn add_primitive_fns(loader: &mut super::Index) {
    loader.add_primitive_fn("signed", fn_signed);
    loader.add_primitive_fn("unsigned", fn_unsigned);
    loader.add_primitive_fn("chunks", fn_chunks);
    loader.add_primitive_fn("complex", fn_complex);
}

#[cfg(test)]
pub fn test_expr_parse(e: &str) -> Expr {
    use std::sync::Arc;
    use crate::syntax::{parse_expr, SourceFile};
    use crate::core::{Index, Scope, value};

    let mut index = Index::new();
    add_primitive_fns(&mut index);

    let scope = Scope { 
        file: Arc::new(SourceFile::new("<tests>".into(), "".into())),
        names: index.prelude().clone()
    };

    let ast = parse_expr(e).unwrap();
    value(&scope, &ast)
}

#[test]
fn exprs() {
    let two = test_expr_parse("2");
    assert_eq!(two.get_type(), Type::Number(2.into(), 2.into()));

    let decimal = test_expr_parse("1.023");
    let decimal_val = Number::new(1023, 1000);
    assert_eq!(decimal.get_type(), Type::Number(decimal_val, decimal_val));

    let four = test_expr_parse("2 + 2");
    assert_eq!(four.get_type(), Type::Number(4.into(), 4.into()));

    let one_one_i = test_expr_parse("complex(1.0, 0.0) + complex(0, 1)");
    assert_eq!(one_one_i.get_type(), Type::Complex);

    let two_two_i = test_expr_parse("complex(1, 1) * 2");
    assert_eq!(two_two_i.get_type(), Type::Complex);

    let ignore = test_expr_parse("_");
    assert_eq!(ignore.get_type(), Type::Ignored);

    let down = test_expr_parse("<: #h");
    assert_eq!(down.get_type(), Type::Symbol(Some("h".to_string()).into_iter().collect()));

    let range = test_expr_parse("0.0..5.0");
    assert_eq!(range.get_type(), Type::Number(0.into(), 5.into()));

    let fncall = test_expr_parse("((a) => a+3.0)(2.0)");
    assert_eq!(fncall.get_type(), Type::Number(5.into(), 5.into()));
}
