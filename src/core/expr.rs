use std::{fmt, collections::{HashSet, HashMap}, sync::Arc};
use num_complex::Complex;
use num_traits::cast::ToPrimitive;
use crate::{syntax::{BinOp, Number }, core::{PrimitiveFn, FunctionDef}, Value};
use super::{ predicate::Predicate, resolve::action::ValueSinkId, Item, LeafItem, Type, ValueSrc };

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

    pub fn map_elem_owned<T>(self, f: impl FnOnce(E) -> T) -> ConcatElem<T> {
        match self {
            ConcatElem::Elem(e) => ConcatElem::Elem(f(e)),
            ConcatElem::Slice(e, w) => ConcatElem::Slice(f(e), w),
        }
    }

    pub fn elem_count(&self) -> u32 {
        match *self {
            ConcatElem::Elem(..) => 1,
            ConcatElem::Slice(_, s) => s,
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
    BitsToInt { width: u32, signed: SignMode },
    Chunks { width: u32 },
    Merge { width: u32 },
}

impl UnaryOp {
    pub fn eval(&self, v: Value) -> Value {
        match *self {
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
            UnaryOp::IntToBits { width, signed } => UnaryOp::BitsToInt { width, signed },
            UnaryOp::BitsToInt { width, signed } => UnaryOp::IntToBits { width, signed },
            UnaryOp::Chunks { width } => UnaryOp::Merge { width },
            UnaryOp::Merge { width } => UnaryOp::Chunks { width },
        }
    }
}

fn eval_chunks(width: u32, i: Vec<Value>) -> Value {
    Value::Vector(i.chunks(width as usize).map(|s| Value::Vector(s.to_vec())).collect())
}

fn eval_int_to_bits(width: u32, i: Number) -> Value {
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

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Const(Value),
    Expr(Type, ExprKind),
}

/// An expression representing a runtime computation
#[derive(PartialEq, Debug, Clone)]
pub enum ExprKind {
    Ignored,
    Const(Value),
    VarDn(ExprDn),
    VarUp(ValueSinkId),
    Range(Number, Number),
    Union(Vec<ExprKind>),
    Flip(Box<ExprKind>, Box<ExprKind>),
    Choose(Box<ExprKind>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem<ExprKind>>),
    BinaryConst(Box<ExprKind>, BinOp, Value),
    Unary(Box<ExprKind>, UnaryOp),
}

impl Expr {
    /// Return the `Type` for the set of possible values this expression may down-evaluate to or
    /// match on up-evaluation.
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Const(c) => c.get_type(),
            Expr::Expr(t, _) => t.clone(),
        }
    }

    pub fn down(&self) -> Option<ExprDn> {
        match self {
            Expr::Const(c) => Some(ExprDn::Const(c.clone())),
            Expr::Expr(_, e) => e.down()
        }
    }

    pub fn up(&self, v: ExprDn, sink: &mut dyn FnMut(ValueSinkId, ExprDn)) {
        match self {
            Expr::Const(_) => {}
            Expr::Expr(_, e) => e.up(v, sink)
        }
    }

    pub(super) fn inner(self) -> ExprKind {
        match self {
            Expr::Const(v) => ExprKind::Const(v),
            Expr::Expr(_, k) => k,
        }
    }

    pub fn predicate(&self) -> Option<Predicate> {
        match self {
            Expr::Const(c) => Predicate::from_value(c),
            Expr::Expr(_, e) => e.predicate(),
        }
    }

    pub fn ignored() -> Self {
        Expr::Expr(Type::Ignored, ExprKind::Ignored)
    }

    pub fn var_dn(id: ValueSrc, ty: Type) -> Self {
        Expr::Expr(ty, ExprKind::VarDn(ExprDn::Variable(id)))
    }

    pub fn var_up(id: ValueSinkId, ty: Type) -> Self {
        Expr::Expr(ty, ExprKind::VarUp(id))
    }
}

impl ExprKind {
    pub fn down(&self) -> Option<ExprDn> {
        match *self {
            ExprKind::Ignored | ExprKind::Range(..) | ExprKind::Union(..) | ExprKind::VarUp(..) => None,
            ExprKind::VarDn(ref dn) => Some(dn.clone()),
            ExprKind::Const(ref v) => Some(ExprDn::Const(v.clone())),
            ExprKind::Flip(ref d, _) => d.down(),
            ExprKind::Choose(ref e, ref c) => Some(ExprDn::Choose(Box::new(e.down()?), c.clone())),
            ExprKind::Concat(ref c) => Some(ExprDn::Concat(
                c.iter().map(|l| l.map_elem(|e| e.down())).collect::<Option<Vec<_>>>()?
            )),
            ExprKind::BinaryConst(ref e, op, Value::Number(c)) => Some(ExprDn::BinaryConstNumber(Box::new(e.down()?), op, c.clone())),
            ExprKind::BinaryConst(_, _, _) => None,
            ExprKind::Unary(ref e, ref op) => Some(ExprDn::Unary(Box::new(e.down()?), op.clone())),
        }
    }

    pub fn up(&self, v: ExprDn, sink: &mut dyn FnMut(ValueSinkId, ExprDn)) {
        match *self {
            ExprKind::Ignored | ExprKind::Const(_) | ExprKind::VarDn(..)
             | ExprKind::Range(_, _) | ExprKind::Union(..) => (),
            ExprKind::VarUp(id) => sink(id, v),
            ExprKind::Flip(_, ref up) => up.up(v, sink),
            ExprKind::Choose(ref e, ref c) => {
                let mapping = c.iter().map(|(v1,v2)| (v2.clone(), v1.clone())).collect();
                e.up(ExprDn::Choose(Box::new(v), mapping), sink)
            }
            ExprKind::Concat(ref concat) => {
                let mut offset = 0;
                for c in concat {
                    match c {
                        ConcatElem::Elem(e) => e.up(ExprDn::Index(Box::new(v.clone()), offset), sink),
                        ConcatElem::Slice(e, width) => e.up(ExprDn::Slice(Box::new(v.clone()), offset, offset + width), sink),
                    };
                    offset += c.elem_count();
                }
            }
            ExprKind::BinaryConst(ref e, op, Value::Number(c)) => {
                e.up(ExprDn::BinaryConstNumber(Box::new(v), op.invert(), c.clone()), sink)
            }
            ExprKind::BinaryConst(..) => (),
            ExprKind::Unary(ref e, ref op) => e.up(ExprDn::Unary(Box::new(v), op.invert()), sink),
        }
    }

    pub fn predicate(&self) -> Option<Predicate> {
        match *self {
            ExprKind::Ignored | ExprKind::VarUp(..) => Some(Predicate::Any),
            ExprKind::VarDn(_) => None,
            ExprKind::Flip(_, ref up) => up.predicate(),
            ExprKind::Const(ref v) => Predicate::from_value(v),
            ExprKind::Range(lo, hi) => Some(Predicate::Range(lo, hi)),
            ExprKind::Union(ref u) => {
                let mut set = HashSet::new();
                for e in u {
                    match e.predicate()? {
                        Predicate::SymbolSet(s) => set.extend(s),
                        _ => return None,
                    }
                }
                Some(Predicate::SymbolSet(set))
            },
            ExprKind::Concat(ref parts) => {
                let mut predicates = Vec::with_capacity(parts.len());
                for part in parts {
                    match part.map_elem(|e| e.predicate())? {
                        ConcatElem::Slice(Predicate::Vector(inner), _) => predicates.extend(inner),
                        e => predicates.push(e),
                    }
                }
                Some(Predicate::Vector(predicates))
            }
            ExprKind::Choose(ref e, _) | ExprKind::BinaryConst(ref e, _, _) | ExprKind::Unary(ref e, _) => {
                match e.predicate() {
                    Some(Predicate::Any) => Some(Predicate::Any),
                    _ => None
                }
            },
        }
    }
}

/// An expression representing a runtime computation
#[derive(PartialEq, Debug, Clone)]
pub enum ExprDn {
    Const(Value),
    Variable(ValueSrc),
    Choose(Box<ExprDn>, Vec<(Value, Value)>),
    Concat(Vec<ConcatElem<ExprDn>>),
    Index(Box<ExprDn>, u32),
    Slice(Box<ExprDn>, u32, u32),
    BinaryConstNumber(Box<ExprDn>, BinOp, Number),
    Unary(Box<ExprDn>, UnaryOp),
}

impl ExprDn {
    pub fn invalid() -> ExprDn {
        ExprDn::Variable(ValueSrc(u32::MAX.into(), u32::MAX))
    }

    /// Down-evaluate the expression with variables from the given value function.
    pub fn eval(&self, state: &dyn Fn(ValueSrc)->Value) -> Value {
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

            ExprDn::Index(ref e, i) => {
                match e.eval(state) {
                    Value::Vector(v) if v.len() >= i as usize => v[i as usize].clone(),
                    other => panic!("Slice expected vector of at least length {i}, found {}", other)
                }
            }

            ExprDn::Slice(ref e, a, b) => {
                match e.eval(state) {
                    Value::Vector(v) if v.len() >= b as usize => Value::Vector(v[a as usize..b as usize].to_vec()),
                    other => panic!("Slice expected vector of at least length {b}, found {}", other)
                }
            }

            ExprDn::BinaryConstNumber(ref e, op, ref c) => {
                match (e.eval(state), c) {
                    (Value::Number(l), r) => Value::Number(op.eval(l, r)),
                    (l, r) => panic!("Invalid types {} {} in BinaryConst", l, r)
                }
            }

            ExprDn::Unary(ref e, ref op) => {
                op.eval(e.eval(state))
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Const(c) => c.fmt(f),
            Expr::Expr(_, ExprKind::Ignored) => write!(f, "_"),
            Expr::Expr(_, ExprKind::Const(ref p)) => write!(f, "{}", p),
            Expr::Expr(_, ExprKind::Range(a, b)) => write!(f, "{}..{}", a, b),
            Expr::Expr(ty, _) => write!(f, "<{ty}>"),
        }
    }
}

fn fn_unsigned(arg: Item) -> Result<Item, &'static str> {
    fn_signed_unsigned(arg, SignMode::None)
}

fn fn_signed(arg: Item) -> Result<Item, &'static str> {
    fn_signed_unsigned(arg, SignMode::TwosComplement)
}

fn fn_signed_unsigned(arg: Item, signed: SignMode) -> Result<Item, &'static str> {
    match <(Item, Item)>::try_from(arg) {
        Ok((Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(width)))), Item::Leaf(LeafItem::Value(v)))) => {
            let width = width.to_u32().ok_or("width must be a constant integer")?;

            match v {
                Expr::Const(Value::Number(i)) => {
                    Ok(Expr::Const(eval_int_to_bits(width, i)).into())
                }

                Expr::Expr(Type::Number(..), e) => {
                    let op = ExprKind::Unary(Box::new(e), UnaryOp::IntToBits {width, signed });
                    let ty = Type::bits(width);
                    Ok(Expr::Expr(ty, op).into())
                }

                _ => Err("value must be a number")
            }
        }
        _ => Err("invalid arguments to signed()")
    }
}

fn fn_chunks(arg: Item) -> Result<Item, &'static str> {
    match <(Item, Item)>::try_from(arg) {
        Ok((Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(width)))), Item::Leaf(LeafItem::Value(v)))) => {
            let width = width.to_u32().ok_or("width must be a constant integer")?;

            match v {
                Expr::Const(Value::Vector(c)) => {
                    Ok(Expr::Const(eval_chunks(width, c)).into())
                },
                Expr::Expr(Type::Vector(c, t), e) => {
                    let op = ExprKind::Unary(Box::new(e), UnaryOp::Chunks { width });
                    let ty = Type::Vector(c/width, Box::new(Type::Vector(width, t)));
                    Ok(Expr::Expr(ty, op).into())
                },
                _ => Err("value must be a vector")
            }
        }
        _ => Err("invalid arguments to chunks()")
    }
}

fn fn_complex(arg: Item) -> Result<Item, &'static str> {
    match <(Item, Item)>::try_from(arg) {
        Ok((
            Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(re)))),
            Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(im))))
         )) => {
            Ok(Item::Leaf(LeafItem::Value(Expr::Const(Value::Complex(Complex::new(re, im))))))
        }
        _ => Err("invalid arguments to complex(re, im): expected two number constants")
    }
}

fn fn_bits(arg: Item) -> Result<Item, &'static str> {
    let Item::Leaf(LeafItem::Value(Expr::Const(Value::Number(width)))) = arg else {
        return Err("invalid arguments to bits(): expected constant integer");
    };
    let width = width.to_u32().ok_or("width must be a constant integer")?;
    let ty = Type::bits(width);
    Ok(Expr::Expr(ty, ExprKind::Ignored).into())
}

pub fn expr_prelude() -> HashMap<String, Item> {
    let mut prelude = HashMap::new();

    pub fn add_primitive_fn(prelude: &mut HashMap<String, Item>, name: &str, prim: PrimitiveFn) {
        prelude.insert(name.to_owned(), Item::Leaf(LeafItem::Func(Arc::new(FunctionDef::Primitive(prim)))));
    }

    pub fn add_type(prelude: &mut HashMap<String, Item>, name: &str, ty: Type) {
        prelude.insert(name.to_owned(), Expr::Expr(ty, ExprKind::Ignored).into());
    }

    add_type(&mut prelude, "bit", Type::bit());
    add_type(&mut prelude, "byte", Type::bits(8));
    add_primitive_fn(&mut prelude, "bits", fn_bits);

    add_primitive_fn(&mut prelude, "signed", fn_signed);
    add_primitive_fn(&mut prelude, "unsigned", fn_unsigned);
    add_primitive_fn(&mut prelude, "chunks", fn_chunks);
    add_primitive_fn(&mut prelude, "complex", fn_complex);

    prelude
}

#[cfg(test)]
use super::data::NumberType;

#[cfg(test)]
pub fn test_expr_parse(e: &str) -> Expr {
    use crate::diagnostic::{DiagnosticContext, print_diagnostics};
    use crate::syntax::{parse_expr, SourceFile};
    use crate::core::{Scope, value};

    let mut dcx = DiagnosticContext::new();

    let scope = Scope { 
        file: Arc::new(SourceFile::new("<tests>".into(), "".into())),
        names: expr_prelude()
    };

    let ast = parse_expr(e).unwrap();
    let v = value(&mut dcx, &scope, &ast).unwrap();

    print_diagnostics(dcx.diagnostics());

    v
}

#[test]
fn test_number_const() {
    let two = test_expr_parse("2");
    assert_eq!(two.get_type(), Type::NumberSet([Number::new(2, 1)].into()));

    let decimal = test_expr_parse("1.023");
    assert_eq!(decimal.get_type(), Type::NumberSet([Number::new(1023, 1000)].into()));

    let four = test_expr_parse("2 + 2");
    assert_eq!(four.get_type(), Type::NumberSet([Number::new(4, 1)].into()));
}

#[test]
fn test_number_range() {
    let range = test_expr_parse("0.0..5.0");
    assert_eq!(range.get_type(), Type::Number(NumberType::new(Number::new(1, 1), 0, 5)));

    let sum = test_expr_parse("(0..10) + 5");
    assert_eq!(sum.get_type(), Type::Number(NumberType::new(Number::new(1, 1), 5, 15)));

    let sub = test_expr_parse("(0..10) - 5");
    assert_eq!(sub.get_type(), Type::Number(NumberType::new(Number::new(1, 1), -5, 5)));

    let sub_swap = test_expr_parse("5 - (0..7)");
    assert_eq!(sub_swap.get_type(), Type::Number(NumberType::new(Number::new(-1, 1), -5, 2)));

    let mul = test_expr_parse("(0..10) * 5");
    assert_eq!(mul.get_type(), Type::Number(NumberType::new(Number::new(5, 1), 0, 10)));

    let div = test_expr_parse("(0..10) / 8");
    assert_eq!(div.get_type(), Type::Number(NumberType::new(Number::new(1, 8), 0, 10)));

    let scale_by = test_expr_parse("0.0..2.0 by 0.1");
    assert_eq!(scale_by.get_type(), Type::Number(NumberType::new(Number::new(1, 10), 0, 20)));

    let scale_by_mul = test_expr_parse("(0.0..2.0 by 0.1) * 5");
    assert_eq!(scale_by_mul.get_type(), Type::Number(NumberType::new(Number::new(5, 10), 0, 20)));
}

#[test]
fn test_number_set() {
    let range = test_expr_parse("0 | 1.5");
    assert_eq!(range.get_type(), Type::NumberSet([Number::new(0, 1), Number::new(3, 2)].into()));

    let sum = test_expr_parse("(0|1) + 2");
    assert_eq!(sum.get_type(), Type::NumberSet([Number::new(2, 1), Number::new(3, 1)].into()));

    let sub = test_expr_parse("(100 | 200) - 2");
    assert_eq!(sub.get_type(), Type::NumberSet([Number::new(98, 1), Number::new(198, 1)].into()));

    let sub_swap = test_expr_parse("5 - (1 | 7)");
    assert_eq!(sub_swap.get_type(), Type::NumberSet([Number::new(4, 1), Number::new(-2, 1)].into()));

    let mul = test_expr_parse("(-1 | 2) * 5");
    assert_eq!(mul.get_type(), Type::NumberSet([Number::new(-5, 1), Number::new(10, 1)].into()));

    let div = test_expr_parse("(64 | 32) / 8");
    assert_eq!(div.get_type(), Type::NumberSet([Number::new(8, 1), Number::new(4, 1)].into()));

    let div_swap = test_expr_parse("128 / (64 | 32)");
    assert_eq!(div_swap.get_type(), Type::NumberSet([Number::new(2, 1), Number::new(4, 1)].into()));
}

#[test]
fn exprs() {
    let one_one_i = test_expr_parse("complex(1.0, 0.0) + complex(0, 1)");
    assert_eq!(one_one_i.get_type(), Type::Complex);

    let two_two_i = test_expr_parse("complex(1, 1) * 2");
    assert_eq!(two_two_i.get_type(), Type::Complex);

    let choose = test_expr_parse("(#a | #b)[#a = #x, #b = #y]");
    assert_eq!(choose.get_type(), Type::Symbol(["x".into(), "y".into()].into_iter().collect()));

    let concat = test_expr_parse("[(#a|#b), #c, _, 2:[(#a | #c), _], #a]");
    assert_eq!(concat.get_type(), Type::Vector(6, Box::new(
        Type::Symbol(["a".into(), "b".into(), "c".into()].into_iter().collect())
    )));

    let ignore = test_expr_parse("_");
    assert_eq!(ignore.get_type(), Type::Ignored);

    let down = test_expr_parse("<: #h");
    assert_eq!(down.get_type(), Type::Symbol(Some("h".to_string()).into_iter().collect()));

    let bound1 = test_expr_parse("_ : (0..10)");
    assert_eq!(bound1.get_type(), Type::Number(NumberType::new(Number::new(1, 1), 0, 10)));

    let bound2 = test_expr_parse("(0..2) : (0..10)");
    assert_eq!(bound2.get_type(), Type::Number(NumberType::new(Number::new(1, 1), 0, 2)));

    let fncall = test_expr_parse("((a) => a+3.0)(2.0)");
    assert_eq!(fncall.get_type(), Type::NumberSet([Number::new(5, 1)].into()));
}

#[test]
fn vec_const_fold() {
    assert_eq!(
        test_expr_parse("[1, 2, 2:[3, 1:[4]], 5]"),
        Expr::Const(Value::Vector(vec![1i64.into(), 2i64.into(), 3i64.into(), 4i64.into(), 5i64.into()])),
    );

    assert_eq!(
        test_expr_parse("[1, 2:[2, _], 3]"),
        test_expr_parse("[1, 2, _, 3]")
    );
}
