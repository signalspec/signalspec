use std::{collections::HashMap, sync::Arc};
use num_complex::Complex;

use crate::{Item, LeafItem, Type, Value, core::{FunctionDef, PrimitiveFn, resolve::{Expr, ExprKind}}, syntax::Number};
use super::op::{self, SignMode, UnaryOp};

pub fn expr_prelude() -> HashMap<String, Item> {
    let mut prelude = HashMap::new();

    pub fn add_primitive_fn(prelude: &mut HashMap<String, Item>, name: &str, prim: PrimitiveFn) {
        prelude.insert(name.to_owned(), Item::Leaf(LeafItem::Func(Arc::new(FunctionDef::Primitive(prim)))));
    }

    pub fn add_type(prelude: &mut HashMap<String, Item>, name: &str, ty: Type) {
        prelude.insert(name.to_owned(), LeafItem::Type(ty.into()).into());
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

fn fn_unsigned(arg: Item) -> Result<Item, &'static str> {
    fn_signed_unsigned(arg, SignMode::None)
}

fn fn_signed(arg: Item) -> Result<Item, &'static str> {
    fn_signed_unsigned(arg, SignMode::TwosComplement)
}

fn fn_signed_unsigned(arg: Item, signed: SignMode) -> Result<Item, &'static str> {
    let (width, v) = arg.try_into().map_err(|_| "invalid arguments: expected (width, value)")?;
    let width = u32::try_from(width)?;
    let v = Expr::try_from(v)?;

    match v {
        Expr::Const(Value::Number(i)) => {
            Ok(Expr::Const(op::eval_int_to_bits(width, i)).into())
        }

        Expr::Expr(Type::Number(..), e) => {
            let op = ExprKind::Unary(Box::new(e), UnaryOp::IntToBits {width, signed });
            let ty = Type::bits(width);
            Ok(Expr::Expr(ty, op).into())
        }

        _ => Err("value must be a number")
    }
}

fn fn_chunks(arg: Item) -> Result<Item, &'static str> {
    let (width, v) = arg.try_into().map_err(|_| "invalid arguments: expected (width, value)")?;
    let width = u32::try_from(width)?;
    let v = Expr::try_from(v)?;

    match v {
        Expr::Const(Value::Vector(c)) => {
            Ok(Expr::Const(op::eval_chunks(width, c)).into())
        },
        Expr::Expr(Type::Vector(c, t), e) => {
            let op = ExprKind::Unary(Box::new(e), UnaryOp::Chunks { width });
            let ty = Type::Vector(c/width, Box::new(Type::Vector(width, t)));
            Ok(Expr::Expr(ty, op).into())
        },
        _ => Err("value must be a vector")
    }
}

fn fn_complex(arg: Item) -> Result<Item, &'static str> {
    let (re, im) = arg.try_into().map_err(|_| "invalid arguments: expected (re, im)")?;
    let re = Number::try_from(re)?;
    let im = Number::try_from(im)?;
    Ok(Value::Complex(Complex::new(re, im)).into())
}

fn fn_bits(arg: Item) -> Result<Item, &'static str> {
    let width = u32::try_from(arg).map_err(|_| "invalid argument: expected width as a constant integer")?;
    let ty = Type::bits(width);
    Ok(LeafItem::Type(ty.into()).into())
}
