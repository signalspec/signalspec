use std::io;
use std::io::prelude::*;

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num_complex::Complex;

use crate::syntax::Value;
use crate::core::{Index, PrimitiveDef };
use crate::runtime::Connection;
use crate::runtime::primitives::FnProcess;

pub fn add_primitives(index: &mut Index) {
    index.define_primitive("with Bytes(#up) def f32le(): Seq(Float32, #up)", PrimitiveDef {
        id: "f32_le_up",
        instantiate: primitive_args!(|| { Ok(Box::new(FnProcess(f32_le_up, "f32_le_up"))) })
    });

    index.define_primitive("with Bytes(#dn) def f32le(): Seq(Float32, #dn)", PrimitiveDef {
        id: "f32_le_down",
        instantiate: primitive_args!(|| { Ok(Box::new(FnProcess(f32_le_down, "f32_le_down"))) })
    });

    index.define_primitive("with Bytes(#up) def cf32le(): Seq(Float32, #up)", PrimitiveDef {
        id: "cf32_le_up",
        instantiate: primitive_args!(|| { Ok(Box::new(FnProcess(cf32_le_up, "cf32_le_up"))) })
    });

    index.define_primitive("with Bytes(#dn) def cf32le(): Seq(Float32, #dn)", PrimitiveDef {
        id: "cf32_le_down",
        instantiate: primitive_args!(|| { Ok(Box::new(FnProcess(cf32_le_down, "cf32_le_down"))) })
    });
}

fn f32_le_down(downwards: &mut Connection, upwards: &mut Connection) -> bool {
    let mut down = downwards.write_bytes();
    for b in upwards.iter_number() {
        if down.write_f32::<LittleEndian>(b as f32).is_err() { return false; }
    }
    true
}

fn f32_le_up(downwards: &mut Connection, upwards: &mut Connection) -> bool {
    let mut down = io::BufReader::new(downwards.read_bytes());
    loop {
        match down.read_f32::<LittleEndian>() {
            Ok(v) => {
                if upwards.send(vec![Some(Value::Number(v as f64))]).is_err() {
                    return false;
                }
            }
            Err(e) => {
                return e.kind() == ::std::io::ErrorKind::UnexpectedEof;
            }
        }
    }
}

fn cf32_le_down(downwards: &mut Connection, upwards: &mut Connection) -> bool {
    let mut down = downwards.write_bytes();
    for b in upwards.iter_complex() {
        if down.write_f32::<LittleEndian>(b.re as f32).is_err() { return false; }
        if down.write_f32::<LittleEndian>(b.im as f32).is_err() { return false; }
    }
    true
}

fn cf32_le_up(downwards: &mut Connection, upwards: &mut Connection) -> bool {
    let mut down = io::BufReader::new(downwards.read_bytes());
    fn read_complex<R: Read>(r: &mut R) -> io::Result<Complex<f64>> {
        let re = r.read_f32::<LittleEndian>()? as f64;
        let im = r.read_f32::<LittleEndian>()? as f64;
        Ok(Complex::new(re, im))
    }

    loop {
        match read_complex(&mut down) {
            Ok(v) => {
                if upwards.send(vec![Some(Value::Complex(v))]).is_err() {
                    return false;
                }
            }
            Err(e) => {
                return e.kind() == ::std::io::ErrorKind::UnexpectedEof;
            }
        }
    }
}
