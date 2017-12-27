use std::io;
use std::io::prelude::*;

use data::{ Value, DataMode };
use protocol::{ Fields };
use connection::Connection;
use process::{ FnProcess };
use language::{ Ctxt, PrimitiveDef, PrimitiveDefFields };

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num_complex::Complex;

pub fn add_primitives<'a>(loader: &'a Ctxt<'a>) {
    loader.define_primitive("with Bytes() def f32le(): Seq(Float32)", vec![
        PrimitiveDef {
            id: "f32_le_up",
            fields_down: Fields::bytes(DataMode { up: true, down: false, }),
            fields_up: PrimitiveDefFields::Explicit(Fields::f32(DataMode { up: true, down: false, })),
            instantiate: primitive_args!(|| { Ok(Box::new(FnProcess(f32_le_up, "f32_le_up"))) })
        },
        PrimitiveDef {
            id: "f32_le_down",
            fields_down: Fields::bytes(DataMode { up: false, down: true, }),
            fields_up: PrimitiveDefFields::Explicit(Fields::f32(DataMode { up: false, down: true, })),
            instantiate: primitive_args!(|| { Ok(Box::new(FnProcess(f32_le_down, "f32_le_down"))) })
        }
    ]);

    loader.define_primitive("with Bytes() def cf32le(): Seq(Float32)", vec![
        PrimitiveDef {
            id: "cf32_le_up",
            fields_down: Fields::bytes(DataMode { up: true, down: false, }),
            fields_up: PrimitiveDefFields::Explicit(Fields::cf32(DataMode { up: true, down: false, })),
            instantiate: primitive_args!(|| { Ok(Box::new(FnProcess(cf32_le_up, "cf32_le_up"))) })
        },
        PrimitiveDef {
            id: "cf32_le_down",
            fields_down: Fields::bytes(DataMode { up: false, down: true, }),
            fields_up: PrimitiveDefFields::Explicit(Fields::cf32(DataMode { up: false, down: true, })),
            instantiate: primitive_args!(|| { Ok(Box::new(FnProcess(cf32_le_down, "cf32_le_down"))) })
        }
    ]);
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
        let re = try!(r.read_f32::<LittleEndian>()) as f64;
        let im = try!(r.read_f32::<LittleEndian>()) as f64;
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
