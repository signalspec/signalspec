use std::io;
use std::io::prelude::*;

use data::{ Value, DataMode, Shape };
use connection::Connection;
use process::{Process, PrimitiveDef};
use language::Item;

use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num_complex::Complex;

enum Format {
    F32LE,
    C32LE,
}

struct BinFileDown(Shape, Format);
impl Process for BinFileDown {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        let mut down = downwards.write_bytes();

        match self.1 {
            Format::F32LE => {
                for b in upwards.iter_number() {
                    if down.write_f32::<LittleEndian>(b as f32).is_err() { return false; }
                }
            }
            Format::C32LE => {
                for b in upwards.iter_complex() {
                    if down.write_f32::<LittleEndian>(b.re as f32).is_err() { return false; }
                    if down.write_f32::<LittleEndian>(b.im as f32).is_err() { return false; }
                }
            }
        }

        true
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }
}

struct BinFileUp(Shape, Format);
impl Process for BinFileUp {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        let mut down = io::BufReader::new(downwards.read_bytes());

        match self.1 {
            Format::F32LE => {
                loop {
                    match down.read_f32::<LittleEndian>() {
                        Ok(v) => {
                            if upwards.send((0, vec![Value::Number(v as f64)])).is_err() {
                                return false;
                            }
                        }
                        Err(e) => {
                            return e.kind() == ::std::io::ErrorKind::UnexpectedEof;
                        }
                    }
                }
            }
            Format::C32LE => {
                fn read_complex<R: Read>(r: &mut R) -> io::Result<Complex<f64>> {
                    let re = try!(r.read_f32::<LittleEndian>()) as f64;
                    let im = try!(r.read_f32::<LittleEndian>()) as f64;
                    Ok(Complex::new(re, im))
                }

                loop {
                    match read_complex(&mut down) {
                        Ok(v) => {
                            if upwards.send((0, vec![Value::Complex(v)])).is_err() {
                                return false;
                            }
                        }
                        Err(e) => {
                            return e.kind() == ::std::io::ErrorKind::UnexpectedEof;
                        }
                    }
                }
            }
        }
    }

    fn shape_up(&self) -> &Shape {
        &self.0
    }
}

pub struct BinFileDef;
impl PrimitiveDef for BinFileDef {
    fn invoke_def(&self, downward_shape: &Shape, arg: Item) -> Box<Process + 'static> {
        let dir = downward_shape.match_bytes()
            .expect("Invalid shape below binfile");

        let arg_sym = match arg.as_constant() {
            Some(&Value::Symbol(ref s)) => s,
            _ => panic!("binfile expected a constant symbol argument")
        };

        let (shape, format) = match &arg_sym[..] {
            "f32le" => (Shape::number(dir, 0.0, 1.0), Format::F32LE),
            "c32le" => (Shape::complex(dir), Format::C32LE),
            other => panic!("Invalid binfile format `{}`", other),
        };

        match dir {
            DataMode { down: false, up: true } => box BinFileUp(shape, format),
            DataMode { down: true, up: false } => box BinFileDown (shape, format),
            _ => panic!("Invalid shape {:?} below binfile", downward_shape)
        }
    }
}
pub static BINFILE_DEF: BinFileDef = BinFileDef;
