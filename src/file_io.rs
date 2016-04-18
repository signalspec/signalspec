use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::fs::File;

use data::{ Value, DataMode, Shape };
use connection::Connection;
use language::Item;
use process::{Process, PrimitiveDef};

struct ReaderProcess(pub PathBuf);
impl Process for ReaderProcess {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        debug!("reader started");

        let mut c = upwards.write_bytes();
        let mut file = File::open(&self.0).unwrap();
        io::copy(&mut file, &mut c).is_ok()
    }

    fn shape_up(&self) -> &Shape {
        lazy_static! {
            static ref SHAPE: Shape = Shape::bytes(DataMode { down: false, up: true });
        }
        &SHAPE
    }
}

struct WriterProcess(pub PathBuf);
impl Process for WriterProcess {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        debug!("writer started {} {}", upwards.can_tx(), upwards.can_rx());

        let mut c = upwards.read_bytes();
        let mut file = File::create(&self.0).unwrap();
        io::copy(&mut c, &mut file).is_ok()
    }

    fn shape_up(&self) -> &Shape {
        lazy_static! {
            static ref SHAPE: Shape = Shape::bytes(DataMode { down: true, up: false });
        }
        &SHAPE
    }
}

pub struct FileDef;
impl PrimitiveDef for FileDef {
    fn invoke_def(&self, _: &Shape, arg: Item) -> Box<Process + 'static> {
        let args = match arg {
            Item::Tuple(v) => v,
            x => panic!("Unknown args type: {:?}", x)
        };

        let path = match &args[0] {
            &Item::String(ref v) => PathBuf::from(&v),
            x => panic!("Expected string, found {:?}", x)
        };

        match args[1].as_constant() {
            Some(&Value::Symbol(ref v)) => {
                match &v[..] {
                    "r" => box ReaderProcess(path),
                    "w" => box WriterProcess(path),
                    _ => panic!("Unknown file mode {:?}", args[1])
                }
            }
            _ => panic!("Expected symbol, found {:?}", args[1])
        }
    }
}
pub static FILE_DEF: FileDef = FileDef;
