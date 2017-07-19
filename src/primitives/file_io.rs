use std::io;
use std::path::PathBuf;
use std::fs::File;

use data::{ DataMode };
use protocol::{ Fields };
use connection::Connection;
use language::{ Ctxt, PrimitiveDef, PrimitiveDefFields };
use process::Process;

pub fn add_primitives<'a>(loader: &'a Ctxt<'a>) {
    loader.define_primitive("with () def file(#r, name): Bytes", vec![
        PrimitiveDef {
            id: "file_read",
            fields_down: Fields::null(),
            fields_up: PrimitiveDefFields::Explicit(Fields::bytes(DataMode { up: true, down: false, })),
            instantiate: bind!(|name: &str| {
                Ok(Box::new(ReaderProcess(PathBuf::from(name))))
            })
        }
    ]);

    loader.define_primitive("with () def file(#w, name): Bytes", vec![
        PrimitiveDef {
            id: "file_write",
            fields_down: Fields::null(),
            fields_up: PrimitiveDefFields::Explicit(Fields::bytes(DataMode { up: false, down: true, })),
            instantiate: bind!(|name: &str| {
                Ok(Box::new(WriterProcess(PathBuf::from(name))))
            })
        }
    ]);
}


struct ReaderProcess(pub PathBuf);
impl Process for ReaderProcess {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        debug!("reader started");

        let mut c = upwards.write_bytes();
        let mut file = File::open(&self.0).unwrap();
        io::copy(&mut file, &mut c).is_ok()
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
}