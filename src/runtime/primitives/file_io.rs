use std::io;
use std::path::PathBuf;
use std::fs::File;

use crate::core::{ Index, PrimitiveDef };
use crate::runtime::{ Connection, PrimitiveProcess };

pub fn add_primitives(index: &mut Index) {
    index.define_primitive("with Base() def file(const #r, const name): Bytes(#up)", PrimitiveDef {
        id: "file_read",
        instantiate: primitive_args!(|name: &str| {
            Ok(Box::new(ReaderProcess(PathBuf::from(name))))
        })
    });

    index.define_primitive("with Base() def file(const #w, const name): Bytes(#dn)", PrimitiveDef {
        id: "file_write",
        instantiate: primitive_args!(|name: &str| {
            Ok(Box::new(WriterProcess(PathBuf::from(name))))
        })
    });
}


struct ReaderProcess(pub PathBuf);
impl PrimitiveProcess for ReaderProcess {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        debug!("reader started");

        let mut c = upwards.write_bytes();
        let mut file = File::open(&self.0).unwrap();
        io::copy(&mut file, &mut c).is_ok()
    }
}

impl ::std::fmt::Debug for ReaderProcess {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "ReaderProcess({})", self.0.display())
    }
}

struct WriterProcess(pub PathBuf);
impl PrimitiveProcess for WriterProcess {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        debug!("writer started {} {}", upwards.can_tx(), upwards.can_rx());

        let mut c = upwards.read_bytes();
        let mut file = File::create(&self.0).unwrap();
        io::copy(&mut c, &mut file).is_ok()
    }
}

impl ::std::fmt::Debug for WriterProcess {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "WriterProcess({})", self.0.display())
    }
}
