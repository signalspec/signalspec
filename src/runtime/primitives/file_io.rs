use std::path::PathBuf;
use std::future::Future;
use std::sync::Arc;
use async_fs::File;

use crate::core::{ Index, PrimitiveDef };
use crate::runtime::channel::Channel;
use crate::runtime::{ PrimitiveProcess };

pub fn add_primitives(index: &mut Index) {
    index.define_primitive("with Base() def file(const #r, const name): Bytes(#up)", PrimitiveDef {
        id: "file_read",
        instantiate: primitive_args!(|name: &str| {
            Ok(Arc::new(ReaderProcess(PathBuf::from(name))))
        })
    });

    index.define_primitive("with Base() def file(const #w, const name): Bytes(#dn)", PrimitiveDef {
        id: "file_write",
        instantiate: primitive_args!(|name: &str| {
            Ok(Arc::new(WriterProcess(PathBuf::from(name))))
        })
    });
}


struct ReaderProcess(pub PathBuf);
impl PrimitiveProcess for ReaderProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        assert_eq!(chan.len(), 1);
        let mut chan = chan.into_iter().next().unwrap();
        let fname = self.0.clone();
        Box::pin(async move {
            let file = File::open(&fname).await.unwrap();
            futures_lite::io::copy(file, chan.write_bytes()).await.unwrap();
            Ok(())
        })
    }
    
}

impl ::std::fmt::Debug for ReaderProcess {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "ReaderProcess({})", self.0.display())
    }
}

struct WriterProcess(pub PathBuf);
impl PrimitiveProcess for WriterProcess {
    fn run(&self, chan: Vec<Channel>) -> std::pin::Pin<Box<dyn Future<Output = Result<(), ()>>>> {
        assert_eq!(chan.len(), 1);
        let mut chan = chan.into_iter().next().unwrap();
        let fname = self.0.clone();
        Box::pin(async move {
            let file = File::create(&fname).await.unwrap();
            futures_lite::io::copy(chan.read_bytes(), file).await.unwrap();
            Ok(())
        })
    }
}

impl ::std::fmt::Debug for WriterProcess {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "WriterProcess({})", self.0.display())
    }
}
