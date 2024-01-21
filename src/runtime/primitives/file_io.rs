use std::path::PathBuf;
use std::future::Future;
use std::sync::Arc;
use async_fs::File;

use crate::{Item, Shape};
use crate::runtime::channel::Channel;
use crate::runtime::PrimitiveProcess;

pub(crate) struct ReaderProcess(PathBuf);

impl ReaderProcess {
    pub fn instantiate(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let name: String = args.try_into()?;
        assert!(shape_up.unwrap().tag_offset == 1);
        Ok(Arc::new(ReaderProcess(PathBuf::from(name))))
    }
}

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

pub(crate) struct WriterProcess(PathBuf);

impl WriterProcess {
    pub fn instantiate(args: Item, _shape_dn: &Shape, shape_up: Option<&Shape>) -> Result<Arc<dyn PrimitiveProcess>, String> {
        let name: String = args.try_into()?;
        assert!(shape_up.unwrap().tag_offset == 1);
        Ok(Arc::new(WriterProcess(PathBuf::from(name))))
    }
}

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
