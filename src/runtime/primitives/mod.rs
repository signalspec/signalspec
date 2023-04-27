use std::{fmt::Debug, pin::Pin, future::Future, sync::Arc};
use crate::{syntax::ast::Identifier, Item, Shape};
use super::Channel;

mod file_io;
mod seq;
#[cfg(target_os = "linux")] mod linux;

pub trait PrimitiveProcess: Debug + Send + Sync {
    fn run(&self, chan: Vec<Channel>) -> Pin<Box<dyn Future<Output = Result<(), ()>>>>;
}

pub(crate) fn instantiate_primitive(
    name: &Identifier,
    args: Item,
    shape_dn: &Shape,
    shape_up: Option<&Shape>
) -> Result<Arc<dyn PrimitiveProcess>, String> {
    let instantiate_fn = match name.name.as_str() {
        "const_seq_dn" => seq::SeqDownProcess::instantiate,
        "const_seq_up" => seq::SeqUpProcess::instantiate,

        "file_read" => file_io::ReaderProcess::instantiate,
        "file_write" => file_io::WriterProcess::instantiate,

        #[cfg(target_os = "linux")]
        "spidev" => linux::spidev::SpiProcess::instantiate,

        _ => return Err("primitive does not exist on this platform".into())
    };

    instantiate_fn(args, shape_dn, shape_up)
}
