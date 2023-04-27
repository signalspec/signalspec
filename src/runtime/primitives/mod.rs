use std::{fmt::Debug, pin::Pin, future::Future};

#[macro_export]
macro_rules! primitive_args {
    (|$($i:ident: $t:ty),*| $body:block) => {
        Box::new(|s: &$crate::Scope| {
            $(let $i:$t = s.get_as(stringify!($i))
                .map_err(|()| format!("invalid value for primitive argument {}", stringify!($i)))?;)*
            $body
        })
    };
    (|| $body:block ) => { Box::new(|_: &$crate::Scope| { $body }) };
}

mod file_io;
mod seq;
#[cfg(target_os = "linux")] mod linux;

use crate::core::Index;
use super::Channel;

pub fn add_primitives(index: &mut Index) {
    seq::add_primitives(index);
    file_io::add_primitives(index);

    #[cfg(target_os = "linux")]
    linux::add_primitives(index);
}

pub trait PrimitiveProcess: Debug + Send + Sync {
    fn run(&self, chan: Vec<Channel>) -> Pin<Box<dyn Future<Output = Result<(), ()>>>>;
}

