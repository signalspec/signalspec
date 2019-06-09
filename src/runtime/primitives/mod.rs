use std::fmt::Debug;

#[macro_export]
macro_rules! primitive_args {
    (|$($i:ident: $t:ty),*| $body:block) => {
        Box::new(|s: &$crate::Scope| {
            $(let $i:$t = s.get_as(stringify!($i))?;)*
            $body
        })
    };
    (|| $body:block ) => { Box::new(|_: &$crate::Scope| { $body }) };
}

mod file_io;
mod binfile;

use crate::core::{ add_primitive_fns, Ctxt };
use super::Connection;

pub fn add_primitives<'a>(loader: &'a Ctxt) {
    loader.define_prelude(r#"
    protocol Base() {}
    protocol Seq(T) { val(T) }

    protocol Bytes() { byte(0..255) }
    let Float32 = -1.0..1.0
    "#);

    add_primitive_fns(loader);

    file_io::add_primitives(loader);
    binfile::add_primitives(loader);
}

pub trait PrimitiveProcess: Debug + Send + Sync {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool;
}

pub struct FnProcess<T: Fn(&mut Connection, &mut Connection) -> bool>(pub T, pub &'static str);

impl<T: Sync + Send + Fn(&mut Connection, &mut Connection) -> bool> PrimitiveProcess for FnProcess<T> {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        (self.0)(downwards, upwards)
    }
}

impl<T: Sync +Send + Fn(&mut Connection, &mut Connection) -> bool> Debug for FnProcess<T> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> Result<(), ::std::fmt::Error> {
        write!(f, "{}", self.1)
    }
}
