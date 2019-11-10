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
mod seq;

use crate::core::{ add_primitive_fns, Index };
use super::Connection;

pub fn add_primitives(index: &mut Index) {
    index.define_prelude(r#"
    protocol Base() {}
    protocol Seq(T, dir) {
        val(var(dir) T)
    }

    protocol Bytes(dir) { byte(var(dir) 0..255) }
    let Float32 = -1.0..1.0
    "#);

    add_primitive_fns(index);

    seq::add_primitives(index);
    file_io::add_primitives(index);
    binfile::add_primitives(index);
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
