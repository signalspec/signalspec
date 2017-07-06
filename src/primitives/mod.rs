macro_rules! bind {
    (|$($i:ident: $t:ty),*| $body:block) => {
        Box::new(|s: &::language::Scope| {
            $(let $i:$t = s.get_as(stringify!($i))?;)*
            $body
        })
    };
    (|| $body:block ) => { Box::new(|_: &::language::Scope| { $body }) };
}

mod file_io;
mod binfile;

use language::{ add_primitive_fns, Ctxt };

pub fn add_primitives<'a>(loader: &'a Ctxt<'a>) {
    loader.define_prelude(r#"
    protocol Base() {}
    let Bytes = #0..#255
    let Float32 = -1.0..1.0
    "#);

    add_primitive_fns(loader);

    file_io::add_primitives(loader);
    binfile::add_primitives(loader);
}
