mod test_runner;
mod primitives;
mod compile;
mod channel;

use std::sync::Arc;

pub use self::test_runner::run as run_tests_in_file;
pub use self::primitives::{ PrimitiveProcess, add_primitives };
use crate::{ Scope };
use crate::syntax::{ SourceFile, parse_process_chain, ast };
pub use channel::{ Channel, ChannelMessage };

use crate::core::{ Dir, Index, Item, Shape, compile_process_chain};

fn base_shape(index: &Index) -> Shape {
    let base = index.find_protocol("Base").cloned().expect("No `Base` protocol found in prelude");
    Shape {
        def: base,
        param: Item::Tuple(vec![]),
        dir: Dir::Dn,
        messages: vec![],
    }
}

pub fn compile_run(index: &Index, scope: &Scope, processes: &[ast::Process]) -> bool {
    let base_shape = base_shape(index);
    let p = compile_process_chain(index, &scope, base_shape, &processes);

    let compiled = Arc::new(compile::compile(&p));

    futures_lite::future::block_on(compile::ProgramExec::new(compiled, Vec::new())).is_ok()
}

pub fn parse_compile_run(index: &Index, call: &str) -> Result<bool, String> {
    let file = Arc::new(SourceFile::new("<process>".into(),  call.into()));
    let ast = parse_process_chain(&file.source).map_err(|e| e.to_string())?;
    let scope = Scope::new(file.clone());
    
    Ok(compile_run(index, &scope, &ast))
}