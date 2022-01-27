mod connection;
mod exec;
mod test_runner;
mod primitives;

pub use self::connection::{ Connection, ConnectionMessage };
pub use self::test_runner::run as run_tests_in_file;
pub use self::primitives::{ PrimitiveProcess, add_primitives };
pub use self::exec::run;
use crate::{ Scope };
use crate::syntax::{ SourceFile, parse_process_chain, ast };

use crate::core::{ Ctxt, Dir, Index, Item, Shape, compile_process_chain};

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
    let ctx = Ctxt::new(Default::default(), index);
    let base_shape = base_shape(index);
    let p = compile_process_chain(&ctx, &scope, &base_shape, &processes);

    println!("\nCompiled:");
    compile::compile(&p);

    exec::run(&p, &mut Connection::null(), &mut Connection::null())
}

pub fn parse_compile_run(index: &Index, call: &str) -> Result<bool, String> {
    let scope = Scope::new();
    let file = SourceFile { name: "<process>".into(), source: call.into() };
    let ast = parse_process_chain(&file.source).map_err(|e| e.to_string())?;
    
    Ok(compile_run(index, &scope, &ast))
}