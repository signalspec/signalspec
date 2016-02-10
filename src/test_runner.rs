use std::fs;
use std::path::Path;
use std::io::prelude::*;
use ast;
use data::Shape;
use process::{self, Process};
use resolve::module_loader::{Module, ModuleLoader};
use session::Session;

pub fn run(fname: &str) {
    let fname = Path::new(fname);
    let success = match fs::metadata(fname) {
        Ok(ref meta) if meta.is_file() => run_file(fname),
        Ok(ref meta) if meta.is_dir() => {
            let mut success = true;
            for entry in fs::read_dir(fname).unwrap() {
                let path = entry.unwrap().path();
                if path.to_str().unwrap().ends_with(".signalspec") {
                    success &= run_file(&path);
                }
            }
            success
        }
        _ => {
            println!("Could not open {}", fname.to_string_lossy());
            false
        }
    };
    ::std::process::exit( if success { 0 } else { 1 } );
}

pub fn run_file(fname: &Path) -> bool {
    println!("Running tests for {}", fname.to_string_lossy());

    let sess = Session::new();
    let loader = ModuleLoader::new(&sess);

    let source = match fs::File::open(fname) {
        Ok(mut file) => {
            let mut source = String::new();
            file.read_to_string(&mut source).unwrap();
            source
        }
        Err(..) => {
            println!("\tCould not open {}", fname.to_string_lossy());
            return false;
        }
    };

    let ast = match loader.parse(&source) {
        Ok(m) => m,
        Err(e) => {
            println!("\tParse error: {}", e);
            return false;
        }
    };

    let modscope = loader.resolve_module(ast);

    let mut count = 0;
    let mut success = true;
    for entry in &ast.defs {
        match *entry {
            ast::ModuleEntry::Test(ref t) => {
                count += 1;
                print!("\tTest #{}:", count);
                let r = run_test(&sess, &modscope, t);
                println!(" {}", if r {"ok"} else {"FAIL"} );
                success &= r;
            }
            _ => {}
        }
    }

    success
}

fn run_ast(sess: &Session, module: &Module, ast: &Vec<ast::Process>) -> bool {
    let mut processes = vec![];
    let mut shape = Shape::null();

    for process_ast in ast {
        let process = process::resolve_process(sess, module, &shape, process_ast);
        shape = process.shape_up().clone();
        processes.push(process);
    }

    process::run_process_chain(processes)
}

fn run_test(sess: &Session, module: &Module, test: &ast::Test) -> bool {
    // If the test uses `@both`, generate `@up` and `@dn` versions and run them
    if let Some((&ast::Process::Literal(ast::ProcessLiteralDirection::Both, ref ty, ref blk), rest)) = test.processes.split_first() {
        let mut up_version = vec![ast::Process::Literal(ast::ProcessLiteralDirection::Up, ty.clone(), blk.clone())];
        up_version.extend(rest.iter().cloned());
        let success_up = run_ast(sess, module, &up_version) ^ test.should_fail;

        let mut down_version = vec![ast::Process::Literal(ast::ProcessLiteralDirection::Down, ty.clone(), blk.clone())];
        down_version.extend(rest.iter().cloned());
        let success_down = run_ast(sess, module, &down_version) ^ test.should_fail;

        success_up && success_down
    } else {
        run_ast(sess, module, &test.processes) ^ test.should_fail
    }
}
