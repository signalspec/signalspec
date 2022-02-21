use std::sync::Arc;
use std::fs;
use std::path::Path;
use futures_lite::future;

use crate::DiagnosticHandler;
use crate::syntax::{ ast, SourceFile };
use crate::core::{ Index, FileScope };
use crate::core::{resolve_protocol_invoke, compile_process_chain };

pub fn run(fname: &str) -> bool {
    let fname = Path::new(fname);
    match fs::metadata(fname) {
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
    }
}

pub fn run_file(fname: &Path) -> bool {
    println!("Running tests for {}", fname.to_string_lossy());

    let ui = &crate::diagnostic::SimplePrintHandler;

    let mut index = Index::new();
    super::primitives::add_primitives(&mut index);
    
    let file = match SourceFile::load(fname) {
        Ok(file) => Arc::new(file),
        Err(err) => {
            println!("\tCould not open '{}': {}", fname.display(), err);
            return false;
        }
    };

    let module = match index.parse_module(file) {
        Ok(m) => m,
        Err(e) => {
            println!("\tParse error: {}", e);
            return false;
        }
    };

    let mut success = true;

    for (count, test) in module.tests.iter().enumerate() {
        print!("\tTest #{}:", count+1);
        let res = run_test(ui, &index, &module, &test);

        if let Some(r) = res.down{
            print!(" down:{}", if r { "ok" } else { "FAIL" });
            success &= r;
        }
        if let Some(r) = res.up {
            print!(" up:{}", if r { "ok" } else { "FAIL" });
            success &= r;
        }
        println!("");
    }

    success
}


pub struct TestResult {
    pub up: Option<bool>,
    pub down: Option<bool>
}

pub fn run_test(ui: &dyn DiagnosticHandler, index: &Index, file: &FileScope, test: &ast::Test) -> TestResult {
    fn symbol_expr(dir: &str) -> ast::SpannedExpr {
        let span = crate::syntax::FileSpan::new(0, 0);
        let node = ast::Expr::Value(ast::Value::Symbol(dir.into()));
        ast::Spanned { node, span }
    }

    // If the test uses `seq_both`, or `roundtrip`, generate `up` and `dn` versions and run them
    match test.processes.split_first() {
        Some((&ast::Process::Call(ref name, ref args), rest)) if name.node == "seq_both" => {
            // Turn seq_both into seq(_, dir, ...) and run it
            let run = |dir: &str| -> bool {
                let mut args = args.clone();
                args.insert(1, symbol_expr(dir));
                let process = ast::Process::Call(ast::Spanned { node: "seq".into(), span: name.span }, args);

                let processes: Vec<ast::Process> = Some(process).into_iter().chain(rest.iter().cloned()).collect();
                
                super::compile_run(ui, index, &file.scope, &processes)
            };

            let down_res = run("dn");
            let up_res = run("up");

            TestResult {
                down: Some(down_res ^ test.should_fail),
                up:   Some(up_res ^ test.should_fail),
            }
        }

        Some((&ast::Process::Call(ref name, ref args), rest)) if name.node == "roundtrip" => {
            let make_seq_shape = |ty: ast::SpannedExpr, dir: &str| {
                let seq_args = ast::Spanned { span: ty.span, node: ast::Expr::Tup(vec![ty, symbol_expr(dir)]) };
                let seq = ast::ProtocolRef { name: "Seq".into(), param: seq_args };
                resolve_protocol_invoke(index, &file.scope, &seq)
            };

            let shape_dn = make_seq_shape(args[0].clone(), "dn");
            let shape_up = make_seq_shape(args[0].clone(), "up");

            let p1 = compile_process_chain(ui, index, &file.scope, shape_dn, rest);
            let p2 = compile_process_chain(ui, index, &file.scope, shape_up, rest);

            let c1 = Arc::new(super::compile::compile(&p1));
            let c2 = Arc::new(super::compile::compile(&p2));

            let channel = super::Channel::new();

            let (r1, r2) = future::block_on(future::zip(
                async {
                    let r = super::compile::ProgramExec::new(c1, vec![channel.clone()]).await;
                    channel.set_closed(true);
                    r
                },
                super::compile::ProgramExec::new(c2, vec![channel.clone()])
            ));


            TestResult {
                down: Some(r1.is_ok() ^ test.should_fail),
                up:   Some(r2.is_ok() ^ test.should_fail),
            }
        }

        Some(_) => {
            let r = super::compile_run(ui, index, &file.scope, &test.processes);
            TestResult { down: Some(r ^ test.should_fail), up: None }
        }

        None => panic!("No tests to run")
    }
}
