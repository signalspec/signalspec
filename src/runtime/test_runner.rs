use std::fs;
use std::path::Path;

use crate::syntax::{ ast, SourceFile };
use crate::core::{ Index, FileScope, Ctxt };
use crate::core::{resolve_protocol_invoke, resolve_process };
use crate::runtime::{ Handle, Connection };


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

    let mut index = Index::new();
    super::primitives::add_primitives(&mut index);
    
    let file = match SourceFile::load(fname) {
        Ok(file) => file,
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
        let res = run_test(&index, &module, &test);

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

pub fn run_test(index: &Index, file: &FileScope, test: &ast::Test) -> TestResult {
    let ctx = Ctxt::new(Default::default(), index);

    let run_stack = |mut handle: Handle<'_>,  ast: &[ast::Process]| {
        let p = resolve_process(&ctx, &file.scope, handle.top_shape(), ast);
        handle.spawn(p).join()
    };

    fn symbol_expr(dir: &str) -> ast::SpannedExpr {
        let span = crate::syntax::FileSpan::new(0, 0);
        let node = ast::Expr::Value(ast::Value::Symbol(dir.into()));
        ast::Spanned { node, span }
    }

    // If the test uses `seq_both`, or `roundtrip`, generate `up` and `dn` versions and run them
    match test.processes.split_first() {
        Some((&ast::Process::Call(ref name, ref args), rest)) if name == "seq_both" => {
            // Turn seq_both into seq(_, dir, ...) and run it
            let run = |dir: &str| -> bool {
                let mut args = args.clone();
                args.insert(1, symbol_expr(dir));
                let process = ast::Process::Call("seq".into(), args);

                let processes: Vec<ast::Process> = Some(process).into_iter().chain(rest.iter().cloned()).collect();
                run_stack(Handle::base(Default::default(), index), &processes)
            };

            let down_res = run("dn");
            let up_res = run("up");

            TestResult {
                down: Some(down_res ^ test.should_fail),
                up:   Some(up_res ^ test.should_fail),
            }
        }

        Some((&ast::Process::Call(ref name, ref args), rest)) if name == "roundtrip" => {
            let make_seq_shape = |ty: ast::SpannedExpr, dir: &str| {
                let seq_args = ast::Spanned { span: ty.span, node: ast::Expr::Tup(vec![ty, symbol_expr(dir)]) };
                let seq = ast::ProtocolRef { name: "Seq".into(), param: seq_args };
                resolve_protocol_invoke(&ctx, &file.scope, &seq)
            };

            let shape_dn = make_seq_shape(args[0].clone(), "dn");
            let shape_up = make_seq_shape(args[0].clone(), "up");

            let fields_dn = shape_dn.fields();
            let (c1, c2) = Connection::new(&fields_dn);

            let h1 = Handle::new(Default::default(), index, shape_dn, c1, None);
            let h2 = Handle::new(Default::default(), index, shape_up, c2, None);

            TestResult {
                down: Some(run_stack(h1, rest) ^ test.should_fail),
                up:   Some(run_stack(h2, rest) ^ test.should_fail),
            }
        }

        Some(_) => {
            let mut handle = Handle::base(Default::default(), index);
            let p = resolve_process(&ctx, &file.scope, handle.top_shape(), &test.processes);
            let is_up = p.processes[0].shape_up.direction().up;

            let r = handle.spawn(p).join() ^ test.should_fail;

            let (up, down) = if is_up { (Some(r), None) } else { (None, Some(r)) };
            TestResult { down, up }
        }

        None => panic!("No tests to run")
    }
}
