use std::fs;
use std::path::Path;
use std::io::prelude::*;

use crate::syntax::{ ast, SourceFile };
use crate::core::{ Ctxt, Config, Scope, DataMode, ProtocolScope };
use crate::core::{resolve_protocol_invoke, make_literal_process, resolve_process };
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

    let loader = Ctxt::new(Config::default());
    super::primitives::add_primitives(&loader);

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

    let file = SourceFile { name: fname.to_string_lossy().into(), source };
    let module = match loader.parse_module(file) {
        Ok(m) => m,
        Err(e) => {
            println!("\tParse error: {}", e);
            return false;
        }
    };

    let mut success = true;

    for (count, test) in module.tests().iter().enumerate() {
        print!("\tTest #{}:", count+1);
        let res = run_test(&loader, &test.scope, &*loader.protocol_scope.borrow(), &test.ast);

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

pub fn run_test<'a>(ctx: &'a Ctxt,
                        scope: &Scope,
                        protocol_scope: &ProtocolScope,
                        test: &ast::Test) -> TestResult {

    fn run_stack<'a>(ctx: &'a Ctxt, mut handle: Handle<'a>, scope: &Scope, protocol_scope: &ProtocolScope, ast: &[ast::Process]) -> bool {
        let p = resolve_process(ctx, scope, protocol_scope, handle.top_shape(), handle.top_fields(), ast);
        handle.spawn(p).join()
    }

    // If the test uses `@both`, generate `@up` and `@dn` versions and run them
    match test.processes.split_first() {
        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::Both, ref shape_expr, ref block), rest)) => {
            let dn_base = make_literal_process(ctx, scope, protocol_scope, false, shape_expr, block);
            let up_base = make_literal_process(ctx, scope, protocol_scope, true, shape_expr, block);

            let dn_handle = Handle::base(ctx).spawn(dn_base);
            let down_res = run_stack(ctx, dn_handle, scope, protocol_scope, rest);

            let up_handle = Handle::base(ctx).spawn(up_base);
            let up_res = run_stack(ctx, up_handle, scope, protocol_scope, rest);

            TestResult {
                down: Some(down_res ^ test.should_fail),
                up:   Some(up_res ^ test.should_fail),
            }
        }

        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::RoundTrip, ref ty, _), rest)) => {
            let shape = resolve_protocol_invoke(ctx, scope, ty);
            let fields_dn = shape.fields(DataMode { down: true, up: false });
            let fields_up = shape.fields(DataMode { down: false, up: true });

            let (c1, c2) = Connection::new(&fields_dn);

            let h1 = Handle::new(ctx, shape.clone(), fields_dn, c1, None);
            let h2 = Handle::new(ctx, shape, fields_up, c2, None);

            TestResult {
                down: Some(run_stack(ctx, h1, scope, protocol_scope, rest) ^ test.should_fail),
                up:   Some(run_stack(ctx, h2, scope, protocol_scope, rest) ^ test.should_fail),
            }
        }

        Some(_) => {
            let mut handle = Handle::base(ctx);
            let p = resolve_process(ctx, scope, protocol_scope, handle.top_shape(), handle.top_fields(), &test.processes);
            let is_up = p.processes[0].fields_up.direction().up;

            let r = handle.spawn(p).join() ^ test.should_fail;

            let (up, down) = if is_up { (Some(r), None) } else { (None, Some(r)) };
            TestResult { down, up }
        }

        None => panic!("No tests to run")
    }
}
