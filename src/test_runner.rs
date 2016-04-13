use std::fs;
use std::path::Path;
use std::io::prelude::*;
use std::sync::mpsc;
use language::ast;
use data::{Shape, DataMode};
use process::{self, Process};
use language::{self, Module, ModuleLoader};
use session::Session;
use connection::{ Connection, ConnectionMessage };

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

    let sess = Session::new(None);
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
                success &= r;
            }
            _ => {}
        }
    }

    success
}

fn run_ast(sess: &Session, module: &Module, bottom_process: Option<Box<Process>>, ast: &[ast::Process]) -> bool {
    let mut processes;
    let mut shape;

    if let Some(p) = bottom_process {
        shape = p.shape_up().clone();
        processes = vec![p];
    } else {
        shape = Shape::null();
        processes = vec![];
    }

    for process_ast in ast {
        let process = language::program::resolve_process(sess, module, &shape, process_ast);
        shape = process.shape_up().clone();
        processes.push(process);
    }

    process::run_process_chain(processes)
}

struct Collect {
    shape: Shape,
    sender: mpsc::Sender<ConnectionMessage>,
}

impl Process for Collect {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        while let Ok(d) = upwards.recv() {
            if self.sender.send(d).is_err() { return false; }
        }
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.shape
    }
}

struct Emit {
    shape: Shape,
    receiver: mpsc::Receiver<ConnectionMessage>,
}

impl Process for Emit {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        while let Ok(d) = self.receiver.recv() {
            if upwards.send(d).is_err() { return false; }
        }
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.shape
    }
}

fn run_test(sess: &Session, module: &Module, test: &ast::Test) -> bool {
    // If the test uses `@both`, generate `@up` and `@dn` versions and run them
    match test.processes.split_first() {
        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::Both, ref ty, ref blk), rest)) => {
            let mut up_version = vec![ast::Process::Literal(ast::ProcessLiteralDirection::Up, ty.clone(), blk.clone())];
            up_version.extend(rest.iter().cloned());
            let success_up = run_ast(sess, module, None, &up_version) ^ test.should_fail;

            print!(" up:{}", if success_up { "pass" } else { "FAIL" });

            let mut down_version = vec![ast::Process::Literal(ast::ProcessLiteralDirection::Down, ty.clone(), blk.clone())];
            down_version.extend(rest.iter().cloned());
            let success_down = run_ast(sess, module, None, &down_version) ^ test.should_fail;

            println!(" down:{}", if success_down { "pass" } else { "FAIL" });

            success_up && success_down
        }

        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::RoundTrip, ref ty, _), rest)) => {
            let shape_item = language::expr::rexpr(sess, &*module.scope.borrow(), ty);
            let shape_dn = shape_item.clone().into_shape(sess, DataMode { down: true, up: false });
            let shape_up = shape_item.clone().into_shape(sess, DataMode { down: false, up: true });

            let (s, r) = mpsc::channel();
            let process_dn = Collect { shape: shape_dn, sender: s};
            let process_up = Emit { shape: shape_up, receiver: r};

            let success_down = run_ast(sess, module, Some(box process_dn), rest) ^ test.should_fail;
            print!(" down:{}", if success_down { "pass" } else { "FAIL" });

            let success_up = run_ast(sess, module, Some(box process_up), rest) ^ test.should_fail;
            println!(" up:{}", if success_up { "pass" } else { "FAIL" });

            success_up && success_down
        }

        _ => {
            let success = run_ast(sess, module, None, &test.processes) ^ test.should_fail;
            println!(" {}", if success { "pass" } else { "FAIL" });
            success
        }
    }
}
