use std::fmt::Display;
use std::sync::Arc;
use std::fs;
use std::path::Path;

use crate::diagnostic::Diagnostics;
use crate::{ Handle, ChannelMessage, DiagnosticContext };
use crate::runtime::channel::item_to_msgs;
use crate::syntax::{ ast, SourceFile };
use crate::core::{ Index, FileScope, rexpr, rexpr_tup };

pub fn run(show_diagnostics: &dyn Fn(Diagnostics), index: &Index, fname: &Path) -> bool {
    let fname = Path::new(fname);
    match fs::metadata(fname) {
        Ok(ref meta) if meta.is_file() => {
            println!("Running tests for {}", fname.to_string_lossy());
            let file = match SourceFile::load(fname) {
                Ok(file) => Arc::new(file),
                Err(err) => {
                    println!("\tCould not open '{}': {}", fname.display(), err);
                    return false;
                }
            };
            run_file(show_diagnostics, index, file, false)
        },
        Ok(ref meta) if meta.is_dir() => {
            let mut success = true;
            for entry in fs::read_dir(fname).unwrap() {
                let path = entry.unwrap().path();
                if path.to_str().unwrap().ends_with(".signalspec") {
                    success &= run(show_diagnostics, index, &path);
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

pub fn run_file(show_diagnostics: &dyn Fn(Diagnostics), index: &Index, file: Arc<SourceFile>, compile_only: bool) -> bool {
    let mut index = index.clone();

    let module = index.parse_module(file);

    if module.errors.has_errors() {
        show_diagnostics(module.errors.diagnostics());
        return false;
    }

    let mut success = true;

    for def in module.defs() {
        for (count, attr) in def.attributes.iter().filter(|a| a.name.name == "test").enumerate() {
            if !compile_only {
                print!("\tTest {} #{}:", def.name.name, count+1);
            }
            let res = match run_test(show_diagnostics, &index, &module, def, attr, compile_only) {
                Ok(res) => res,
                Err(msg) => {
                    println!(" ERR: {msg}");
                    success = false;
                    continue;
                },
            };
    
            if let Some(r) = res.down{
                if !compile_only {
                    print!(" down:{r}");
                }
                success &= r.success();
            }
            if let Some(r) = res.up {
                if !compile_only {
                    print!(" up:{r}");
                }
                success &= r.success();
            }
            if !compile_only{
                println!("");
            }
        }
    }

    success
}


enum TestState {
    CompileError,
    CompileSuccess,
    Pass,
    Fail,
}

impl Display for TestState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            TestState::CompileError => "ERR",
            TestState::CompileSuccess => "compiled",
            TestState::Pass => "ok",
            TestState::Fail => "FAIL",
        };
        Display::fmt(s, f)
    }
}

impl TestState {
    fn success(&self) -> bool {
        match self {
            TestState::CompileError => false,
            TestState::CompileSuccess => true,
            TestState::Pass => true,
            TestState::Fail => false,
        }
    }
}

struct TestResult {
    up: Option<TestState>,
    down: Option<TestState>
}

fn run_test(show_diagnostics: &dyn Fn(Diagnostics), index: &Index, file: &FileScope, def: &ast::Def, attr: &ast::Attribute, compile_only: bool) -> Result<TestResult, &'static str> {
    if def.bottom.name.name != "Seq" {
        return Err("Test def must use Seq");
    }

    if !def.params.fields.is_empty() {
        return Err("Test def must not have parameters");
    }

    let mut dcx = DiagnosticContext::new();

    let scope = file.scope();

    let seq_ty = match &def.bottom.param {
        ast::Expr::Tup(t) if t.fields.len() == 2 => rexpr(&mut dcx, &scope, &t.fields[0].expr),
        _ => return Err("Seq takes two arguments"),
    };

    let test_args = rexpr_tup(&mut dcx, &scope, &attr.args);
    let test_args = test_args.as_tuple();
    let test_mode = test_args.first().and_then(|i| i.as_symbol());
    let test_data = test_args.get(1);

    let run_dn = || {
        let Ok((channel, mut handle)) = Handle::seq_dn(index, seq_ty.clone()) else {
            eprintln!("Bad type for Seq(): {seq_ty}");
            return (None, TestState::CompileError);
        };

        let h = match handle.compile_run(index, &scope, &def.process) {
            Ok(h) => h,
            Err(d) => {
                show_diagnostics(d);
                return (None, TestState::CompileError); 
            }
        };

        if compile_only {
            return (None, TestState::CompileSuccess);
        }

        match h.finish() {
            Ok(()) => {
                let data = channel.take_all();
                (Some(data), TestState::Pass)
            },
            Err(()) => {
                (None, TestState::Fail)
            },
        }
    };

    let run_up = |messages: Vec<ChannelMessage>| -> TestState {
        let Ok((channel, mut handle)) = Handle::seq_up(index, seq_ty.clone()) else {
            eprintln!("Bad type for Seq(): {seq_ty}");
            return TestState::CompileError;
        };
        for m in messages { channel.send(m); }
        channel.end(true);
        let h = match handle.compile_run( index, &scope, &def.process) {
            Ok(h) => h,
            Err(d) => {
                show_diagnostics(d);
                return TestState::CompileError;
            }
        };

        if compile_only {
            return TestState::CompileSuccess;
        }
        
        match h.finish() {
            Ok(_) => TestState::Pass,
            Err(_) => TestState::Fail,
        }
    };

    match (test_mode, test_data.map(|d| item_to_msgs(&seq_ty, d))) {
        (Some("up"), Some(Ok(data))) => {
            let up = run_up(data);
            Ok(TestResult { down: None, up: Some(up) })
        }

        (Some("up_fail"), Some(Ok(data))) => {
            let up = match run_up(data) {
                TestState::Pass => TestState::Fail,
                TestState::Fail => TestState::Pass,
                e => e,
            };
            Ok(TestResult { down: None, up: Some(up) })
        }

        (Some("dn"), Some(Ok(data))) => {
            let (res, down) = run_dn();
            let down = res.map(|x| if x == data { TestState::Pass } else { TestState::Fail }).unwrap_or(down);
            Ok(TestResult { down: Some(down), up: None })
        }

        (Some("both"), Some(Ok(data))) => {
            let (res, down) = run_dn();
            let down = res.map(|x| if x == data { TestState::Pass } else { TestState::Fail }).unwrap_or(down);
            let up = run_up(data);
            Ok(TestResult { down: Some(down), up: Some(up) })
        }

        (Some("roundtrip"), None)  => {
            let (res, down) = run_dn();
            let up = res.map(|msgs| run_up(msgs));
            Ok(TestResult { down: Some(down), up })
        }

        (_, Some(Err(()))) => {
            Err("Sequence in @test doesn't match sequence type")
        }

        _ => {
            Err("Invalid test args")
        }
    }
}
