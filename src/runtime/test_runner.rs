use std::sync::Arc;
use std::fs;
use std::path::Path;

use crate::{DiagnosticHandler,  Handle, ChannelMessage };
use crate::runtime::channel::item_to_msgs;
use crate::syntax::{ ast, SourceFile };
use crate::core::{ Index, FileScope, rexpr, rexpr_tup };

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

    for def in &module.defs {
        for (count, attr) in def.attributes.iter().filter(|a| a.name.name == "test").enumerate() {
            print!("\tTest {} #{}:", def.name.name, count+1);
            let res = run_test(ui, &index, &module, def, attr);
    
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
    }

    success
}


pub struct TestResult {
    pub up: Option<bool>,
    pub down: Option<bool>
}

pub fn run_test(ui: &dyn DiagnosticHandler, index: &Index, file: &FileScope, def: &ast::Def, attr: &ast::Attribute) -> TestResult {
    assert!(def.bottom.name.name == "Seq", "Test def must use Seq");
    assert!(def.params.is_empty(), "Test def must not have parameters");

    let seq_ty = match &def.bottom.param {
        ast::Expr::Tup(t) if t.items.len() == 2 => rexpr(&file.scope, &t.items[0]),
        e => panic!("Invalid seq args")
    };

    let test_args = rexpr_tup(&file.scope, &attr.args);
    let test_args = test_args.as_tuple();
    let test_mode = test_args.first().and_then(|i| i.as_symbol());
    let test_data = test_args.get(1);

    let run_dn = || -> Result<Vec<ChannelMessage>, ()> {
        let (channel, mut handle) = Handle::seq_dn(index, seq_ty.clone());
        handle.compile_run(ui, index, &file.scope, &def.processes).expect("failed to compile").finish()?;

        let mut read = channel.read();
        Ok(std::iter::from_fn(|| read.pop()).collect())
    };

    let run_up = |messages: Vec<ChannelMessage>| -> Result<(), ()> {
        let (channel, mut handle) = Handle::seq_up(index, seq_ty.clone());
        for m in messages { channel.send(m); }
        channel.set_closed(true);
        handle.compile_run(ui, index, &file.scope, &def.processes).expect("failed to compile").finish()
    };

    match (test_mode, test_data.map(item_to_msgs)) {
        (Some("up"), Some(data)) => {
            let up = run_up(data).is_ok();
            TestResult { down: None, up: Some(up) }
        }

        (Some("up_fail"), Some(data)) => {
            let up = run_up(data).is_err();
            TestResult { down: None, up: Some(up) }
        }

        (Some("dn"), Some(data)) => {
            let down = run_dn() == Ok(data);
            TestResult { down: Some(down), up: None }
        }

        (Some("both"), Some(data)) => {
            let down = run_dn().as_ref() == Ok(&data);
            let up = run_up(data).is_ok();
            TestResult { down: Some(down), up: Some(up) }
        }

        (Some("roundtrip"), None)  => {
            let down_res = run_dn();
            let down = Some(down_res.is_ok());
            let up = down_res.ok().map(|msgs| run_up(msgs).is_ok());
            TestResult { down, up }
        }

        _ => panic!("Invalid test args {:?}", test_args)
    }
}
