use std::fs;
use std::path::Path;
use std::io::prelude::*;
use language::Ctxt;
use session::Session;
use connection::Connection;

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
    let loader = Ctxt::new(&sess);
    ::add_primitives(&loader);

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

    let file = loader.codemap.borrow_mut().add_file(fname.to_string_lossy().to_string(), source);
    let module = match loader.parse_module(&file) {
        Ok(m) => m,
        Err(e) => {
            println!("\tParse error: {}", e);
            return false;
        }
    };

    let mut success = true;

    for (count, test) in module.tests().iter().enumerate() {
        print!("\tTest #{}:", count+1);
        let compiled = test.compile(&loader);

        let (mut conn1, mut conn2) = if let Some(ref fields) = compiled.roundtrip_fields {
            Connection::new(fields)
        } else {
            (Connection::null(), Connection::null())
        };

        if let Some(stack) = compiled.down {
            let r = stack.run(&mut conn1, &mut Connection::null()) ^ test.should_fail();
            print!(" down:{}", if r { "ok" } else { "FAIL" });
            success &= r;
        }

        drop(conn1);

        if let Some(stack) = compiled.up {
            let r = stack.run(&mut conn2, &mut Connection::null()) ^ test.should_fail();
            print!(" up:{}", if r { "ok" } else { "FAIL" });
            success &= r;
        }
        println!("");
    }

    success
}
