extern crate signalspec;
extern crate env_logger;
extern crate argparse;

extern crate signalspec_vcd;
extern crate signalspec_starfish;

mod console;

use std::io::prelude::*;
use std::{ process, fs };
use std::path::PathBuf;

use argparse::{ArgumentParser, Collect, StoreOption};

fn main() {
    env_logger::init().unwrap();

    let mut test: Option<String> = None;
    let mut imports: Vec<String> = vec![];
    let mut cmds: Vec<String> = vec![];
    let mut debug: Option<String> = None;

    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut test)
            .add_option(&["-t"], StoreOption, "Run tests from FILE");
        ap.refer(&mut imports).
            add_option(&["-i"], Collect, "Import a module");
        ap.refer(&mut debug)
            .add_option(&["-d"], StoreOption, "Dump debug info to DIR");
        ap.refer(&mut cmds)
            .add_argument("process", Collect, "Processes to run");
        ap.parse_args_or_exit();
    }

    if let Some(path) = test {
        let success = signalspec::run_tests_in_file(&*path);
        process::exit( if success { 0 } else { 1 } );
    } else {
        let loader = signalspec::Ctxt::new(signalspec::Config {
            debug_dir: debug.map(PathBuf::from)
        });

        signalspec::add_primitives(&loader);
        signalspec_vcd::add_primitives(&loader);
        signalspec_starfish::add_primitives(&loader);

        for source_fname in imports {
            let mut source = String::new();
            fs::File::open(&source_fname).unwrap().read_to_string(&mut source).unwrap();
            let file = loader.codemap.borrow_mut().add_file(source_fname, source);
            loader.parse_module(&file).unwrap();
        }

        let mut stack = signalspec::Handle::base(&loader);
        for arg in cmds {
            stack = stack.parse_spawn(&arg).unwrap_or_else(|e| panic!("Error parsing argument: {}", e));
        }

        if stack.top_fields().direction().up {
            console::run(stack.top_shape(), &mut stack.connection());
        }

        let success = stack.join();

        process::exit(if success { 0 } else { 1 });
    }
}
