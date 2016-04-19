extern crate signalspec;
extern crate env_logger;
extern crate argparse;

extern crate signalspec_vcd;

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
        let success = signalspec::run_test(&*path);
        process::exit( if success { 0 } else { 1 } );
    } else {
        let sess = signalspec::Session::new(debug.map(PathBuf::from));
        let loader = signalspec::ModuleLoader::new(&sess);

        signalspec::add_primitives(&loader);
        signalspec_vcd::load_plugin(&loader);

        for source_fname in imports {
            let mut source = String::new();
            fs::File::open(source_fname).unwrap().read_to_string(&mut source).unwrap();
            loader.parse_module(&source).unwrap();
        }

        let mut stack = signalspec::ProcessStack::new(&loader);

        for arg in cmds {
            stack.parse_add(&arg).unwrap_or_else(|e| panic!("Error parsing argument: {}", e));
        }

        let topmost_mode = stack.top_shape().data_mode();
        if topmost_mode.up {
            stack.add_print_process();
        }

        let success = stack.run();
        process::exit(if success { 0 } else { 1 });
    }
}
