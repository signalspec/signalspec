mod console;
mod diagnostic;

use std::io::prelude::*;
use std::sync::Arc;
use std::{ process, fs };

use argparse::{ArgumentParser, Collect, StoreOption, Store};

fn main() {
    env_logger::init();

    let mut test: Option<String> = None;
    let mut imports: Vec<String> = vec![];
    let mut cmd: String = String::new();
    let mut debug: Option<String> = None;

    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut test)
            .add_option(&["-t"], StoreOption, "Run tests from FILE");
        ap.refer(&mut imports).
            add_option(&["-i"], Collect, "Import a module");
        ap.refer(&mut debug)
            .add_option(&["-d"], StoreOption, "Dump debug info to DIR");
        ap.refer(&mut cmd)
            .add_argument("process", Store, "Process to run");
        ap.parse_args_or_exit();
    }

    if let Some(path) = test {
        let success = signalspec::run_tests_in_file(&*path);
        process::exit( if success { 0 } else { 1 } );
    } else {
        let mut index = signalspec::Index::new();

        signalspec::add_primitives(&mut index);

        for source_fname in imports {
            let mut source = String::new();
            fs::File::open(&source_fname).unwrap().read_to_string(&mut source).unwrap();
            let file = Arc::new(signalspec::SourceFile::new(source_fname, source));
            index.parse_module(file).unwrap();
        }

        let ui = &diagnostic::CliHandler;

        let success = signalspec::parse_compile_run(ui, &index, &cmd).unwrap();

        process::exit(if success { 0 } else { 1 });
    }
}

