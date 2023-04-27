use signalspec::{self, Index};
use env_logger;
use std::{process, path::Path};

fn main() {
    let ui = &signalspec::diagnostic::SimplePrintHandler;
    env_logger::init();
    let mut index = Index::new();
    index.load(ui, Path::new("tests/min-prelude.signalspec")).unwrap();

    let mut success = true;
    success &= signalspec::runtime::run_tests(ui, &index, &Path::new("tests/check"));
    success &= signalspec::runtime::run_tests(ui, &index, &Path::new("lib"));
    process::exit( if success { 0 } else { 1 } );
}
