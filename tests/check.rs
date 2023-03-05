use signalspec;
use env_logger;
use std::{process, path::Path};

fn main() {
    let ui = &signalspec::diagnostic::SimplePrintHandler;
    env_logger::init();
    let mut success = true;
    success &= signalspec::runtime::run_tests(ui, &Path::new("tests/check"));
    success &= signalspec::runtime::run_tests(ui, &Path::new("lib"));
    process::exit( if success { 0 } else { 1 } );
}
