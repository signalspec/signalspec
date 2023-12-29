use signalspec::{self, Index, diagnostic::print_diagnostics};
use env_logger;
use std::{process, path::Path};

fn main() {
    env_logger::init();

    let mut index = Index::new();
    index.load(Path::new("tests/min-prelude.signalspec")).unwrap();
    let index = index.validate().unwrap();

    let mut success = true;
    success &= signalspec::runtime::run_tests(&print_diagnostics, &index, &Path::new("tests/check"));
    success &= signalspec::runtime::run_tests(&print_diagnostics, &index, &Path::new("lib"));
    process::exit( if success { 0 } else { 1 } );
}
