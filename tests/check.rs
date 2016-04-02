extern crate signalspec;
use std::process;

fn main() {
    let mut success = true;
    success &= signalspec::run_test("tests/check");
    success &= signalspec::run_test("lib");
    process::exit( if success { 0 } else { 1 } );
}
