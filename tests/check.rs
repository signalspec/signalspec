extern crate signalspec;
extern crate env_logger;
use std::process;

fn main() {
    env_logger::init().unwrap();
    let mut success = true;
    success &= signalspec::run_test("tests/check");
    success &= signalspec::run_test("lib");
    process::exit( if success { 0 } else { 1 } );
}
