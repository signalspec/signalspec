use signalspec;
use env_logger;
use std::process;

fn main() {
    env_logger::init().unwrap();
    let mut success = true;
    success &= signalspec::run_tests_in_file("tests/check");
    success &= signalspec::run_tests_in_file("lib");
    process::exit( if success { 0 } else { 1 } );
}
