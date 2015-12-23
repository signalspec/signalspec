extern crate signalspec;
extern crate env_logger;
use std::{env, process};

fn main() {
    env_logger::init().unwrap();

    let args: Vec<String> = env::args().collect();

    if args[1] == "--test" {
        return signalspec::run_test(&args[2]);
    }

    let success = signalspec::run(&args[1], &args[2..]);

    process::exit(if success { 0 } else { 1 });
}
