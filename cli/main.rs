mod console;
mod diagnostic;

use std::path::{PathBuf, Path};
use std::process;

use argparse::{ArgumentParser, Collect, StoreOption, Store};
use diagnostic::report_diagnostics;

fn main() {
    env_logger::init();

    let mut test: Option<String> = None;
    let mut imports: Vec<String> = vec![];
    let mut cmd: String = String::new();
    let mut debug: Option<String> = None;
    let mut dump_ast: Option<PathBuf> = None;

    {
        let mut ap = ArgumentParser::new();
        ap.refer(&mut test)
            .add_option(&["-t"], StoreOption, "Run tests from FILE");
        ap.refer(&mut dump_ast)
            .add_option(&["--dump-ast"], StoreOption, "Dump AST from FILE");
        ap.refer(&mut imports).
            add_option(&["-i"], Collect, "Import a module");
        ap.refer(&mut debug)
            .add_option(&["-d"], StoreOption, "Dump debug info to DIR");
        ap.refer(&mut cmd)
            .add_argument("process", Store, "Process to run");
        ap.parse_args_or_exit();
    }

    let mut index = signalspec::Index::from_env().unwrap();

    for source_fname in imports {
        index.load(Path::new(&source_fname)).unwrap();
    }

    report_diagnostics(index.diagnostics());

    if let Some(dump_ast) = dump_ast {
        let file = signalspec::syntax::SourceFile::load(&dump_ast).unwrap();
        let ast = signalspec::syntax::parse_module(file.source()).unwrap();
        signalspec::syntax::dump_tree(&mut std::io::stdout(), &file, &ast, 0).unwrap();
        process::exit(0);
    } if let Some(path) = test {
        let success = signalspec::runtime::run_tests(&report_diagnostics, &index, &Path::new(&path));
        process::exit( if success { 0 } else { 1 } );
    } else {
        let mut handle = signalspec::Handle::base(&index);
        let inner_handle = handle.parse_compile_run(&index, &cmd).unwrap_or_else(|e| {
            report_diagnostics(e);
            eprintln!("Exiting due to previous errors");
            process::exit(2);
        });

        let success = console::run(inner_handle).is_ok();

        process::exit(if success { 0 } else { 1 });
    }
}
