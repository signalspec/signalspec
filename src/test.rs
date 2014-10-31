use std::io::{MemReader, MemWriter};
use std::str;
use std::task;

use {ast, session, resolve, grammar, exec, eval, dumpfile};
use resolve::types;
use resolve::scope::{Dynamic, Ignored,ValueItem,TupleItem};
use resolve::context::ValueID;
use data_usage;

struct TestCode {
    step: exec::Step,
    args: Vec<ValueID>,
}

fn compile(source: &str) -> TestCode {
    let sess = session::Session::new();
    let mut signal_info = resolve::SignalInfo::new();
    let mut ctx = resolve::Context::new(&sess);

    let module = grammar::module(source).unwrap();
    let prelude = resolve::Scope::new();

    let modscope = resolve::resolve_module(&mut ctx, &prelude, &module);
    let main = match modscope.get("main").unwrap() {
        resolve::scope::DefItem(s) => s,
        _ => fail!("Main is not an event"),
    };

    let mut args = Vec::new();

    let param = {
        let make_reg = || {
            let reg = ctx.make_register();
            args.push(reg);
            ValueItem(types::Bottom, Ignored, Dynamic(reg))
        };

        match main.ast.param {
            ast::TupExpr(ref v) => {
                TupleItem(v.iter().map(|_| make_reg()).collect())
            }
            _ => make_reg()
        }
    };

    let mut step = main.resolve_call(&mut ctx, &mut signal_info, param, None);
    data_usage::pass(&mut step, &mut signal_info);
    TestCode{ step: step, args: args }
}

impl TestCode {
    fn up(&self, arg: &str, bottom: &'static str, top: &'static str) -> bool {
        debug!("--- up")
        let (mut s1, mut s2) = exec::Connection::new();
        let (mut t1, mut t2) = exec::Connection::new();
        task::spawn(proc(){
            let mut reader = MemReader::new(bottom.as_bytes().to_vec());
            dumpfile::read_values(&mut reader, &mut s2);
        });
        task::spawn(proc(){
            let mut writer = MemWriter::new();
            dumpfile::write_values(&mut writer, &mut t2);
            let v = writer.unwrap();
            assert_eq!(top, str::from_utf8(v[]).unwrap());
        });
        let mut state = eval::State::new();

        let r = exec::exec(&mut state, &self.step, &mut s1, &mut t1);

        if r {
            let args = dumpfile::parse_line(arg);
            for (argval, &id) in args.iter().zip(self.args.iter()) {
                assert_eq!(argval, state.get(id))
            }
        }

        r
    }

    fn down() -> String {
        unimplemented!();
    }

    fn up_pass(&self, arg: &str, bottom: &'static str, top: &'static str) {
        if !self.up(arg, bottom, top) {
            fail!("up_pass test failed to match: {}", bottom);
        }
    }

    fn up_fail(&self, arg: &str, bottom: &'static str, top: &'static str) {
        if self.up(arg, bottom, top) {
            fail!("up_fail test matched: {}", bottom);
        }
    }

    fn down_pass(&self, _arg: &str, _bottom: &'static str, _top: &'static str) {

    }
}

#[test]
fn test_seq() {
    let p = compile("
    def main () {
        #a
        #b
        #c
    }
        "
    );

    p.up_pass("", "#a \n #b \n #c", "");
    p.down_pass("", "#a \n #b \n #c", "");
    p.up_fail("", "#a \n #b \n #a", "");
    p.up_fail("", "#b \n #b \n #c", "");
}

#[test]
fn test_arg() {
    let p = compile("
    def main(a, b, c) {
        a
        b
        c
    }
    ");

    p.up_pass("#x, #y, #z", "#x \n #y \n #z", "");
}

#[test]
fn test_loop() {
    let p = compile("
    def main(x) {
        #a
        #b
        repeat x {
            #c
            #d
        }
        #a
    }
    ");

    p.up_pass("#0", "#a \n #b \n #a", "");
    p.up_pass("#1", "#a \n #b \n #c \n #d \n #a", "");
    p.up_pass("#2", "#a \n #b \n #c \n #d \n #c \n #d \n #a", "");
    p.up_fail("", "#a \n #b \n #c \n #a", "");
    p.up_fail("", "#a \n #b \n #c \n #d", "");
}

#[test]
fn test_nested_loop() {
    let p = compile("
    def main() {
        #a
        repeat {
            repeat {
                #b
            }
            repeat {
                #c
            }
            #d
        }
        #e
    }
    ");

    p.up_pass("", "#a \n #e", "");
    p.up_pass("", "#a \n #b \n #c \n #d \n #e", "");
    p.up_pass("", "#a \n #b \n #b \n #c \n #c \n #c \n #d \n #b \n #d \n #e", "");
    p.up_fail("", "#a \n #b \n #c", "");
}

#[test]
fn test_tup() {
    let p = compile("
        def main(w) {
            (#a, #b)
            (#c, #d)
        }
    ");

    p.up_pass("", "#a, #b \n #c, #d", "");
    p.up_fail("", "#a, #d \n #c, #b", "");
}

#[test]
fn test_on() {
    let p = compile("
        def main() {
            on 1 {}
            on 2 {}
            on a {
                (a+1)
            }
        }
    ");

    p.up_pass("", "5", "1\n2\n4\n")
}
