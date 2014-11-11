use {ast, grammar};
use resolve::types;
use session::Session;

fn v(s: &str) -> ast::Value{
    grammar::literal(s).unwrap()
}

fn shape_none() -> types::Shape { types::ShapeTup(Vec::new()) } // needs CTFE
const SHAPE_UP_ANY: types::Shape = types::ShapeUnknown(false, true);
const SHAPE_UP_VAL: types::Shape = types::ShapeVal(types::Bottom, false, true);
const SHAPE_DOWN_VAL: types::Shape = types::ShapeUnknown(true, false);
const SHAPE_DOWN_ANY: types::Shape = types::ShapeVal(types::Bottom, true, false);

#[test]
fn test_seq() {
    let s = Session::new();
    let m = s.parse_module("
    def main () {
        #a
        #b
        #c
    }
    "
    ).unwrap();

    let p = m.compile_call("main", SHAPE_UP_ANY, shape_none(), ()).unwrap();

    p.run_test_pass("#a \n #b \n #c", "");
    //p.down_pass("", "#a \n #b \n #c", "");
    p.run_test_fail("#a \n #b \n #a", "");
    p.run_test_fail("#b \n #b \n #c", "");
}

#[test]
fn test_arg() {
    let s = Session::new();
    let m = s.parse_module("
    def main(a, b, c) {
        a
        b
        c
    }
    ").unwrap();

    let a = s.var(types::Symbol, false, true);
    let b = s.var(types::Symbol, false, true);
    let c = s.var(types::Symbol, false, true);
    let p = m.compile_call("main", SHAPE_UP_ANY, shape_none(), tuple_item![a, b, c]).unwrap();

    let env = p.run_test_pass("#x \n #y \n #z", "");
    assert_eq!(env.get_var(a), &v("#x"));
    assert_eq!(env.get_var(b), &v("#y"));
    assert_eq!(env.get_var(c), &v("#z"));
}

#[test]
fn test_let() {
    let s = Session::new();
    let m = s.parse_module("
        let x = #x
        def main(a) {
            let y = a
            let z = #z
            x
            y
            z
        }
    ").unwrap();

    let a = s.var(types::Symbol, false, true);
    let p = m.compile_call("main",  SHAPE_UP_ANY, shape_none(), a).unwrap();

    let env = p.run_test_pass( "#x \n #y \n #z", "");
    assert_eq!(env.get_var(a), &v("#y"));
}

#[test]
fn test_loop() {
    let s = Session::new();
    let m = s.parse_module("
    def main(x) {
        #a
        #b
        repeat x {
            #c
            #d
        }
        #a
    }
    ").unwrap();

    let x = s.var(types::Integer, false, true);
    let p = m.compile_call("main", SHAPE_UP_ANY, shape_none(), x).unwrap();

    let env = p.run_test_pass("#a \n #b \n #a", "");
    assert_eq!(env.get_var(x), &v("#0"));

    let env = p.run_test_pass("#a \n #b \n #c \n #d \n #a", "");
    assert_eq!(env.get_var(x), &v("#1"));

    let env = p.run_test_pass("#a \n #b \n #c \n #d \n #c \n #d \n #a", "");
    assert_eq!(env.get_var(x), &v("#2"));

    p.run_test_fail("#a \n #b \n #c \n #a", "");
    p.run_test_fail("#a \n #b \n #c \n #d", "");
}

#[test]
fn test_nested_loop() {
    let s = Session::new();
    let m = s.parse_module("
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
    ").unwrap();

    let p = m.compile_call("main", SHAPE_UP_ANY, shape_none(), ()).unwrap();

    p.run_test_pass("#a \n #e", "");
    p.run_test_pass("#a \n #b \n #c \n #d \n #e", "");
    p.run_test_pass("#a \n #b \n #b \n #c \n #c \n #c \n #d \n #b \n #d \n #e", "");
    p.run_test_fail("#a \n #b \n #c", "");
}

#[test]
fn test_unbounded_loop() {
    let s = Session::new();
    let m = s.parse_module("
    def main() {
      repeat {
        on v {
          repeat v { #h }
          repeat ignore { #l }
        }
      }
    }
    ").unwrap();

    let p = m.compile_call("main", SHAPE_UP_ANY, SHAPE_UP_VAL, ()).unwrap();

    p.run_test_pass("#h \n #l \n #h \n #h \n #l \n #h \n #h \n #h", "1\n2\n3\n");
}

#[test]
fn test_tup() {
    let s = Session::new();
    let m = s.parse_module("
        def main() {
            (#a, #b)
            (#c, #d)
        }
    ").unwrap();

    let p = m.compile_call("main", SHAPE_UP_ANY, shape_none(), ()).unwrap();

    p.run_test_pass("#a, #b \n #c, #d", "");
    p.run_test_fail("#a, #d \n #c, #b", "");
}

#[test]
fn test_on() {
    let s = Session::new();
    let m = s.parse_module("
        def main() {
            on 1 {}
            on 2 {}
            on a {
                (a+1)
            }
        }
    ").unwrap();

    let p = m.compile_call("main", SHAPE_UP_ANY, SHAPE_UP_VAL, ()).unwrap();

    p.run_test_pass("5", "1\n2\n4\n");
}
