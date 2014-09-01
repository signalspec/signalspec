use std::io::MemReader;
use std::task;

use {session, resolve, grammar, exec, dumpfile};

struct TestCode {
  step: exec::Step,
}

fn compile(source: &str) -> TestCode {
	let sess = session::Session::new();
  let mut ctx = resolve::Context::new(&sess);

  let module = grammar::module(source).unwrap();
  let prelude = resolve::Scope::new();

  let modscope = resolve::resolve_module(&mut ctx, &prelude, &module);
  let main = match *modscope.get("main").unwrap() {
    resolve::scope::DefItem(ref s) => s,
    _ => fail!("Main is not an event"),
  };

  let w = resolve::scope::EmptyItem;
  TestCode{ step: main.resolve_call(&mut ctx, &w, None) }
}

impl TestCode {
  fn up(&self, input: &'static str) -> bool {
    debug!("--- up")
    let (mut s1, mut s2) = exec::Connection::new();
    task::spawn(proc(){
      let mut reader = MemReader::new(input.as_bytes().to_vec());
      dumpfile::read_values(&mut reader, &mut s2);
    });
    exec::exec(&self.step, &mut s1)
  }

  fn down() -> String {
    unimplemented!();
  }

  fn up_pass(&self, _arg: &str, input: &'static str) {
    if !self.up(input) {
      fail!("up_pass test failed to match: {}", input);
    }
  }

  fn up_fail(&self, _arg: &str, input: &'static str) {
    if self.up(input) {
      fail!("up_fail test matched: {}", input);
    }
  }

  fn down_pass(&self, _arg: &str, _output: &str) {

  }
}

#[test]
fn test_seq() {
  let p = compile("
  def main (w) {
    #a
    #b
    #c
  }
    "
  );

  p.up_pass("", "#a \n #b \n #c");
  p.down_pass("", "#a \n #b \n #c");
  p.up_fail("", "#a \n #b \n #a");
  p.up_fail("", "#b \n #b \n #c");
}

#[test]
fn test_loop() {
  let p = compile("
  def main(w) {
    #a
    #b
    repeat {
      :> #c
      :> #d
    }
    :> #a
  }
  ");

  p.up_pass("", "#a \n #b \n #a");
  p.up_pass("", "#a \n #b \n #c \n #d \n #a");
  p.up_pass("", "#a \n #b \n #c \n #d \n #c \n #d \n #a");
  p.up_fail("", "#a \n #b \n #c \n #a");
  p.up_fail("", "#a \n #b \n #c \n #d");
}
