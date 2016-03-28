use std::thread;
use ast;
use session::Session;
use resolve;
use data::{DataMode, Shape};
use resolve::module_loader::Module;
use exec;
use nfa;
use dfa::{self, Dfa};

pub trait Process: Send {
    fn run(&self, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool;
    fn shape_up(&self) -> &Shape;
}

pub struct Program {
    pub dfa: Dfa,
    pub shape_down: Shape,
    pub shape_up: Shape,
}

impl Process for Program {
    fn run(&self, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        dfa::run(&self.dfa, downwards, upwards)
    }

    fn shape_up(&self) -> &Shape {
        &self.shape_up
    }
}

pub struct ProgramFlip {
    pub dfa: Dfa,
    pub shape_down: Shape,
    pub shape_up: Shape,
}

impl Process for ProgramFlip {
    fn run(&self, downwards: &mut exec::Connection, upwards: &mut exec::Connection) -> bool {
        dfa::run(&self.dfa, upwards, downwards)
    }

    fn shape_up(&self) -> &Shape {
        &self.shape_up
    }
}

pub fn resolve_process(sess: &Session, modscope: &Module, shape: &Shape, p: &ast::Process) -> Box<Process> {
    use {connection_io, dumpfile, vcd};
    match *p {
        ast::Process::Call(ref name, ref arg) => {
            let arg = resolve::rexpr(sess, &*modscope.scope.borrow(), arg);
            match &name[..] {
                "file" => connection_io::file_process(arg),
                "dump" => dumpfile::process(shape, arg),
                "vcd" => vcd::process(shape, arg),
                name => (box modscope.compile_call(name, shape.clone(), arg)
                  .ok().expect("Failed to compile call"))
            }
        }
        ast::Process::Block(ref block) => {
            let mut shape_up = Shape::null();
            let (step, _) = resolve::seq(sess, &*modscope.scope.borrow(), shape, &mut shape_up, block);
            let mut nfa = nfa::from_step_tree(&step);
            nfa.remove_useless_epsilons();
            let dfa = dfa::make_dfa(&nfa, &shape, &shape_up);
            box Program { dfa: dfa, shape_down: shape.clone(), shape_up: shape_up }
        }
        ast::Process::Literal(dir, ref shape_up_expr, ref block) => {
            let shape_item = resolve::rexpr(sess, &*modscope.scope.borrow(), shape_up_expr);
            let is_up = match dir {
                ast::ProcessLiteralDirection::Up => true,
                ast::ProcessLiteralDirection::Down => false,
                ast::ProcessLiteralDirection::Both => panic!("@both is only usable in tests"),
                ast::ProcessLiteralDirection::RoundTrip => panic!("@roundtrip is only usable in tests"),
            };
            let shape_up = shape_item.clone().into_shape(sess, DataMode { down: !is_up, up: is_up });
            let shape_flip = shape_item.into_shape(sess, DataMode { down: is_up, up: !is_up });

            let mut shape_dn = Shape::null();
            let (step, _) = resolve::seq(sess, &*modscope.scope.borrow(), &shape_flip, &mut shape_dn, block);

            let mut nfa = nfa::from_step_tree(&step);
            nfa.remove_useless_epsilons();
            let dfa = dfa::make_dfa(&nfa, &shape_flip, &shape_dn);

            box ProgramFlip { dfa: dfa, shape_down: shape_dn, shape_up: shape_up }
        }
    }
}

pub fn run_process_chain(processes: Vec<Box<Process>>) -> bool {
    let (_, mut connection) = exec::Connection::new(&Shape::null());
    let threads = processes.into_iter().map(|p| {
        let (mut c2, c1) = exec::Connection::new(p.shape_up());
        ::std::mem::swap(&mut c2, &mut connection);
        thread::spawn(move || {
            let mut downward = c2;
            let mut upward = c1;
            p.run(&mut downward, &mut upward)
        })
    }).collect::<Vec<_>>();

    threads.into_iter().all(|x| x.join().unwrap())
}
