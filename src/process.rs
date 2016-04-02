use std::thread;
use ast;
use session::Session;
use resolve;
use data::{DataMode, Shape};
use resolve::module_loader::Module;
use resolve::Item;
use exec;
use nfa;
use dfa::{self, Dfa};

pub trait PrimitiveDef: Send {
    fn invoke_def(&self, &Shape, Item) -> Box<Process + 'static>;
}

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
    match *p {
        ast::Process::Call(ref name, ref arg) => {
            let arg = resolve::rexpr(sess, &*modscope.scope.borrow(), arg);
            match modscope.scope.borrow().get(name) {
                Some(item @ Item::Def(..)) => {
                    let (shape_up, step, _) = resolve::call(&item, sess, &shape, arg);

                    if let Some(mut f) = sess.debug_file(|| format!("{}.steps", name)) {
                        exec::write_step_tree(&mut f, &step, 0).unwrap_or_else(|e| error!("{}", e));
                    }

                    let mut nfa = nfa::from_step_tree(&step);

                    if let Some(mut f) = sess.debug_file(|| format!("{}.nfa.dot", name)) {
                        nfa.to_graphviz(&mut f).unwrap_or_else(|e| error!("{}", e));
                    }

                    nfa.remove_useless_epsilons();

                    if let Some(mut f) = sess.debug_file(|| format!("{}.cleaned.nfa.dot", name)) {
                        nfa.to_graphviz(&mut f).unwrap_or_else(|e| error!("{}", e));
                    }

                    let dfa = dfa::make_dfa(&nfa, &shape, &shape_up);

                    if let Some(mut f) = sess.debug_file(|| format!("{}.dfa.dot", name)) {
                        dfa.to_graphviz(&mut f).unwrap_or_else(|e| error!("{}", e));
                    }

                    box Program{ dfa: dfa, shape_down: shape.clone(), shape_up: shape_up}
                }
                Some(Item::PrimitiveDef(p)) => p.invoke_def(shape, arg),
                Some(_) => panic!("`{}` is not callable", name),
                None => panic!("`{}` does not exist in scope", name)
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
