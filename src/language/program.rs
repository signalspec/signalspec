use super::{ ast, nfa, Module, Item };
use super::dfa::{ self, Dfa };
use data::{DataMode, Shape};
use process::Process;
use connection::Connection;
use session::Session;

pub struct Program {
    pub dfa: Dfa,
    pub shape_down: Shape,
    pub shape_up: Shape,
}

impl Process for Program {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
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
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        dfa::run(&self.dfa, upwards, downwards)
    }

    fn shape_up(&self) -> &Shape {
        &self.shape_up
    }
}

pub fn resolve_process(sess: &Session, modscope: &Module, shape: &Shape, p: &ast::Process) -> Box<Process> {
    match *p {
        ast::Process::Call(ref name, ref arg) => {
            let arg = super::expr::rexpr(sess, &*modscope.scope.borrow(), arg);
            match modscope.scope.borrow().get(name) {
                Some(item @ Item::Def(..)) => {
                    let (shape_up, step, _) = super::step::call(&item, sess, &shape, arg);

                    if let Some(mut f) = sess.debug_file(|| format!("{}.steps", name)) {
                        step.write_tree(&mut f, 0).unwrap_or_else(|e| error!("{}", e));
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
            let (step, _) = super::step::resolve_seq(sess, &*modscope.scope.borrow(), shape, &mut shape_up, block);
            let mut nfa = nfa::from_step_tree(&step);
            nfa.remove_useless_epsilons();
            let dfa = dfa::make_dfa(&nfa, &shape, &shape_up);
            box Program { dfa: dfa, shape_down: shape.clone(), shape_up: shape_up }
        }
        ast::Process::Literal(dir, ref shape_up_expr, ref block) => {
            let shape_item = super::expr::rexpr(sess, &*modscope.scope.borrow(), shape_up_expr);
            let is_up = match dir {
                ast::ProcessLiteralDirection::Up => true,
                ast::ProcessLiteralDirection::Down => false,
                ast::ProcessLiteralDirection::Both => panic!("@both is only usable in tests"),
                ast::ProcessLiteralDirection::RoundTrip => panic!("@roundtrip is only usable in tests"),
            };
            let shape_up = shape_item.clone().into_shape(sess, DataMode { down: !is_up, up: is_up });
            let shape_flip = shape_item.into_shape(sess, DataMode { down: is_up, up: !is_up });

            let mut shape_dn = Shape::null();
            let (step, _) = super::step::resolve_seq(sess, &*modscope.scope.borrow(), &shape_flip, &mut shape_dn, block);

            let mut nfa = nfa::from_step_tree(&step);
            nfa.remove_useless_epsilons();
            let dfa = dfa::make_dfa(&nfa, &shape_flip, &shape_dn);

            box ProgramFlip { dfa: dfa, shape_down: shape_dn, shape_up: shape_up }
        }
    }
}
