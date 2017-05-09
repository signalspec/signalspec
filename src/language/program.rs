use super::{ ast, nfa };
use super::dfa::{ self, Dfa };
use super::scope::Scope;
use protocol::{ Shape, Fields };
use super::protocol::{ ProtocolScope, resolve_protocol_invoke };
use super::Ctxt;
use data::{DataMode};
use process::{ Process, ProcessStack };
use connection::{ Connection, ConnectionMessage };
use std::sync::mpsc;

pub struct Program {
    pub dfa: Dfa,
    pub shape_down: Shape,
    pub shape_up: Shape,
    pub fields_up: Fields,
}

impl Process for Program {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        dfa::run(&self.dfa, downwards, upwards)
    }

    fn shape_up(&self) -> &Shape { &self.shape_up }

    fn fields_up(&self) -> &Fields { &self.fields_up }
}

pub struct ProgramFlip {
    pub dfa: Dfa,
    pub shape_down: Shape,
    pub shape_up: Shape,
    pub fields_up: Fields,
}

impl Process for ProgramFlip {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        dfa::run(&self.dfa, upwards, downwards)
    }

    fn shape_up(&self) -> &Shape {
        &self.shape_up
    }

    fn fields_up(&self) -> &Fields { &self.fields_up }
}

pub fn resolve_process<'s>(ctx: &Ctxt<'s>,
                           scope: &Scope<'s>,
                           protocol_scope: &ProtocolScope<'s>,
                           shape: &Shape,
                           p: &'s ast::Process) -> Box<Process> {
    match *p {
        ast::Process::Call(ref name, ref arg) => {
            let arg = super::expr::rexpr(ctx.session, scope, arg);
            let (shape_up, mut step, ri) = protocol_scope.call(ctx, shape, name, arg);

            let fields_down = shape.fields();
            let mut fields_up = shape_up.fields();
            let ri2 = super::step::infer_direction(&mut step, &fields_down, &mut fields_up);
            assert_eq!(ri, ri2);

            if let Some(mut f) = ctx.session.debug_file(|| format!("{}.steps", name)) {
                step.write_tree(&mut f, 0).unwrap_or_else(|e| error!("{}", e));
            }

            let mut nfa = nfa::from_step_tree(&step);

            if let Some(mut f) = ctx.session.debug_file(|| format!("{}.nfa.dot", name)) {
                nfa.to_graphviz(&mut f).unwrap_or_else(|e| error!("{}", e));
            }

            nfa.remove_useless_epsilons();

            if let Some(mut f) = ctx.session.debug_file(|| format!("{}.cleaned.nfa.dot", name)) {
                nfa.to_graphviz(&mut f).unwrap_or_else(|e| error!("{}", e));
            }

            let dfa = dfa::make_dfa(&nfa, &fields_down, &fields_up);

            if let Some(mut f) = ctx.session.debug_file(|| format!("{}.dfa.dot", name)) {
                dfa.to_graphviz(&mut f).unwrap_or_else(|e| error!("{}", e));
            }

            box Program{ dfa, fields_up, shape_down: shape.clone(), shape_up }
        }
        ast::Process::Block(ref block) => {
            let mut shape_up = Shape::null();
            let (mut step, ri) = super::step::resolve_seq(ctx, scope, protocol_scope, shape, &mut shape_up, block);


            let fields_down = shape.fields();
            let mut fields_up = shape_up.fields();
            let ri2 = super::step::infer_direction(&mut step, &fields_down, &mut fields_up);
            assert_eq!(ri, ri2, "Failed on block {:?}", block);

            let mut nfa = nfa::from_step_tree(&step);
            nfa.remove_useless_epsilons();
            let dfa = dfa::make_dfa(&nfa, &fields_down, &fields_up);
            box Program { dfa, shape_down: shape.clone(), shape_up, fields_up }
        }
        ast::Process::Literal(dir, ref shape_up_expr, ref block) => {
            let is_up = match dir {
                ast::ProcessLiteralDirection::Up => true,
                ast::ProcessLiteralDirection::Down => false,
                ast::ProcessLiteralDirection::Both => panic!("@both is only usable in tests"),
                ast::ProcessLiteralDirection::RoundTrip => panic!("@roundtrip is only usable in tests"),
            };

            make_literal_process(ctx, scope, protocol_scope, is_up, shape_up_expr, block)
        }
    }
}

fn make_literal_process<'s>(ctx: &Ctxt<'s>,
                            scope: &Scope<'s>,
                            protocol_scope: &ProtocolScope<'s>,
                            is_up: bool,
                            shape_up_expr: &'s ast::ProtocolRef,
                            block: &'s ast::Block) -> Box<Process> {
    let shape_up = resolve_protocol_invoke(ctx, scope, shape_up_expr, DataMode { down: !is_up, up: is_up });
    let shape_flip = resolve_protocol_invoke(ctx, scope, shape_up_expr, DataMode { down: is_up, up: !is_up });

    let mut shape_down = Shape::null();
    let (mut step, ri) = super::step::resolve_seq(ctx, scope, protocol_scope, &shape_flip, &mut shape_down, block);

    let mut fields_dn = shape_down.fields();
    let fields_flip = shape_flip.fields();
    let ri2 = super::step::infer_direction(&mut step, &fields_flip, &mut fields_dn);
    assert_eq!(ri, ri2);

    let mut nfa = nfa::from_step_tree(&step);
    nfa.remove_useless_epsilons();
    let dfa = dfa::make_dfa(&nfa, &fields_flip, &fields_dn);

    box ProgramFlip { dfa, fields_up: shape_up.fields(), shape_down, shape_up }
}

pub struct CompiledTest<'a> {
    pub down: Option<ProcessStack<'a>>,
    pub up: Option<ProcessStack<'a>>,
}

pub fn compile_test<'a>(ctx: &'a Ctxt<'a>,
                        scope: &Scope<'a>,
                        protocol_scope: &ProtocolScope<'a>,
                        test: &'a ast::Test) -> CompiledTest<'a> {

    fn build_stack<'a>(ctx: &'a Ctxt<'a>, scope: &Scope<'a>, protocol_scope: &ProtocolScope<'a>,
                       bottom_process: Box<Process>, ast: &'a [ast::Process]) -> ProcessStack<'a> {
        let mut stack = ProcessStack::new(ctx);
        stack.add(bottom_process);

        for process_ast in ast {
            let process = resolve_process(ctx, scope, protocol_scope, stack.top_shape(), process_ast);
            stack.add(process);
        }

        stack
    }

    // If the test uses `@both`, generate `@up` and `@dn` versions and run them
    match test.processes.split_first() {
        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::Both, ref shape_expr, ref block), rest)) => {
            let dn_base = make_literal_process(ctx, scope, protocol_scope, true, shape_expr, block);
            let up_base = make_literal_process(ctx, scope, protocol_scope, true, shape_expr, block);

            CompiledTest {
                down: Some(build_stack(ctx, scope, protocol_scope, dn_base, rest)),
                up:   Some(build_stack(ctx, scope, protocol_scope, up_base, rest)),
            }
        }

        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::RoundTrip, ref ty, _), rest)) => {
            let shape_dn = resolve_protocol_invoke(ctx, scope, ty, DataMode { down: true, up: false });
            let shape_up = resolve_protocol_invoke(ctx, scope, ty, DataMode { down: false, up: true });

            let (s, r) = mpsc::channel();
            let process_dn = box Collect { fields: shape_dn.fields(), shape: shape_dn, sender: s };
            let process_up = box Emit { fields: shape_up.fields(), shape: shape_up, receiver: r };

            CompiledTest {
                down: Some(build_stack(ctx, scope, protocol_scope, process_dn, rest)),
                up:   Some(build_stack(ctx, scope, protocol_scope, process_up, rest)),
            }
        }

        Some((first, rest)) => {
            let bottom = resolve_process(ctx, scope, protocol_scope, &Shape::null(), first);
            let is_up = bottom.shape_up().data_mode().up;
            let stack = build_stack(ctx, scope, protocol_scope, bottom, rest);

            let (up, down) = if is_up { (Some(stack), None) } else { (None, Some(stack)) };
            CompiledTest {
                down: down,
                up:   up,
            }
        }

        None => CompiledTest { down: None, up: None }
    }
}

struct Collect {
    shape: Shape,
    fields: Fields,
    sender: mpsc::Sender<ConnectionMessage>,
}

impl Process for Collect {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        while let Ok(d) = upwards.recv() {
            if self.sender.send(d).is_err() { return false; }
        }
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.shape
    }

    fn fields_up(&self) -> &Fields { &self.fields }
}

struct Emit {
    shape: Shape,
    fields: Fields,
    receiver: mpsc::Receiver<ConnectionMessage>,
}

impl Process for Emit {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        while let Ok(d) = self.receiver.recv() {
            if upwards.send(d).is_err() { return false; }
        }
        true
    }

    fn shape_up(&self) -> &Shape {
        &self.shape
    }

    fn fields_up(&self) -> &Fields { &self.fields }
}
