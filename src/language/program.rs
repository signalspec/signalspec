use super::{ ast, nfa, PrimitiveDef, PrimitiveDefFields };
use super::dfa::{ self, Dfa };
use super::scope::Scope;
use protocol::{ Shape, Fields };
use super::protocol::{ ProtocolScope, DefImpl, resolve_protocol_invoke };
use super::step::resolve_seq;
use super::Ctxt;
use data::{DataMode};
use process::{ Process, ProcessStack, ProcessInfo };
use connection::{ Connection, ConnectionMessage };
use std::sync::mpsc;

pub struct Program {
    pub dfa: Dfa
}

impl Process for Program {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        dfa::run(&self.dfa, downwards, upwards)
    }
}

pub struct ProgramFlip {
    pub dfa: Dfa,
}

impl Process for ProgramFlip {
    fn run(&self, downwards: &mut Connection, upwards: &mut Connection) -> bool {
        dfa::run(&self.dfa, upwards, downwards)
    }
}

fn compile_block(ctx: &Ctxt,
                     scope: &Scope,
                     protocol_scope: &ProtocolScope,
                     shape_down: &Shape,
                     fields_down: &Fields,
                     shape_up: Shape,
                     seq: &ast::Block,
                     name: &str) -> ProcessInfo {
    let mut step = resolve_seq(ctx, &scope, protocol_scope, shape_down, &shape_up, seq);

    let mut fields_up = shape_up.fields(DataMode { down: false, up: true });
    super::step::infer_direction(&mut step, &fields_down, &mut fields_up);

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

    ProcessInfo { fields_up, shape_up, implementation: Box::new(Program{dfa}) }
}

fn call_primitive(_ctx: &Ctxt,
                  scope: &Scope,
                  fields_down: &Fields,
                  shape_up: Shape,
                  primitive_impls: &[PrimitiveDef],
                  name: &str) -> ProcessInfo {
    for def in primitive_impls {
        if fields_down == &def.fields_down {
            info!("Using {} for primitive at {}", def.id, name);
            let fields_up = match def.fields_up {
                PrimitiveDefFields::Explicit(ref fields) => fields.clone(),
                PrimitiveDefFields::Auto(dir) => shape_up.fields(dir),
            };

            let implementation = (def.instantiate)(scope).expect("Failed to instantiate primitive");

            return ProcessInfo { fields_up, shape_up, implementation };
        }
    }

    panic!("No matching call for primitive {} for {:?}", name, fields_down);
}

pub fn resolve_process(ctx: &Ctxt,
                       scope: &Scope,
                       protocol_scope: &ProtocolScope,
                       shape_down: &Shape,
                       fields_down: &Fields,
                       p: &ast::Process) -> ProcessInfo {
    match *p {
        ast::Process::Call(ref name, ref arg) => {
            let arg = super::expr::rexpr(ctx, scope, arg);
            let (scope, imp, shape_up) = protocol_scope.find(ctx, shape_down, name, arg);

            match *imp {
                DefImpl::Code(ref seq) => {
                    compile_block(ctx, &scope, protocol_scope, shape_down, fields_down, shape_up, seq, &name)
                }
                DefImpl::Primitive(ref primitive) => {
                    call_primitive(ctx, &scope, fields_down, shape_up, primitive, &name)
                },
            }
        }

        ast::Process::Block(ref block) => {
            let shape_up = Shape::None;
            compile_block(ctx, scope, protocol_scope, shape_down, fields_down, shape_up, block, "anon_block")
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

fn make_literal_process(ctx: &Ctxt,
                        scope: &Scope,
                        protocol_scope: &ProtocolScope,
                        is_up: bool,
                        shape_up_expr: &ast::ProtocolRef,
                        block: &ast::Block) -> ProcessInfo {
    let shape_up = resolve_protocol_invoke(ctx, scope, shape_up_expr);

    let mut shape_down = Shape::None;
    let mut step = super::step::resolve_seq(ctx, scope, protocol_scope, &shape_up, &mut shape_down, block);

    let mut fields_dn = shape_down.fields(DataMode { down: false, up: false });
    let fields_up = shape_up.fields(DataMode { down: !is_up, up: is_up });
    let fields_flip = shape_up.fields(DataMode { down: is_up, up: !is_up });
    super::step::infer_direction(&mut step, &fields_flip, &mut fields_dn);

    let mut nfa = nfa::from_step_tree(&step);
    nfa.remove_useless_epsilons();
    let dfa = dfa::make_dfa(&nfa, &fields_flip, &fields_dn);

    ProcessInfo { fields_up, shape_up, implementation: Box::new(ProgramFlip { dfa })}
}

pub struct CompiledTest<'a> {
    pub down: Option<ProcessStack<'a>>,
    pub up: Option<ProcessStack<'a>>,
}

pub fn compile_test<'a>(ctx: &'a Ctxt<'a>,
                        scope: &Scope,
                        protocol_scope: &ProtocolScope,
                        test: &ast::Test) -> CompiledTest<'a> {

    fn build_stack<'a>(ctx: &'a Ctxt<'a>, scope: &Scope, protocol_scope: &ProtocolScope,
                       bottom_process: ProcessInfo, ast: &[ast::Process]) -> ProcessStack<'a> {
        let mut stack = ProcessStack::new(ctx);
        stack.add(bottom_process);

        for process_ast in ast {
            let process = resolve_process(ctx, scope, protocol_scope, stack.top_shape(), stack.top_fields(), process_ast);
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
            let shape_dn = resolve_protocol_invoke(ctx, scope, ty);
            let shape_up = resolve_protocol_invoke(ctx, scope, ty);

            let (s, r) = mpsc::channel();
            let process_dn = ProcessInfo {
                fields_up: shape_dn.fields(DataMode { down: true, up: false }),
                shape_up: shape_dn,
                implementation: Box::new(Collect { sender: s })
            };
            let process_up = ProcessInfo {
                fields_up: shape_up.fields(DataMode { down: false, up: true }),
                shape_up: shape_up,
                implementation: Box::new(Emit { receiver: r })
            };

            CompiledTest {
                down: Some(build_stack(ctx, scope, protocol_scope, process_dn, rest)),
                up:   Some(build_stack(ctx, scope, protocol_scope, process_up, rest)),
            }
        }

        Some((first, rest)) => {
            let bottom = resolve_process(ctx, scope, protocol_scope, &Shape::None, &Fields::null(), first);
            let is_up = bottom.fields_up.direction().up;
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
    sender: mpsc::Sender<ConnectionMessage>,
}

impl Process for Collect {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        while let Ok(d) = upwards.recv() {
            if self.sender.send(d).is_err() { return false; }
        }
        true
    }
}

struct Emit {
    receiver: mpsc::Receiver<ConnectionMessage>,
}

impl Process for Emit {
    fn run(&self, _: &mut Connection, upwards: &mut Connection) -> bool {
        while let Ok(d) = self.receiver.recv() {
            if upwards.send(d).is_err() { return false; }
        }
        true
    }
}
