use super::{ ast, PrimitiveDef, PrimitiveDefFields };
use super::scope::Scope;
use protocol::{ Shape, Fields };
use super::protocol::{ ProtocolScope, DefImpl, resolve_protocol_invoke };
use super::step::{ compile_block, make_literal_process };
use super::Ctxt;
use data::{DataMode};
use process::{ Process, ProcessStack, ProcessInfo };
use connection::{ Connection, ConnectionMessage };
use std::sync::mpsc;


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
            let dn_base = make_literal_process(ctx, scope, protocol_scope, false, shape_expr, block);
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
