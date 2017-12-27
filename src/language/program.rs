use super::ast;
use super::scope::Scope;
use protocol::{ Shape, Fields };
use super::protocol::{ ProtocolScope, DefImpl, resolve_protocol_invoke };
use super::step::{ compile_block, make_literal_process };
use super::Ctxt;
use super::primitive::call_primitive;
use process::{ ProcessStack, ProcessInfo };
use data::DataMode;

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
                    let (step, fields_up) = call_primitive(ctx, &scope, fields_down, &shape_up, primitive, &name);
                    ProcessInfo { step, fields_up, shape_up }
                },
            }
        }

        ast::Process::Block(ref block) => {
            compile_block(ctx, scope, protocol_scope, shape_down, fields_down, Shape::None, block, "anon_block")
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
    pub roundtrip_fields: Option<Fields>,
}

pub fn compile_test<'a>(ctx: &'a Ctxt<'a>,
                        scope: &Scope,
                        protocol_scope: &ProtocolScope,
                        test: &ast::Test) -> CompiledTest<'a> {

    fn build_stack<'a>(ctx: &'a Ctxt<'a>, scope: &Scope, protocol_scope: &ProtocolScope,
                       shape: Option<(Shape, Fields)>, bottom_process: Option<ProcessInfo>, ast: &[ast::Process]) -> ProcessStack<'a> {
        let mut stack = if let Some((shape, fields)) = shape {
            ProcessStack::with_shape(ctx, shape, fields)
        } else { ProcessStack::new(ctx) };

        if let Some(bottom_process) = bottom_process {
            stack.add(bottom_process);
        }

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
                down: Some(build_stack(ctx, scope, protocol_scope, None, Some(dn_base), rest)),
                up:   Some(build_stack(ctx, scope, protocol_scope, None, Some(up_base), rest)),
                roundtrip_fields: None,
            }
        }

        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::RoundTrip, ref ty, _), rest)) => {
            let shape = resolve_protocol_invoke(ctx, scope, ty);
            let fields_dn = shape.fields(DataMode { down: true, up: false });
            let fields_up = shape.fields(DataMode { down: false, up: true });

            CompiledTest {
                down: Some(build_stack(ctx, scope, protocol_scope, Some((shape.clone(), fields_dn.clone())), None, rest)),
                up:   Some(build_stack(ctx, scope, protocol_scope, Some((shape, fields_up)), None, rest)),
                roundtrip_fields: Some(fields_dn)
            }
        }

        Some((first, rest)) => {
            let bottom = resolve_process(ctx, scope, protocol_scope, &Shape::None, &Fields::null(), first);
            let is_up = bottom.fields_up.direction().up;
            let stack = build_stack(ctx, scope, protocol_scope, None, Some(bottom), rest);

            let (up, down) = if is_up { (Some(stack), None) } else { (None, Some(stack)) };
            CompiledTest { down, up, roundtrip_fields: None }
        }

        None => CompiledTest { down: None, up: None, roundtrip_fields: None }
    }
}
