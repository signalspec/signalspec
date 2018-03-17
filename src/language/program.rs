use super::ast;
use super::scope::Scope;
use protocol::{ Shape, Fields };
use super::protocol::{ ProtocolScope, DefImpl, resolve_protocol_invoke };
use super::step::{ compile_block, make_literal_process };
use super::Ctxt;
use super::primitive::call_primitive;
use process::{ Handle, ProcessInfo };
use data::DataMode;
use connection::Connection;

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

pub struct TestResult {
    pub up: Option<bool>,
    pub down: Option<bool>
}

pub fn run_test<'a>(ctx: &'a Ctxt,
                        scope: &Scope,
                        protocol_scope: &ProtocolScope,
                        test: &ast::Test) -> TestResult {

    fn run_stack<'a>(ctx: &'a Ctxt, mut handle: Handle<'a>, scope: &Scope, protocol_scope: &ProtocolScope, ast: &[ast::Process]) -> bool {
        for process_ast in ast {
            let process = resolve_process(ctx, scope, protocol_scope, handle.top_shape(), handle.top_fields(), process_ast);
            handle = handle.spawn(process);
        }
        handle.join()
    }

    // If the test uses `@both`, generate `@up` and `@dn` versions and run them
    match test.processes.split_first() {
        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::Both, ref shape_expr, ref block), rest)) => {
            let dn_base = make_literal_process(ctx, scope, protocol_scope, false, shape_expr, block);
            let up_base = make_literal_process(ctx, scope, protocol_scope, true, shape_expr, block);

            let dn_handle = Handle::base(ctx).spawn(dn_base);
            let down_res = run_stack(ctx, dn_handle, scope, protocol_scope, rest);

            let up_handle = Handle::base(ctx).spawn(up_base);
            let up_res = run_stack(ctx, up_handle, scope, protocol_scope, rest);

            TestResult {
                down: Some(down_res ^ test.should_fail),
                up:   Some(up_res ^ test.should_fail),
            }
        }

        Some((&ast::Process::Literal(ast::ProcessLiteralDirection::RoundTrip, ref ty, _), rest)) => {
            let shape = resolve_protocol_invoke(ctx, scope, ty);
            let fields_dn = shape.fields(DataMode { down: true, up: false });
            let fields_up = shape.fields(DataMode { down: false, up: true });

            let (c1, c2) = Connection::new(&fields_dn);

            let h1 = Handle::new(ctx, shape.clone(), fields_dn, c1, None);
            let h2 = Handle::new(ctx, shape, fields_up, c2, None);

            TestResult {
                down: Some(run_stack(ctx, h1, scope, protocol_scope, rest) ^ test.should_fail),
                up:   Some(run_stack(ctx, h2, scope, protocol_scope, rest) ^ test.should_fail),
            }
        }

        Some((first, rest)) => {
            let bottom = resolve_process(ctx, scope, protocol_scope, &Shape::None, &Fields::null(), first);
            let is_up = bottom.fields_up.direction().up;

            let handle = Handle::base(ctx).spawn(bottom);
            let r = run_stack(ctx, handle, scope, protocol_scope, rest) ^ test.should_fail;

            let (up, down) = if is_up { (Some(r), None) } else { (None, Some(r)) };
            TestResult { down, up }
        }

        None => panic!("No tests to run")
    }
}
