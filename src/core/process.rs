use syntax::ast;
use super::{ Shape, Fields, Ctxt, Scope, ProtocolScope, StepInfo, Message, DefImpl };
use super::{rexpr, resolve_token, resolve_protocol_invoke, call_primitive, compile_block, make_literal_process};
use runtime::PrimitiveProcess;

#[derive(Debug)]
pub enum Process {
    Token(Message),
    Seq(StepInfo),
    Primitive(Box<PrimitiveProcess + 'static>),
}

#[derive(Debug)]
pub struct ProcessInfo {
    pub process: Process,
    pub shape_up: Shape,
    pub fields_up: Fields,
}

#[derive(Debug)]
pub struct ProcessChain {
    pub processes: Vec<ProcessInfo>
}

impl ProcessChain {
    pub fn fields_up(&self) -> &Fields {
        &self.processes.last().unwrap().fields_up
    }

    pub fn shape_up(&self) -> &Shape {
        &self.processes.last().unwrap().shape_up
    }
}

pub fn resolve_process(ctx: &Ctxt,
                       scope: &Scope,
                       protocol_scope: &ProtocolScope,
                       shape_down: &Shape,
                       fields_down: &Fields,
                       processes_ast: &[ast::Process]) -> ProcessChain {

    let mut processes: Vec<ProcessInfo> = vec![];

    for process_ast in processes_ast {
        let (shape_down, fields_down) = processes.last().map_or((shape_down, fields_down), |p|(&p.shape_up, &p.fields_up));

        match *process_ast {
            ast::Process::Call(ref name, ref param_ast) => {
                let param = rexpr(ctx, scope, param_ast);

                if shape_down.has_variant_named(name) {
                    processes.push(ProcessInfo {
                        process: Process::Token(resolve_token(shape_down, name, param)),
                        shape_up: Shape::None,
                        fields_up: Fields::null()
                    })
                } else {
                    let (scope, imp) = protocol_scope.find(ctx, shape_down, name, param);
                    match *imp {
                        DefImpl::Code(ref callee_ast) => {
                            let chain = resolve_process(ctx, &scope, protocol_scope, shape_down, fields_down, callee_ast);
                            processes.extend(chain.processes);
                        }
                        DefImpl::Primitive(ref primitive, ref shape_up_ast) => {
                            let shape_up = if let Some(ref x) = shape_up_ast {
                                resolve_protocol_invoke(ctx, &scope, x)
                            } else {
                                Shape::None
                            };
                            let (prim, fields_up) = call_primitive(ctx, &scope, fields_down, &shape_up, primitive, &name);
                            processes.push(ProcessInfo { process: Process::Primitive(prim), fields_up, shape_up });
                        }
                    }
                }
            }

            ast::Process::Seq(ref top_shape, ref block) => {
                let top_shape = resolve_protocol_invoke(ctx, &scope, top_shape);
                processes.push(compile_block(ctx, scope, protocol_scope, shape_down, fields_down, top_shape, block, "anon_block"));
            }

            ast::Process::InferSeq(ref block) => {
                processes.push(compile_block(ctx, scope, protocol_scope, shape_down, fields_down, Shape::None, block, "anon_block"));
            }

            ast::Process::Literal(dir, ref shape_up_expr, ref block) => {
                let is_up = match dir {
                    ast::ProcessLiteralDirection::Up => true,
                    ast::ProcessLiteralDirection::Down => false,
                    ast::ProcessLiteralDirection::Both => panic!("@both is only usable in tests"),
                    ast::ProcessLiteralDirection::RoundTrip => panic!("@roundtrip is only usable in tests"),
                };

                processes.extend(make_literal_process(ctx, scope, protocol_scope, is_up, shape_up_expr, block).processes);
            }
        };
    }

    ProcessChain { processes }
}
