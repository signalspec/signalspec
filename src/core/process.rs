use std::sync::atomic::{AtomicUsize, Ordering};
use std::path::PathBuf;
use std::fs;

use crate::syntax::{ ast, SourceFile, parse_process_chain, ParseError };
use super::{ Index, Shape, Fields, Scope, StepInfo, Message, DefImpl };
use super::{rexpr, resolve_token, resolve_protocol_invoke, call_primitive, compile_block };
use crate::runtime::PrimitiveProcess;

#[derive(Debug)]
pub enum Process {
    Token(Message),
    Seq(StepInfo),
    Primitive(Box<dyn PrimitiveProcess + 'static>),
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
                       shape_down: &Shape,
                       fields_down: &Fields,
                       processes_ast: &[ast::Process]) -> ProcessChain {

    let mut processes: Vec<ProcessInfo> = vec![];

    for process_ast in processes_ast {
        let (shape_down, fields_down) = processes.last().map_or((shape_down, fields_down), |p|(&p.shape_up, &p.fields_up));

        match *process_ast {
            ast::Process::Call(ref name, ref args_ast) => {
                let args = args_ast.iter().map(|a| rexpr(scope, a)).collect();

                if shape_down.has_variant_named(name) {
                    let token_proc = resolve_token(shape_down, name, args);
                    processes.push(ProcessInfo {
                        process: Process::Token(token_proc),
                        shape_up: Shape::None,
                        fields_up: Fields::null()
                    })
                } else {
                    let (scope, imp) = ctx.index.find_def(shape_down, name, args);
                    match *imp {
                        DefImpl::Code(ref callee_ast) => {
                            let chain = resolve_process(ctx, &scope, shape_down, fields_down, callee_ast);
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
                let block = compile_block(ctx, scope, shape_down, fields_down, top_shape, block, "anon_block");
                processes.push(block);
            }

            ast::Process::InferSeq(ref block) => {
                let block = compile_block(ctx, scope, shape_down, fields_down, Shape::None, block, "anon_block");
                processes.push(block);
            }
        };
    }

    ProcessChain { processes }
}


#[derive(Clone, Default, Debug)]
pub struct Config {
    pub debug_dir: Option<PathBuf>
}

pub struct Ctxt<'a> {
    pub id_counter: AtomicUsize,
    pub debug_dir: Option<PathBuf>,
    pub index: &'a Index,
}

impl Ctxt<'_> {
    pub fn new<'a>(config: Config, index: &'a Index) -> Ctxt<'a> {
        if let Some(ref p) = config.debug_dir {
            fs::create_dir_all(p)
                .unwrap_or_else(|e| error!("Failed to create debug directory `{}`: {}", p.display(), e));
        }

        Ctxt {
            id_counter: AtomicUsize::new(1),
            debug_dir: config.debug_dir,
            index: index,
        }
    }

    pub fn make_id(&self) -> usize {
        self.id_counter.fetch_add(1, Ordering::Relaxed)
    }

    pub fn debug_file<T: FnOnce() -> String>(&self, name: T) -> Option<fs::File> {
        self.debug_dir.as_ref().and_then(|path| {
            let mut p = path.to_owned();
            p.push(name());
            fs::File::create(&p)
                .map_err(|e| error!("Failed to open debug file `{}`: {}", p.display(), e))
                .ok()
        })
    }

    pub fn parse_process(&self, source: &str, shape_below: &Shape, fields_below: &Fields) -> Result<ProcessChain, ParseError> {
        let file = SourceFile { name: "<process>".into(), source: source.into() };
        let ast = parse_process_chain(&file.source)?;
        Ok(resolve_process(self, &self.index.prelude, shape_below, fields_below, &ast))
    }
}

