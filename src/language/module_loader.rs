use std::cell::RefCell;

use super::{ ast, grammar, Item };
use super::scope::Scope;
use super::function::{ FunctionId, FunctionDef, PrimitiveFn };
use process::{ ProcessInfo, Process };
use session::Session;
use language::protocol::ProtocolScope;
use protocol::{ ProtocolId, Shape, Fields };
use util::Index;
use data::DataMode;

pub enum PrimitiveDefFields {
    Explicit(Fields),
    Auto(DataMode)
}

pub struct PrimitiveDef {
    pub id: &'static str,
    pub fields_down: Fields,
    pub fields_up: PrimitiveDefFields,
    pub instantiate: Box<Fn(&Scope) -> Result<Box<Process>, ()>>,
}

pub struct Ctxt<'a> {
    pub session: &'a Session,
    pub prelude: RefCell<Scope>,
    pub protocol_scope: RefCell<ProtocolScope>, // TODO: should be scoped
    pub protocols: Index<ProtocolDef, ProtocolId>,
    pub functions: Index<FunctionDef, FunctionId>,
}

pub struct Module {
    scope: Scope,
    tests: Vec<ast::Test>,
}

pub struct ProtocolDef {
    pub ast: ast::Protocol,
    pub scope: Scope,
}

impl<'a> Ctxt<'a> {
    pub fn new(session: &'a Session) -> Ctxt<'a> {
        Ctxt {
            session: session,
            prelude: RefCell::new(Scope::new()),
            protocol_scope: RefCell::new(ProtocolScope::new()),
            protocols: Index::new(),
            functions: Index::new(),
        }
    }

    pub fn add_primitive_fn(&self, name: &str, prim: PrimitiveFn) {
        let fnid = self.create_function(FunctionDef::Primitive(prim));
        self.prelude.borrow_mut().bind(name, Item::Func(fnid));
    }

    pub fn define_primitive(&self, header_src: &str, implementations: Vec<PrimitiveDef>) {
        let header = grammar::primitive_header(header_src).expect("failed to parse primitive header");
        self.protocol_scope.borrow_mut().add_primitive(self, &*self.prelude.borrow(), header, implementations);
    }

    pub fn define_prelude(&self, source: &str) {
        let module = self.parse_module(source).expect("failed to parse prelude module");
        *self.prelude.borrow_mut() = module.scope;
    }

    pub fn create_function(&self, def: FunctionDef) -> FunctionId {
        let fnid = self.functions.create();
        self.functions.define(fnid, def);
        fnid
    }

    pub fn look_up_function(&self, id: FunctionId) -> &FunctionDef {
        self.functions.get(id)
    }

    pub fn parse_process(&self, source: &str, shape_below: &Shape, fields_below: &Fields) -> Result<ProcessInfo, grammar::ParseError> {
        let ast = try!(grammar::process(source));
        Ok(super::program::resolve_process(self, &*self.prelude.borrow(), &*self.protocol_scope.borrow(), shape_below, fields_below, &ast))
    }

    pub fn parse_module(&self, source: &str) -> Result<Module, grammar::ParseError> {
        let ast = grammar::module(source)?;

        let mut scope = self.prelude.borrow().child();
        let mut with_blocks = vec![];
        let mut protocols = vec![];
        let mut tests = vec![];

        for entry in ast.entries {
            match entry {
                ast::ModuleEntry::Let(letdef) => {
                    super::step::resolve_letdef(self, &mut scope, &letdef);
                }
                ast::ModuleEntry::Use(_) => {
                    panic!("`use` unimplemented");
                }
                ast::ModuleEntry::WithDef(def) => {
                    with_blocks.push(def);
                }
                ast::ModuleEntry::Protocol(d) => {
                    let protocol_id = self.protocols.create();
                    scope.names.insert(d.name.clone(), Item::Protocol(protocol_id));
                    protocols.push((protocol_id, d));
                }
                ast::ModuleEntry::Test(t) => {
                    tests.push(t)
                }
            }
        }

        let scope = scope; // No longer mutable

        for (id, protocol_ast) in protocols {
            self.protocols.define(id, ProtocolDef{ ast: protocol_ast, scope: scope.clone() });
        }

        let mut protocol_scope = self.protocol_scope.borrow_mut();
        for def in with_blocks {
            protocol_scope.add_def(self, scope.clone(), def);
        }

        Ok(Module { scope: scope, tests: tests  })
    }
}

impl Module {
    pub fn tests<'m>(&'m self) -> Vec<Test<'m>> {
        self.tests.iter().map(|test| {
            Test { ast: test, scope: &self.scope }
        }).collect()
    }
}

pub struct Test<'m> {
    scope: &'m Scope,
    ast: &'m ast::Test,
}

impl<'m> Test<'m> {
    pub fn compile<'s>(&self, ctx: &'s Ctxt<'s>) -> super::program::CompiledTest<'s> {
        super::program::compile_test(ctx, &self.scope, &*ctx.protocol_scope.borrow(), &self.ast)
    }

    pub fn should_fail(&self) -> bool { self.ast.should_fail }
}
