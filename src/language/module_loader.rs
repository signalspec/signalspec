use typed_arena::Arena;
use std::cell::RefCell;
use ref_slice::ref_slice;

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
    ast_arena: Arena<ast::Module>,
    ast_process_arena: Arena<ast::Process>,
    ast_header_arena: Arena<ast::PrimitiveHeader>,
    pub prelude: RefCell<Scope>,
    pub protocol_scope: RefCell<ProtocolScope<'a>>, // TODO: should be scoped
    pub protocols: Index<ProtocolDef<'a>, ProtocolId>,
    pub functions: Index<FunctionDef<'a>, FunctionId>,
}

pub struct Module<'a> {
    ast: &'a ast::Module,
    scope: Scope,
}

pub struct ProtocolDef<'a> {
    pub ast: &'a ast::Protocol,
    pub scope: Scope,
}

pub struct Test<'a: 'm, 'm> {
    ast: &'a ast::Test,
    module: &'m Module<'a>,
}

impl<'a> Ctxt<'a> {
    pub fn new(session: &'a Session) -> Ctxt<'a> {
        Ctxt {
            session: session,
            ast_arena: Arena::new(),
            ast_process_arena: Arena::new(),
            ast_header_arena: Arena::new(),
            prelude: RefCell::new(Scope::new()),
            protocol_scope: RefCell::new(ProtocolScope::new()),
            protocols: Index::new(),
            functions: Index::new(),
        }
    }

    pub fn add_primitive_fn(&'a self, name: &str, prim: PrimitiveFn<'a>) {
        let fnid = self.create_function(FunctionDef::Primitive(prim));
        self.prelude.borrow_mut().bind(name, Item::Func(fnid));
    }

    pub fn define_primitive(&'a self, header: &str, implementations: Vec<PrimitiveDef>) {
        let header = &*self.ast_header_arena.alloc(grammar::primitive_header(header).expect("failed to parse primitive header"));
        self.protocol_scope.borrow_mut().add_primitive(self, &*self.prelude.borrow(), header, implementations);
    }

    pub fn define_prelude(&'a self, source: &str) {
        let module = self.parse_module(source).expect("failed to parse prelude module");
        *self.prelude.borrow_mut() = module.scope;
    }

    pub fn create_function(&'a self, def: FunctionDef<'a>) -> FunctionId {
        let fnid = self.functions.create();
        self.functions.define(fnid, def);
        fnid
    }

    pub fn look_up_function(&self, id: FunctionId) -> &FunctionDef<'a> {
        self.functions.get(id)
    }

    pub fn parse_process(&'a self, source: &str, shape_below: &Shape, fields_below: &Fields) -> Result<ProcessInfo, grammar::ParseError> {
        let ast = &*self.ast_process_arena.alloc(try!(grammar::process(source)));
        Ok(super::program::resolve_process(self, &*self.prelude.borrow(), &*self.protocol_scope.borrow(), shape_below, fields_below, &ast))
    }

    pub fn parse_module(&'a self, source: &str) -> Result<Module, grammar::ParseError> {
        let ast = &*self.ast_arena.alloc(try!(grammar::module(source)));

        let mut scope = self.prelude.borrow().child();
        let mut with_blocks = vec![];
        let mut protocols = vec![];

        for entry in &ast.entries {
            match *entry {
                ast::ModuleEntry::Let(ref letdef) => {
                    super::step::resolve_letdef(self, &mut scope, ref_slice(letdef));
                }
                ast::ModuleEntry::Use(_) => {
                    panic!("`use` unimplemented");
                }
                ast::ModuleEntry::WithDef(ref def) => {
                    with_blocks.push(def);
                }
                ast::ModuleEntry::Protocol(ref d) => {
                    let protocol_id = self.protocols.create();
                    scope.names.insert(d.name.clone(), Item::Protocol(protocol_id));
                    protocols.push((protocol_id, d));
                }
                ast::ModuleEntry::Test(..) => {}
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

        Ok(Module { ast: ast, scope: scope })
    }

    pub fn compile_test<'m>(&'a self, test: &Test<'a, 'm>) -> super::program::CompiledTest<'a> {
        super::program::compile_test(self, &test.module.scope, &*self.protocol_scope.borrow(), test.ast)
    }
}

impl<'a> Module<'a> {
    pub fn tests<'m>(&'m self) -> Vec<Test<'a, 'm>> {
        self.ast.entries.iter().filter_map(|entry| {
            match *entry {
                ast::ModuleEntry::Test(ref t) => Some(Test { ast: t, module: &self }),
                _ => None,
            }
        }).collect()
    }
}

impl<'a, 'm> Test<'a, 'm> {
    pub fn should_fail(&self) -> bool { self.ast.should_fail }
}
