use typed_arena::Arena;
use std::cell::RefCell;
use ref_slice::ref_slice;

use super::{ ast, grammar, Item};
use super::scope::Scope;
use process::Process;
use session::Session;
use language::protocol::ProtocolScope;
use protocol::{ ProtocolId, Shape };
use util::Index;

pub type PrimitiveFn<'a> = fn(Item<'a>)->Result<Item<'a>, &'static str>;

pub struct ModuleLoader<'a> {
    session: &'a Session,
    ast_arena: Arena<ast::Module>,
    ast_process_arena: Arena<ast::Process>,
    prelude: RefCell<Scope<'a>>,
    pub protocol_scope: RefCell<ProtocolScope<'a>>, // TODO: should be scoped
    pub protocols: Index<ProtocolDef<'a>, ProtocolId>,
}

pub struct Module<'a> {
    ast: &'a ast::Module,
    scope: Scope<'a>,
}

pub struct ProtocolDef<'a> {
    ast: &'a ast::Protocol,
    scope: Scope<'a>,
}

pub struct Test<'a: 'm, 'm> {
    ast: &'a ast::Test,
    module: &'m Module<'a>,
}

impl<'a> ModuleLoader<'a> {
    pub fn new(session: &'a Session) -> ModuleLoader<'a> {
        ModuleLoader {
            session: session,
            ast_arena: Arena::new(),
            ast_process_arena: Arena::new(),
            prelude: RefCell::new(Scope::new()),
            protocol_scope: RefCell::new(ProtocolScope::new()),
            protocols: Index::new(),
        }
    }

    pub fn add_primitive_def(&self, name: &str, prim: &'a ::process::PrimitiveDef) {
    }

    pub fn add_primitive_fn(&self, name: &str, prim: PrimitiveFn<'a>) {
        self.prelude.borrow_mut().bind(name, Item::PrimitiveFn(prim));
    }

    pub fn parse_process(&'a self, source: &str, shape_below: &Shape) -> Result<Box<Process>, grammar::ParseError> {
        let ast = &*self.ast_process_arena.alloc(try!(grammar::process(source)));
        Ok(super::program::resolve_process(&self.session, &*self.prelude.borrow(), &*self.protocol_scope.borrow(), shape_below, &ast))
    }

    pub fn parse_module(&'a self, source: &str) -> Result<Module, grammar::ParseError> {
        let ast = &*self.ast_arena.alloc(try!(grammar::module(source)));

        let mut scope = self.prelude.borrow().child();
        let mut with_blocks = vec![];
        let mut protocols = vec![];

        for entry in &ast.entries {
            match *entry {
                ast::ModuleEntry::Let(ref letdef) => {
                    super::step::resolve_letdef(self.session, &mut scope, ref_slice(letdef));
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
            protocol_scope.add_def(self.session, scope.clone(), def);
        }

        Ok(Module { ast: ast, scope: scope })
    }

    pub fn compile_test<'m>(&'a self, test: &Test<'a, 'm>) -> super::program::CompiledTest<'a> {
        super::program::compile_test(self.session, self, &test.module.scope, &*self.protocol_scope.borrow(), test.ast)
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
