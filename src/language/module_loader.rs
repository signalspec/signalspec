use typed_arena::Arena;
use std::cell::RefCell;
use ref_slice::ref_slice;

use super::{ ast, grammar, Item};
use super::scope::Scope;
use data::Shape;
use process::Process;
use session::Session;
use language::protocol::{ resolve_protocol, ProtocolScope };

pub type PrimitiveFn<'a> = fn(Item<'a>)->Result<Item<'a>, &'static str>;

pub struct ModuleLoader<'a> {
    session: &'a Session,
    ast_arena: Arena<ast::Module>,
    ast_process_arena: Arena<ast::Process>,
    prelude: RefCell<Scope<'a>>,
    pub protocol_scope: RefCell<ProtocolScope<'a>>, // TODO: should be scoped
}

pub struct Module<'a> {
    ast: &'a ast::Module,
    scope: &'a RefCell<Scope<'a>>,
}

pub struct Test<'a> {
    ast: &'a ast::Test,
    scope: &'a RefCell<Scope<'a>>,
}

impl<'a> ModuleLoader<'a> {
    pub fn new(session: &'a Session) -> ModuleLoader<'a> {
        ModuleLoader {
            session: session,
            ast_arena: Arena::new(),
            ast_process_arena: Arena::new(),
            prelude: RefCell::new(Scope::new()),
            protocol_scope: RefCell::new(ProtocolScope::new()),
        }
    }

    pub fn add_primitive_def(&self, name: &str, prim: &'a ::process::PrimitiveDef) {
        self.prelude.borrow_mut().bind(name, Item::PrimitiveDef(prim));
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

        let scope = &self.prelude;
        let mut with_blocks = vec![];
        let mut protocols = vec![];

        for entry in &ast.entries {
            match *entry {
                ast::ModuleEntry::Let(ref letdef) => {
                    super::step::resolve_letdef(self.session, &mut *scope.borrow_mut(), ref_slice(letdef));
                }
                ast::ModuleEntry::Use(_) => {
                    panic!("`use` unimplemented");
                }
                ast::ModuleEntry::WithDef(ref with, ref d) => {
                    let ed = Item::Def(d, &scope);
                    scope.borrow_mut().names.insert(d.name.clone(), ed);
                    if let Some(w) = with.as_ref() {
                        with_blocks.push((w, d));
                    }
                }
                ast::ModuleEntry::Protocol(ref d) => {
                    let protocol_id = self.session.protocols.create();
                    scope.borrow_mut().names.insert(d.name.clone(), Item::Protocol(protocol_id, d, &scope));
                    protocols.push((protocol_id, d));
                }
                ast::ModuleEntry::Test(..) => {}
            }
        }

        for (id, protocol_ast) in protocols {
            self.session.protocols.define(id, resolve_protocol(self.session, &*scope.borrow(), protocol_ast));
        }

        let mut protocol_scope = self.protocol_scope.borrow_mut();
        for (with, def) in with_blocks {
            protocol_scope.add_def(self.session, scope, with, def);
        }

        Ok(Module { ast: ast, scope: scope })
    }

    pub fn compile_test(&'a self, test: &Test<'a>) -> super::program::CompiledTest<'a> {

        super::program::compile_test(self.session, self, &*test.scope.borrow(), &*self.protocol_scope.borrow(), test.ast)
    }
}

impl<'a> Module<'a> {
    pub fn tests(&self) -> Vec<Test<'a>> {
        self.ast.entries.iter().filter_map(|entry| {
            match *entry {
                ast::ModuleEntry::Test(ref t) => Some(Test { ast: t, scope: self.scope }),
                _ => None,
            }
        }).collect()
    }
}

impl<'a> Test<'a> {
    pub fn should_fail(&self) -> bool { self.ast.should_fail }
}
