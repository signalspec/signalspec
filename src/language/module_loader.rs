use typed_arena::Arena;
use std::cell::RefCell;

use super::{ ast, grammar, Item};
use super::scope::Scope;
use data::Shape;
use process::Process;
use session::Session;

pub struct ModuleLoader<'a> {
    session: &'a Session,
    ast_arena: Arena<ast::Module>,
    prelude: RefCell<Scope<'a>>,
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
            prelude: RefCell::new(Scope::new()),
        }
    }

    pub fn add_primitive_def(&self, name: &str, prim: &'a ::process::PrimitiveDef) {
        self.prelude.borrow_mut().bind(name, Item::PrimitiveDef(prim));
    }

    pub fn parse_process(&'a self, source: &str, shape_below: &Shape) -> Result<Box<Process>, grammar::ParseError> {
        let ast = try!(grammar::process(source));
        Ok(super::program::resolve_process(&self.session, &*self.prelude.borrow(), shape_below, &ast))
    }

    pub fn parse_module(&'a self, source: &str) -> Result<Module, grammar::ParseError> {
        let ast = &*self.ast_arena.alloc(try!(grammar::module(source)));

        for _import in ast.imports.iter() {
            panic!("Imports unimplemented");
        }

        let scope = &self.prelude;

        super::step::resolve_letdef(self.session, &mut *scope.borrow_mut(), &ast.lets);

        for def in &ast.defs {
            match *def {
                ast::ModuleEntry::Signal(ref d) => {
                    let ed = Item::Def(d, &scope);
                    scope.borrow_mut().names.insert(d.name.clone(), ed);
                }
                ast::ModuleEntry::Interface(ref d) => {
                    let ed = Item::Interface(d, &scope);
                    scope.borrow_mut().names.insert(d.name.clone(), ed);
                }
                ast::ModuleEntry::Test(..) => {}
            }
        }

        Ok(Module { ast: ast, scope: scope })
    }

    pub fn compile_test(&'a self, test: &Test<'a>) -> super::program::CompiledTest<'a> {
        super::program::compile_test(self.session, self, &*test.scope.borrow(), test.ast)
    }
}

impl<'a> Module<'a> {
    pub fn tests(&self) -> Vec<Test<'a>> {
        self.ast.defs.iter().filter_map(|entry| {
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
