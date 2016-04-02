use typed_arena::Arena;
use std::cell::RefCell;

use ast;
use resolve::{Scope, Item};
use grammar;
use session::Session;

pub struct ModuleLoader<'a> {
    pub session: &'a Session,
    pub scope_arena: Arena<RefCell<Scope<'a>>>,
    pub ast_arena: Arena<ast::Module>,
    pub prelude: Scope<'a>,
}

impl<'a> ModuleLoader<'a> {
    pub fn new(session: &'a Session) -> ModuleLoader<'a> {
        ModuleLoader {
            session: session,
            scope_arena: Arena::new(),
            ast_arena: Arena::new(),
            prelude: Scope::new(),
        }
    }

    pub fn add_primitive_def(&mut self, name: &str, prim: &'a ::process::PrimitiveDef) {
        self.prelude.bind(name, Item::PrimitiveDef(prim));
    }

    pub fn parse(&'a self, source: &str) -> Result<&'a ast::Module, grammar::ParseError> {
        grammar::module(source).map(|ast| &*self.ast_arena.alloc(ast))
    }

    pub fn parse_module(&'a self, source: &str) -> Result<Module<'a>, grammar::ParseError> {
         self.parse(source).map(|ast| self.resolve_module(ast))
    }

    pub fn resolve_module(&'a self, ast: &'a ast::Module) -> Module<'a> {
        let ref_scope = self.scope_arena.alloc(RefCell::new(self.prelude.clone()));

        {
            let mut scope = ref_scope.borrow_mut();

            for _import in ast.imports.iter() {
                panic!("Imports unimplemented");
            }

            super::block::resolve_letdef(self.session, &mut scope, &ast.lets);

            for def in &ast.defs {
                match *def {
                    ast::ModuleEntry::Signal(ref d) => {
                        let ed = Item::Def(d, ref_scope);
                        scope.names.insert(d.name.clone(), ed);
                    }
                    ast::ModuleEntry::Interface(ref d) => {
                        let ed = Item::Interface(d, ref_scope);
                        scope.names.insert(d.name.clone(), ed);
                    }
                    ast::ModuleEntry::Test(..) => {}
                }
            }
        }

        Module {
            loader: self,
            scope: ref_scope,
        }
    }
}

pub struct Module<'s> {
    pub loader: &'s ModuleLoader<'s>,
    pub scope: &'s RefCell<Scope<'s>>,
}
