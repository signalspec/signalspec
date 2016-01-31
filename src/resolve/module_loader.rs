use typed_arena::Arena;
use std::cell::RefCell;

use ast;
use resolve::{self, Scope, Item};
use data::Shape;
use grammar;
use session::Session;
use process::Program;
use exec;
use nfa;

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

impl <'s> Module<'s> {
    pub fn compile_call(&self, name: &str,
                        shape_down: Shape,
                        param: Item<'s>) -> Result<Program, ()> {
        if let Some(item) = self.scope.borrow().get(name) {
            let (shape_up, step, _) = resolve::call(&item, self.loader.session, &shape_down, param);

            if let Some(mut f) = self.loader.session.debug_file(|| format!("{}.steps", name)) {
                exec::write_step_tree(&mut f, &step, 0).unwrap_or_else(|e| error!("{}", e));
            }

            let mut nfa = nfa::from_step_tree(&step);

            if let Some(mut f) = self.loader.session.debug_file(|| format!("{}.nfa.dot", name)) {
                nfa.to_graphviz(&mut f).unwrap_or_else(|e| error!("{}", e));
            }

            nfa.remove_useless_epsilons();

            if let Some(mut f) = self.loader.session.debug_file(|| format!("{}.cleaned.nfa.dot", name)) {
                nfa.to_graphviz(&mut f).unwrap_or_else(|e| error!("{}", e));
            }

            Ok(Program{ step: step, shape_down: shape_down, shape_up: shape_up})
        } else {
            Err(())
        }
    }
}
