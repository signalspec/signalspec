use ast;
use data::{ DataMode, Shape, NULL_SHAPE };
use session::Session;
use resolve;
use resolve::scope::Scope;
use eval::Expr;

fn entry<'s>(session: &'s Session<'s>, scope: &Scope<'s>, expr: &ast::Expr,
        children: &[ast::InterfaceEntry], dir: DataMode) -> Shape {

    Shape {
        data: resolve::expr::rexpr(session, scope, expr).into_data_shape(dir),
        child: match children.first() {
            Some(&ast::InterfaceEntry::Shape(ref e, ref children)) => {
                Some(box entry(session, scope, e, &children[..], dir))
            }
            None => None
        }
    }
}

pub fn resolve_interface<'s>(session: &'s Session<'s>, ast: &ast::Interface, scope: &Scope<'s>, dir: DataMode) -> Shape {
    match ast.entries.first() {
        Some(&ast::InterfaceEntry::Shape(ref e, ref children)) => {
            entry(session, scope, e, &children[..], dir)
        }
        None => NULL_SHAPE.clone()
    }
}
