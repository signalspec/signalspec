use ast;
use data::{ DataMode, Shape, ShapeVariant };
use session::Session;
use resolve;
use resolve::scope::Scope;

fn collect_variants<'s>(session: &'s Session<'s>, scope: &Scope<'s>, entries: &[ast::InterfaceEntry], dir: DataMode) -> Shape {
    Shape {
        variants: entries.iter().map(|entry| {
            match entry {
                &ast::InterfaceEntry::Shape(ref expr) => {
                    ShapeVariant {
                        data: resolve::expr::rexpr(session, scope, expr).into_data_shape(dir),
                    }
                },
            }
        }).collect()
    }
}

pub fn resolve_interface<'s>(session: &'s Session<'s>, ast: &ast::Interface, scope: &Scope<'s>, dir: DataMode) -> Shape {
    collect_variants(session, scope, &ast.entries[..], dir)
}
