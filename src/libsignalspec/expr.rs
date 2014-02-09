use context::{
	Context,
	DCell,
	ValueRef,
		Ignored,
		Constant,
		Dynamic,
};

use ast;
use ast::{
	TopType,
	InvalidType,
};
use resolve;
use eval::{
	ValOp,
};

// For now, types have nothing to resolve.
// Eventually, some type parameters will be expressions.
pub type Type = ast::TypeExpr;
fn resolve_type(t: ast::TypeExpr) -> Type { t }

#[deriving(Clone)]
pub enum Item<'s> {
	EventItem(&'s resolve::EventCallable<'s>),
	EntityItem(&'s resolve::Entity<'s>),
	ValueItem(Type, ValueRef /*Down*/, ValueRef /*Up*/)
}

fn common_type(a: Type, b: Type) -> Type{
	match (a, b) {
		(TopType, x) | (x, TopType) => x,
		(a, b) if a == b => a,
		_ => InvalidType
	}
}

#[inline]
fn common_type_all<T:Iterator<Type>>(l: &mut T) -> Type{
	l.fold(TopType, common_type)
}

fn resolve_value_expr<'s>(ctx: &mut Context<'s>, scope: &resolve::Scope<'s>, e: &ast::Expr) ->  (Type, ValueRef /*Down*/, ValueRef /*Up*/) {
	match resolve_expr(ctx, scope, e) {
		ValueItem(t, du, uu) => (t, du, uu),
		_ => fail!("Expected a value expression"),
	}
}

pub fn resolve_expr<'s>(ctx: &mut Context<'s>, scope: &resolve::Scope<'s>, e: &ast::Expr) -> Item<'s> {
	match (*e) {
		ast::IgnoreExpr => ValueItem(TopType, Ignored, Ignored),

		ast::VarExpr(ref name) => {
			scope.get(name.as_slice()).expect("Undefined variable")
		}

		ast::DotExpr(~ref lexpr, ref name) => {
			match resolve_expr(ctx, scope, lexpr) {
				EntityItem(ref e) => e.events.find_equiv(&name.as_slice()).map(|x| EventItem(*x)).expect("Undefined property"),
				_ => fail!("dot only works on entities"),
			}
		}

		ast::ValueExpr(ref val) => ValueItem(val.get_type(), Constant(val.clone()), Constant(val.clone())),

		_ => fail!("unimplemented"),
	}
}
