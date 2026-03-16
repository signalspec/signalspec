use itertools::Itertools;

use crate::{DiagnosticContext, Item, LeafItem, Scope, TypeTree, core::resolve::{rexpr, type_expr::type_tree}, diagnostic::{Diagnostic, ErrorReported, Span}, syntax::ast::{self, AstNode, Identifier}, tree::{Tree, Zip}};

fn get_arg<'a>(
    positional_args: &'a [ast::TupleField],
    named_args: &'a [ast::TupleField],
    i: usize,
    name: &'a Identifier,
) -> impl Iterator<Item = &'a ast::Expr> + use<'a> {
    positional_args.get(i).into_iter()
        .chain(named_args.iter().filter(|a| a.name.as_ref().is_some_and(|n| n.name == name.name)))
        .map(|a| &a.expr)
}

/// Match arguments against a parameter list and report errors for missing / extraneous arguments
pub fn match_args<'a>(
    dcx: &mut DiagnosticContext,
    caller_scope: &Scope,
    def_params: &'a ast::ParameterList,
    args: &'a ast::ExprTup
) -> impl Iterator<Item = (&'a ast::Parameter, Option<&'a ast::Expr>)> + use<'a> {
    let end_positional = args.fields.iter().position(|f| f.name.is_some()).unwrap_or(args.fields.len());
    let (positional_args, named_args) = args.fields.split_at(end_positional);

    if positional_args.len() > def_params.list.len() {
        let n = positional_args.len() - def_params.list.len();
        let span = positional_args[def_params.list.len()].span()
            .to(positional_args.last().unwrap().span());
        dcx.report(Diagnostic::ArgTooManyPositional { span: Span::new(&caller_scope.file, span), n });
    }

    for arg in named_args {
        if let Some(name) = &arg.name {
            if !def_params.list.iter().any(|p| p.name.name == name.name) {
                dcx.report(Diagnostic::ArgExtraNamed { span: Span::new(&caller_scope.file, arg.span), name: name.name.clone() });
            }
        } else {
            dcx.report(Diagnostic::PositionalArgAfterNamed { span: Span::new(&caller_scope.file, arg.span) });
        }
    }

    for (i, param) in def_params.list.iter().enumerate() {
        match get_arg(positional_args, named_args, i, &param.name).at_most_one() {
            Ok(None) if param.default.is_none() => {
                let span = args.close.as_ref().map(|a| a.span).unwrap_or(args.span);
                dcx.report(Diagnostic::ArgMissing { span: Span::new(&caller_scope.file, span), name: param.name.name.clone() });
            }
            Ok(_) => {}
            Err(dups) => {
                let spans = dups.map(|a| Span::new(&caller_scope.file, a.span())).collect();
                dcx.report(Diagnostic::MultipleArgValues { spans, name: param.name.name.clone() });
            }
        }
    }

    def_params.list.iter().enumerate().map(|(i, param)| {
        let arg = get_arg(positional_args, named_args, i, &param.name).next();
        (param, arg)
    })
}

/// Resolve an argument value or default and check its type
pub fn resolve_arg(
    dcx: &mut DiagnosticContext,
    caller_scope: &Scope,
    body_scope: &Scope,
    param: &ast::Parameter,
    arg_ast: Option<&ast::Expr>,
) -> Item {
    if let Some(arg_ast) = arg_ast {
        resolve_arg_val(dcx, &arg_ast, caller_scope, param, body_scope)
    } else if let Some(default) = &param.default {
        resolve_arg_val(dcx, default, body_scope, param, body_scope)
    } else {
        // missing arg reported in `match_args`
        ErrorReported::error_reported().into()
    }
}

fn resolve_arg_val(
    dcx: &mut DiagnosticContext,
    arg_ast: &ast::Expr,
    arg_scope: &Scope,
    param: &ast::Parameter,
    body_scope: &Scope,
) -> Item {
    let arg = rexpr(dcx, arg_scope, arg_ast);
    if let Some(ty_ast) = &param.ty {
        match type_tree(dcx, body_scope, ty_ast) {
            Ok(ty) => check_arg_type(dcx, arg_scope.span(arg_ast.span()), arg, &ty),
            Err(e) => e.into(),
        }
    } else {
        arg
    }
}

pub fn check_arg_type(dcx: &mut DiagnosticContext, span: Span, arg: Item, ty: &TypeTree) -> Item {
    let mut valid = true;
    arg.zip(ty, &mut |z| match z {
        Zip::Both(Tree::Leaf(LeafItem::Value(e)), Tree::Leaf(t)) if e.check_type(t) => {}
        Zip::Both(Tree::Leaf(LeafItem::Invalid(_)), _) => {}
        _ => valid = false,
    });

    if valid {
        arg
    } else {
        let reported = dcx.report(Diagnostic::ArgMismatchType {
            span,
            expected: format!("{ty}"),
            found: format!("{arg}"),
        });
        ty.map_leaf(&mut |_| LeafItem::Invalid(reported.clone()))
    }
}
