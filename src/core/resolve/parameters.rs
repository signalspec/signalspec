use itertools::Itertools;

use crate::{DiagnosticContext, Item, LeafItem, Scope, TypeTree, core::{ShapeMsgParam, resolve::{rexpr, type_expr::type_tree}}, diagnostic::{Diagnostic, ErrorReported, Span}, syntax::ast::{self, AstNode}, tree::{Tree, Zip}};

pub trait Parameter {
    fn name(&self) -> &str;
    fn has_default(&self) -> bool;
}

impl Parameter for ast::Parameter {
    fn name(&self) -> &str {
        &self.name.name
    }

    fn has_default(&self) -> bool {
        self.default.is_some()
    }
}

impl Parameter for ShapeMsgParam {
    fn name(&self) -> &str {
        &self.name
    }

    fn has_default(&self) -> bool {
        false
    }
}

fn get_arg<'a>(
    positional_args: &'a [ast::TupleField],
    named_args: &'a [ast::TupleField],
    i: usize,
    name: &'a str,
) -> impl Iterator<Item = &'a ast::Expr> + Clone + use<'a> {
    positional_args.get(i).into_iter()
        .chain(named_args.iter().filter(move |a| a.name.as_ref().is_some_and(|n| n.name == name)))
        .map(|a| &a.expr)
}

pub enum MatchedArg<'a> {
    Ok(&'a ast::Expr),
    Default,
    Missing(ErrorReported),
}

/// Match arguments against a parameter list and report errors for missing / extraneous arguments
pub fn match_args<'a, P: Parameter>(
    dcx: &mut DiagnosticContext,
    caller_scope: &Scope,
    def_params: &'a [P],
    args: &'a ast::ExprTup
) -> (impl Iterator<Item = (&'a P, MatchedArg<'a>)> + use<'a, P>, impl Iterator<Item = &'a ast::Expr> + use<'a, P>) {
    let end_positional = args.fields.iter().position(|f| f.name.is_some()).unwrap_or(args.fields.len());
    let (positional_args, named_args) = args.fields.split_at(end_positional);

    let mut unexpected_args = Vec::new();

    if let Some(unexpected_positional) = positional_args.get(def_params.len()..) && !unexpected_positional.is_empty() {
        let n = unexpected_positional.len();
        let span = unexpected_positional[0].span().to(unexpected_positional.last().unwrap().span());
        unexpected_args.extend(unexpected_positional.iter().map(|a| &a.expr));
        dcx.report(Diagnostic::ArgTooManyPositional { span: Span::new(&caller_scope.file, span), n });
    }

    for arg in named_args {
        if let Some(name) = &arg.name {
            if !def_params.iter().any(|p| p.name() == name.name) {
                unexpected_args.push(&arg.expr);
                dcx.report(Diagnostic::ArgExtraNamed { span: Span::new(&caller_scope.file, arg.span), name: name.name.clone() });
            }
        } else {
            unexpected_args.push(&arg.expr);
            dcx.report(Diagnostic::PositionalArgAfterNamed { span: Span::new(&caller_scope.file, arg.span) });
        }
    }

    for (i, param) in def_params.iter().enumerate() {
        match get_arg(positional_args, named_args, i, param.name()).at_most_one() {
            Ok(None) if !param.has_default() => {
                let span = args.close.as_ref().map(|a| a.span).unwrap_or(args.span);
                dcx.report(Diagnostic::ArgMissing { span: Span::new(&caller_scope.file, span), name: param.name().to_owned() });
            }
            Ok(_) => {}
            Err(dups) => {
                let spans = dups.clone().map(|a| Span::new(&caller_scope.file, a.span())).collect();
                unexpected_args.extend(dups.skip(1));
                dcx.report(Diagnostic::MultipleArgValues { spans, name: param.name().to_owned() });
            }
        }
    }

    let pairs = def_params.iter().enumerate().map(|(i, param)| {
        let arg = match get_arg(positional_args, named_args, i, param.name()).next() {
            Some(arg_ast) => MatchedArg::Ok(arg_ast),
            None if param.has_default() => MatchedArg::Default,
            None => MatchedArg::Missing(ErrorReported::error_reported()),
        };
        (param, arg)
    });

    (pairs, unexpected_args.into_iter())
}

/// Resolve an argument value or default and check its type
pub fn resolve_arg(
    dcx: &mut DiagnosticContext,
    caller_scope: &Scope,
    body_scope: &Scope,
    param: &ast::Parameter,
    arg_ast: MatchedArg,
) -> Item {
    match arg_ast {
        MatchedArg::Ok(arg_ast) => resolve_arg_val(dcx, arg_ast, caller_scope, param, body_scope),
        MatchedArg::Default if let Some(default) = &param.default => resolve_arg_val(dcx, default, body_scope, param, body_scope),
        MatchedArg::Default => unreachable!(),
        MatchedArg::Missing(r) => r.into(),
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
