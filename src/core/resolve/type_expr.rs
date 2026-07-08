use crate::{
    FileSpan, Type, TypeTree, Value, core::{
        Item, LeafItem, Scope, data::{NumberType, NumberTypeError}, resolve::{expr::constant, rexpr}
    }, diagnostic::{Diagnostic, DiagnosticContext, ErrorReported, collect_or_err}, syntax::{Number,
        ast::{self, AstNode}}, tree::{Tree, TupleFields}
};

fn type_from_item(dcx: &mut DiagnosticContext, scope: &Scope, span: FileSpan, item: Item) -> Result<TypeTree, ErrorReported> {
    match item {
        Item::Leaf(LeafItem::Type(ty)) => Ok(ty),
        Item::Leaf(LeafItem::Invalid(e)) => Err(e),
        item => Err(dcx.report(Diagnostic::ExpectedType {
            span: scope.span(span),
            found: item.to_string()
        }))
    }
}

pub fn type_expr(dcx: &mut DiagnosticContext, scope: &Scope, e: &ast::Expr) -> Result<Type, ErrorReported> {
    match type_tree(dcx, scope, e)? {
        TypeTree::Leaf(t) => Ok(t),
        _ => Err(dcx.report(
            Diagnostic::ExpectedSingleType {
                span: scope.span(e.span()),
            }
        )),
    }
}

pub fn type_tree(dcx: &mut DiagnosticContext, scope: &Scope, e: &ast::Expr) -> Result<Tree<Type>, ErrorReported> {
    match e {
        ast::Expr::Var(name) => {
            let Some(item) = scope.get(&name.name) else {
                return Err(dcx.report(Diagnostic::UndefinedVariable {
                    span: scope.span(name.span),
                    name: name.name.clone()
                }));
            };

            type_from_item(dcx, scope, name.span(), item)
        }

        ast::Expr::Call(_) => {
            let item = rexpr(dcx, scope, e);
            type_from_item(dcx, scope, e.span(), item)
        }

        ast::Expr::Tup(node) => {
            if node.fields.len() == 1 && node.fields[0].name.is_none() {
                // Unwrap singleton tuple
                type_tree(dcx, scope, &node.fields[0].expr)
            } else {
                Ok(TypeTree::Tuple(node.fields.iter()
                    .map(|f| Ok::<_, ErrorReported>((
                        f.name.as_ref().map(|n| n.name.clone()),
                        type_tree(dcx, scope, &f.expr)?
                    ))).collect::<Result<TupleFields<TypeTree>, ErrorReported>>()?
                ))
            }
        }
        ast::Expr::Value(node) => Ok(Value::from_literal(&node.value).get_type().into()),
        ast::Expr::Ignore(_) => Ok(Type::Ignored.into()),
        ast::Expr::Range(node) => resolve_type_range(dcx, scope, node).map(TypeTree::Leaf),
        ast::Expr::Union(node) => resolve_type_union(dcx, scope, node).map(TypeTree::Leaf),
        ast::Expr::ArrayRep(node) => resolve_type_array(dcx, scope, node).map(TypeTree::Leaf),

        ast::Expr::Error(e) => Err(ErrorReported::from_ast(e)),

        _ => Err(dcx.report(Diagnostic::NotAllowedInTypePosition {
            span: scope.span(e.span()),
        }))
    }
}

fn resolve_type_range(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprRange) -> Result<Type, ErrorReported> {
    let lo = &node.lo;
    let Some(hi) = node.hi.as_ref() else {
        return Err(dcx.report(Diagnostic::RangeTypeMissingMax {
            span: scope.span(node.span()),
        }));
    };

    let min = constant::<Number>(dcx, scope, lo);
    let max = constant::<Number>(dcx, scope, hi);
    let step = node.step.as_ref().map(|s| constant::<Number>(dcx, scope, s)).transpose();

    let min = min?;
    let max = max?;
    let step = step?.unwrap_or(Number::new(1, 1));

    match NumberType::from_scaled(min, max, step) {
        Ok(t) => Ok(Type::Number(t)),
        Err(NumberTypeError::BoundsNotMultipleOfStep) => Err(dcx.report(
            Diagnostic::RangeNotMultipleOfStep {
                min, min_span: scope.span(lo.span()),
                max, max_span: scope.span(hi.span()),
                step
            }
        )),
        Err(NumberTypeError::Order) => Err(dcx.report(
            Diagnostic::RangeOrder {
                min, min_span: scope.span(lo.span()),
                max, max_span: scope.span(hi.span()),
            }
        )),
        Err(NumberTypeError::StepIsZero) => Err(dcx.report(
            Diagnostic::RangeStepZero {
                step, step_span: scope.span(node.step.as_ref().unwrap().span()),
            }
        )),
    }
}

fn resolve_type_union(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprUnion) -> Result<Type, ErrorReported> {
    let opts: Vec<_> = collect_or_err(node.items.iter().map(|i| type_expr(dcx, scope, i)))?;

    Type::union_iter(opts.into_iter())
        .map_err(|err| err.report_at(dcx, scope.span(node.span)))
}

fn resolve_type_array(dcx: &mut DiagnosticContext, scope: &Scope, node: &ast::ExprArrayRep) -> Result<Type, ErrorReported> {
    let elem = type_expr(dcx, scope, &node.elem);
    let count = constant::<u32>(dcx, scope, &node.count);
    Ok(Type::Vector(count?, Box::new(elem?)))
}
