use std::sync::Arc;

use itertools::Itertools;

use crate::{
    Value, core::{
        ChannelId, Dir, Item, LeafItem, Scope, Shape, ShapeMsg, Type, index::FindDefError, resolve::expr::{Pattern, ZipTupleResult}, step::ConnectionId
}, diagnostic::{DiagnosticContext, collect_or_err, try_push}, entitymap::EntityMap, runtime::PrimitiveProcess};
use crate::diagnostic::{ErrorReported};
use crate::runtime::instantiate_primitive;
use crate::syntax::ast::{self, AstNode};
use crate::tree::Tree;
use crate::{Diagnostic, Index};

use super::expr::{TryFromConstant, value, lexpr_tup, zip_tuple_ast_fields, lexpr, Expr, ExprKind, rexpr, rexpr_tup, constant, bind_field, bind_fields, bind_fields_const, VarId};
use super::protocol;

pub fn resolve_process(dcx: &mut DiagnosticContext, index: &Index, scope: &Scope, shape_dn: Shape, ast: &ast::Process) -> ResolveResult {
    let mut builder = Builder::new(dcx, index);
    let conn_dn = builder.add_connection(shape_dn.clone());
    let sb = ResolveCx { scope, up: None, down: Conn { shape: &shape_dn, conn: conn_dn }};
    let (action, up) = builder.resolve_process(sb, ast);
    let connections = builder.connections;
    let vars = builder.vars;
    ResolveResult { action, connections, vars, conn_dn, up,  }
}

pub struct ResolveResult {
    pub action: Action,
    pub vars: EntityMap<VarId, ()>,
    pub connections: EntityMap<ConnectionId, Shape>,
    pub conn_dn: ConnectionId,
    pub up: Option<(Shape, ConnectionId)>,
}

pub enum Action {
    Process { proc: Arc<dyn PrimitiveProcess>, channels: Vec<ChannelId> },
    Stack { lo: Box<Action>, conn: ConnectionId, hi: Box<Action> },
    On {
        conn: ConnectionId,
        tag: usize,
        fields: Vec<(Dir, Pattern)>,
        body: Box<Action>,
        token_has_body: bool,
    },
    Token {
        conn: ConnectionId,
        tag: usize,
        fields: Vec<(Dir, ExprKind)>,
        has_body: bool,
    },
    Seq(Vec<Action>),
    Repeat { dir: RepeatMode, count: ExprKind, body: Box<Action> },
    For { count: u32, vars: Vec<ForVar> , body: Box<Action> },
    Alt { dir: Dir, scrutinee: ExprKind, arms: Vec<AltArm> },
    Any { arms: Vec<Action> },
    Error(ErrorReported),
    Pass,
}

pub struct ForVar {
    pub outer: ExprKind,
    pub var: VarId,
}

pub struct AltArm {
    pub discriminant: Pattern,
    pub body: Action,
}

enum AltMode {
    Const,
    Var(Dir),
}

impl TryFrom<Value> for AltMode {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, ()> {
        match value.as_symbol() {
            Some("const") => Ok(AltMode::Const),
            Some("dn") => Ok(AltMode::Var(Dir::Dn)),
            Some("up") => Ok(AltMode::Var(Dir::Up)),
            _ => Err(()),
        }
    }
}

impl TryFromConstant for AltMode {
    const EXPECTED_MSG: &'static str = "#up | #dn | #const";
}

#[derive(Clone, Copy)]
pub enum RepeatMode {
    Up(bool),
    Dn,
}

impl TryFrom<Value> for RepeatMode {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, ()> {
        match value.as_symbol() {
            Some("dn") => Ok(RepeatMode::Dn),
            Some("up") => Ok(RepeatMode::Up(true)),
            Some("up1") => Ok(RepeatMode::Up(false)),
            _ => Err(()),
        }
    }
}

impl TryFromConstant for RepeatMode {
    const EXPECTED_MSG: &'static str = "#up | #dn | #up1";
}

#[derive(Clone, Copy)]
struct Conn<'a> {
    shape: &'a Shape,
    conn: ConnectionId,
}

#[derive(Clone, Copy)]
struct ResolveCx<'a> {
    scope: &'a Scope,
    down: Conn<'a>,
    up: Option<Conn<'a>>,
}

impl<'a> ResolveCx<'a> {
    fn with_lower<'b>(&'b self, scope: &'b Scope, shape: &'b Shape, conn: ConnectionId) -> ResolveCx<'b> {
        ResolveCx { scope, down: Conn { shape, conn }, ..*self }
    }

    fn with_upper<'b>(&'b self, scope: &'b Scope, shape: &'b Shape, conn: ConnectionId) -> ResolveCx<'b> {
        ResolveCx { scope, up: Some(Conn { shape, conn }), ..*self }
    }

    fn without_upper<'b>(&'b self, scope: &'b Scope) -> ResolveCx<'b> {
        ResolveCx { scope, up: None, ..*self }
    }

    fn with_scope<'b>(&'b self, scope: &'b Scope) -> ResolveCx<'b> {
        ResolveCx { scope, ..*self }
    }
}

macro_rules! try_action {
    ($e:expr) => {
        match $e {
            Ok(e) => e,
            Err(r) => return Action::Error(r)
        }
    }
}

struct Builder<'a> {
    dcx: &'a mut DiagnosticContext,
    index: &'a Index,
    connections: EntityMap<ConnectionId, Shape>,
    vars: EntityMap<VarId, ()>,
}

impl<'a> Builder<'a> {
    fn new(dcx: &'a mut DiagnosticContext, index: &'a Index) -> Self {
        Self {
            dcx,
            index,
            connections: EntityMap::new(),
            vars: EntityMap::new(),
        }
    }

    pub(crate) fn add_connection(&mut self, shape: Shape) -> ConnectionId {
        self.connections.push(shape)
    }

    fn err(&mut self, diag: Diagnostic) -> Action {
        let r = self.dcx.report(diag);
        Action::Error(r)
    }

    fn resolve_action(&mut self, sb: ResolveCx<'_>, action: &ast::Action) -> Action {
        match action {
            ast::Action::Process(node) => {
                let (step, shape_up) = self.resolve_process(sb, &node);
                assert!(shape_up.is_none());
                step
            }

            ast::Action::On(node @ ast::ActionOn { args: Some(args), ..}) => {
                let Some(Conn { shape: shape_up, conn: conn_up }) = sb.up else {
                    return self.err(Diagnostic::OnBlockWithoutUpSignal{
                        span: sb.scope.span(node.span)
                    });
                };

                let Some(msg_def) = shape_up.variant_named(&node.name.name) else {
                    return self.err(Diagnostic::NoVariantNamed {
                        span: sb.scope.span(node.name.span),
                        protocol_name: shape_up.def.ast().name.name.to_owned(),
                        name: node.name.name.to_owned(),
                    });
                };

                let mut body_scope = sb.scope.child();

                let mut fields = Ok(Vec::new());

                for m in zip_tuple_ast_fields(&mut self.dcx, &sb.scope.file, &args, &msg_def.params) {
                    match m {
                        ZipTupleResult::Both(expr, param) => {
                            bind_fields(&mut self.dcx, &mut body_scope, expr, &param.ty, &mut self.vars, &mut |field| {
                                try_push(&mut fields, field.map(|f| (param.direction, f)));
                            });
                        },
                        ZipTupleResult::Left(expr, reported) => {
                            // unexpected param: declare its variables with invalid
                            lexpr(&mut self.dcx, &mut body_scope, expr, &reported.into());
                        },
                        ZipTupleResult::Right(reported, _) => {
                            // missing param: Bind an invalid parameter
                            fields = Err(reported);
                        }
                    }
                }

                let inner_sb = if let &Some(ref child_shape) = &msg_def.child {
                    assert!(fields.as_ref().map_or(true, |f| f.is_empty()));
                    sb.with_upper(&body_scope, child_shape, conn_up)
                } else {
                    sb.without_upper(&body_scope)
                };

                let body = if let &Some(ref body_ast) = &node.block {
                    self.resolve_seq(inner_sb, body_ast)
                } else {
                    Action::Seq(Vec::new())
                };

                Action::On {
                    conn: conn_up,
                    tag: msg_def.tag,
                    fields: try_action!(fields),
                    body: Box::new(body),
                    token_has_body: msg_def.child.is_some()
                }
            }

            ast::Action::On(node @ ast::ActionOn { args: None, ..}) => {
                let Some(Conn { shape: shape_up, conn: conn_up }) = sb.up else {
                    return self.err(Diagnostic::OnBlockWithoutUpSignal{
                        span: sb.scope.span(node.span)
                    });
                };

                let Some(inner_shape_up) = shape_up.child_named(&node.name.name) else {
                    return self.err(Diagnostic::NoChildNamed {
                        span: sb.scope.span(node.name.span),
                        protocol_name: shape_up.def.ast().name.name.to_owned(),
                        name: node.name.name.to_owned(),
                    });
                };

                if let &Some(ref body) = &node.block {
                    self.resolve_seq(sb.with_upper(&sb.scope, inner_shape_up, conn_up), body)
                } else {
                    Action::Seq(Vec::new())
                }
            }

            ast::Action::Repeat(node) => {
                let inner = self.resolve_seq(sb, &node.block);

                let (dir, count) = match &node.dir_count {
                    Some((dir_ast, count_ast)) => {
                        let dir = try_action!(constant::<RepeatMode>(&mut self.dcx, sb.scope, dir_ast));
                        let count = try_action!(value(&mut self.dcx, sb.scope, count_ast));

                        let count_ty = count.get_type();
                        if !count_ty.is_natural_number() {
                            self.dcx.report(Diagnostic::InvalidRepeatCountType {
                                span: sb.scope.span(count_ast.span()),
                                found: count_ty,
                            });
                        }

                        (dir, Ok(count.inner()))
                    }
                    None => (RepeatMode::Up(true), Ok(ExprKind::Ignored))
                };

                Action::Repeat { dir, count: try_action!(count), body: Box::new(inner) }
            }

            ast::Action::For(node) => {
                let mut count_spans = Vec::new();
                let mut body_scope = sb.scope.child();

                let vars = collect_or_err(node.vars.iter().map(|&(ref name, ref expr)| {
                    let outer_expr = value(&mut self.dcx, sb.scope, expr);

                    let ty = match outer_expr.as_ref().map_or(Type::Ignored, |e| e.get_type()) {
                        Type::Vector(c, ty) => {
                            count_spans.push((c, sb.scope.span(expr.span())));
                            *ty
                        }
                        Type::Ignored => Type::Ignored,
                        other => {
                            self.dcx.report(Diagnostic::ExpectedVector {
                                span: sb.scope.span(expr.span()),
                                found: other,
                            });
                            Type::Ignored
                        }
                    };

                    let var = self.vars.push(());
                    body_scope.bind(&name.name, Item::Leaf(LeafItem::Value(Expr::Expr(ty, ExprKind::Var(var)))));
                    Ok(ForVar { outer: outer_expr?.inner(), var })
                }));

                let body = self.resolve_seq(sb.with_scope(&body_scope), &node.block);

                let vars = try_action!(vars);

                let count = match count_spans.iter().map(|(c, _)| *c).all_equal_value() {
                    Ok(c) => c,
                    Err(None) => 0,
                    Err(Some((width1, width2))) => {
                        self.dcx.report(Diagnostic::ForLoopVectorWidthMismatch {
                            span: sb.scope.span(node.span()),
                            width1,
                            width2,
                        });
                        0
                    }
                };

                Action::For { count, vars, body: Box::new(body) }
            }

            ast::Action::Alt(node) => {
                let dir = constant::<AltMode>(&mut self.dcx, sb.scope, &node.dir);

                if node.arms.is_empty() {
                    return self.err(Diagnostic::AltZeroArms {
                        span: sb.scope.span(node.span)
                    });
                }

                match try_action!(dir) {
                    AltMode::Const => {
                        let scrutinee = rexpr(&mut self.dcx, sb.scope, &node.expr);
                        for arm in &node.arms {
                            let mut body_scope = sb.scope.child();
                            if bind_fields_const(&mut self.dcx, &mut body_scope, &arm.discriminant, &scrutinee) {
                                return self.resolve_seq(sb.with_scope(&body_scope), &arm.block);
                            }
                        }
                        self.err(Diagnostic::AltNoArmsMatched {
                            span: sb.scope.span(node.span)
                        })
                    }
                    AltMode::Var(dir) => {
                        let scrutinee = value(&mut self.dcx, sb.scope, &node.expr);
                        let ty = scrutinee.as_ref().map_or(Type::Ignored, |t| t.get_type());

                        let arms = try_action!(collect_or_err(node.arms.iter().map(|arm| {
                            let mut body_scope = sb.scope.child();
                            let discriminant = bind_field(&mut self.dcx, &mut body_scope, &arm.discriminant, &ty, &mut self.vars);
                            let body = self.resolve_seq(sb.with_scope(&body_scope), &arm.block);
                            Ok(AltArm { discriminant: discriminant?, body })
                        })));

                        let scrutinee = try_action!(scrutinee).inner();
                        Action::Alt { dir, scrutinee, arms }
                    }
                }
            }
            ast::Action::Any(node) => {
                let arms = node.arms.iter().map(|arm| {
                    self.resolve_action(sb, arm)
                }).collect();

                Action::Any { arms }
            }
            ast::Action::Error(r) => Action::Error(ErrorReported::from_ast(r)),
        }
    }

    fn resolve_process(&mut self, sb: ResolveCx<'_>, process_ast: &ast::Process) -> (Action, Option<(Shape, ConnectionId)>) {
        match process_ast {
            ast::Process::Call(node) => {
                if let Some(msg_def) = sb.down.shape.variant_named(&node.name.name) {
                    let action = match self.resolve_token(msg_def, sb.scope, &node) {
                        Ok(fields) => Action::Token {
                            conn: sb.down.conn,
                            tag: msg_def.tag,
                            fields,
                            has_body: msg_def.child.is_some(),
                        },
                        Err(r) => Action::Error(r),
                    };

                    let top = msg_def.child.as_ref().map(|child_shape| (child_shape.clone(), sb.down.conn));

                    (action, top)
                } else {
                    let def = match self.index.find_def(sb.down.shape, &node.name.name) {
                        Ok(res) => res,
                        Err(FindDefError::NoDefinitionWithName) => {
                            let step = self.err(Diagnostic::NoDefNamed {
                                span: sb.scope.span(node.name.span),
                                protocol_name: sb.down.shape.def.ast().name.name.to_owned(),
                                def_name: node.name.name.to_owned(),
                            });
                            return (step, None)
                        }
                    };

                    let args = rexpr_tup(&mut self.dcx, sb.scope, &node.args);

                    let mut scope = def.file.scope();
                    lexpr(&mut self.dcx, &mut scope, &def.protocol.param, &sb.down.shape.param);
                    lexpr_tup(&mut self.dcx, &mut scope, &def.params, &args);

                    self.resolve_process(sb.with_scope(&scope), &def.implementation)
                }
            }

            ast::Process::Child(node) => {
                let Some(child_shape) = sb.down.shape.child_named(&node.name.name) else {
                    let step = self.err(Diagnostic::NoDefNamed {
                        span: sb.scope.span(node.name.span),
                        protocol_name: sb.down.shape.def.ast().name.name.to_owned(),
                        def_name: node.name.name.to_owned(),
                    });
                    return (step, None)
                };

                (Action::Pass, Some((child_shape.clone(), sb.down.conn)))
            }

            ast::Process::Primitive(node) => {
                let arg = rexpr_tup(&mut self.dcx, sb.scope, &node.args);
                debug!("instantiating primitive {} with {}", node.name.name, arg);
                let proc = match instantiate_primitive(&node.name, arg, sb.down.shape, sb.up.as_ref().map(|u| u.shape)) {
                    Ok(p) => p,
                    Err(msg) => {
                        let step = self.err(Diagnostic::ErrorInPrimitiveProcess {
                            span: sb.scope.span(node.span),
                            msg
                        });
                        return (step, None);

                    }
                };

                let channels = [
                    sb.down.shape.mode.has_dn_channel().then_some(sb.down.conn.dn()),
                    sb.down.shape.mode.has_up_channel().then_some(sb.down.conn.up()),
                    sb.up.and_then(|u| u.shape.mode.has_up_channel().then_some(u.conn.up())),
                    sb.up.and_then(|u| u.shape.mode.has_dn_channel().then_some(u.conn.dn())),
                ].into_iter().flatten().collect();

                (Action::Process { proc, channels }, None)
            }

            ast::Process::New(node) => {
                let top_shape = match protocol::resolve(&mut self.dcx, self.index, sb.scope, &node.top, 1) {
                    Ok(shape) => shape,
                    Err(r) => return (Action::Error(r), None),
                };
                let conn = self.add_connection(top_shape.clone());
                let block = self.resolve_seq(sb.with_upper(sb.scope, &top_shape, conn), &node.block);
                (block, Some((top_shape, conn)))
            }

            ast::Process::Seq(block) => {
                let block = self.resolve_seq(sb, block);
                (block, None)
            }

            ast::Process::Stack(node) => {
                let (lo, up) = self.resolve_process(sb, &node.lower);

                let Some((shape, conn)) = up else {
                    if matches!(lo, Action::Error(_)) {
                        return (lo, None);
                    } else {
                        let step = self.err(Diagnostic::StackWithoutBaseSignal {
                            span: sb.scope.span(node.lower.span()),
                        });
                        return (step, None)
                    }
                };

                let (hi, shape_up) = self.resolve_process(sb.with_lower(sb.scope, &shape, conn), &node.upper);
                (Action::Stack { lo: Box::new(lo), conn, hi: Box::new(hi) }, shape_up)
            }
        }
    }

    fn resolve_token(&mut self, msg_def: &ShapeMsg, scope: &Scope, node: &ast::ProcessCall) -> Result<Vec<(Dir, ExprKind)>, ErrorReported> {
        let mut fields = Ok(Vec::new());

        for m in zip_tuple_ast_fields(&mut self.dcx, &scope.file, &node.args, &msg_def.params) {
            match m {
                ZipTupleResult::Both(arg_ast, param) => {
                    let arg = rexpr(&mut self.dcx, scope, arg_ast);

                    let mut valid = true;
                    param.ty.zip(&arg, &mut |m| { match m {
                        crate::tree::Zip::Both(
                            &Tree::Leaf(ref ty),
                            &Item::Leaf(LeafItem::Value(ref v))
                        ) if v.get_type().is_subtype(ty) => {
                            try_push(&mut fields, Ok((param.direction, v.clone().inner())));
                        },

                        crate::tree::Zip::Both(_, Item::Leaf(LeafItem::Invalid(r))) => {
                            fields = Err(r.clone());
                        }

                        _ => {
                            valid = false;
                        }
                    }});

                    if !valid {
                        fields = Err(self.dcx.report(Diagnostic::ArgMismatchType {
                            span: scope.span(arg_ast.span()),
                            def_name: node.name.name.clone(),
                            expected: format!("{}", param.ty),
                            found: format!("{arg}")
                        }));
                    }
                }
                ZipTupleResult::Left(_, _) => {
                    // unexpected parameter passed and already reported: ignore it
                }
                ZipTupleResult::Right(reported, _) => {
                    // missing expected parameter
                    fields = Err(reported);
                },
            };
        }

        assert!(fields.as_ref().map_or(true, |f| f.is_empty()) || msg_def.child.is_none());

        fields
    }

    fn resolve_seq(&mut self, sb: ResolveCx<'_>, block: &ast::Block) -> Action {
        let mut scope = sb.scope.child();

        for ld in &block.lets {
            resolve_letdef(&mut self.dcx, &mut scope, &ld);
        }

        let steps: Vec<_> = block.actions.iter().map(|action| {
            self.resolve_action(sb.with_scope(&scope), &action)
        }).collect();

        Action::Seq(steps)
    }
}

pub fn resolve_letdef(dcx: &mut DiagnosticContext, scope: &mut Scope, ld: &ast::LetDef) {
    let &ast::LetDef { ref name, ref expr, .. } = ld;
    let item = rexpr(dcx, scope, expr);
    scope.bind(&name.name, item);
}
