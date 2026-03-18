use num_traits::Signed;
use itertools::Itertools;

use crate::{
    Diagnostic, DiagnosticContext, Shape, Value, core::{
        Dir, op::{ConcatElem, UnaryOp}, resolve::{action::{Action, RepeatMode}, expr::{ExprKind, Pattern, VarId}}, step::{ConnectionId, ExprDn, ExprDnId, Predicate, StepBuilder, StepId, ValueSrc, ValueSrcId, expr_lower::ExprLower}
    }, entitymap::EntityMap, syntax::BinOp
};

pub fn build_step_tree(
    dcx: &mut DiagnosticContext,
    vars: &EntityMap<VarId, ()>,
    connections: &EntityMap<ConnectionId, Shape>,
    action: &Action,
) -> (StepBuilder, StepId) {
    let mut builder = Builder {
        dcx,
        expr_lower: ExprLower::new(&vars),
        steps: StepBuilder::new(),
        connections,
    };

    let root = builder.build(action);

    (builder.steps, root)
}

fn fields_of_dir<'a, T: 'a>(shape: &Shape, tag: usize, fields: impl IntoIterator<Item = &'a T>, dir: Dir) -> impl Iterator<Item = &'a T> {
    let msg = shape.message_with_tag(tag).unwrap();
    msg.fields.iter().zip_eq(fields.into_iter()).filter_map(move |(d, f)| (d.direction == dir).then_some(f))
}

struct Builder<'a> {
    dcx: &'a mut DiagnosticContext,
    expr_lower: ExprLower,
    connections: &'a EntityMap<ConnectionId, Shape>,
    steps: StepBuilder,
}

impl<'a> Builder<'a> {
    fn err_step(&mut self, diag: Diagnostic) -> StepId {
        let r = self.dcx.report(diag);
        self.steps.invalid(r)
    }

    fn add_value_src(&mut self) -> ValueSrcId {
        self.steps.ecx.fresh_var()
    }

    fn up_value_src(&mut self, e: &ExprKind) -> (ValueSrcId, bool) {
        let src = self.add_value_src();
        let v = self.steps.ecx.variable(ValueSrc(src, 0));
        let used = self.use_up(e, v);
        (src, used)
    }

    pub fn define_dn(&mut self, pat: &Pattern, v: ExprDnId) -> Predicate {
        self.expr_lower.define_dn(&mut self.steps.ecx, pat, v)
    }

    pub fn define_up(&mut self, pat: &Pattern, predicate: Predicate) {
        self.expr_lower.define_up(&mut self.steps.ecx, pat, predicate)
    }

    pub fn consume_up(&mut self, pat: &Pattern) -> ExprDnId {
        self.expr_lower.consume_up(&mut self.steps.ecx, pat).unwrap()
    }

    pub fn use_dn(&mut self, expr: &ExprKind) -> ExprDnId {
        self.expr_lower.use_dn(&mut self.steps.ecx, expr).unwrap()
    }

    pub fn use_up(&mut self, expr: &ExprKind, v: ExprDnId) -> bool {
        self.expr_lower.use_up(&mut self.steps.ecx, expr, v)
    }

    pub fn predicate(&self, expr: &ExprKind) -> Predicate {
        self.expr_lower.predicate(expr).unwrap()
    }

    fn build(&mut self, action: &Action) -> StepId {
        match *action {
            Action::Process {
                ref proc,
                ref channels,
            } => self.steps.add_process(proc.clone(), channels.clone()),
            Action::Stack {
                ref lo,
                conn,
                ref hi,
            } => {
                let lo = self.build(lo);
                let hi = self.build(hi);
                self.steps.stack(lo, conn, hi)
            }
            Action::Seq(ref actions) => {
                let steps: Vec<_> = actions.iter().map(|a| self.build(a)).collect();
                self.steps.seq_from(steps)
            }
            Action::Token {
                conn,
                tag,
                ref fields,
                has_body,
            } => {
                let shape = &self.connections[conn];
                let mode = shape.mode;

                let dn = fields_of_dir(shape, tag, fields, Dir::Dn).map(|f| self.use_dn(f)).collect();

                let up_src = self.add_value_src();
                let up_predicates = fields_of_dir(shape, tag, fields, Dir::Up)
                    .zip(up_src.fields())
                    .map(|(f, s)| {
                        let v = self.steps.ecx.variable(s);
                        self.use_up(f, v);
                        self.predicate(f)
                    })
                    .collect();

                let send = if mode.has_dn_channel() {
                    self.steps.send(conn.dn(), tag, dn)
                } else {
                    self.steps.accepting()
                };

                let receive = if mode.has_up_channel() {
                    self.steps.receive(conn.up(), tag, up_src, up_predicates)
                } else {
                    self.steps.accepting()
                };

                let step = self.steps.seq(send, receive);

                if has_body {
                    let pass = self.steps.pass();
                    self.steps.seq(pass, step)
                } else {
                    step
                }
            }

            Action::On {
                conn,
                tag,
                ref fields,
                ref body,
                token_has_body,
            } => {
                let shape = &self.connections[conn];
                let mode = shape.mode;

                let dn_src = self.add_value_src();
                let dn_predicates = fields_of_dir(shape, tag, fields, Dir::Dn)
                    .zip(dn_src.fields())
                    .map(|(f, i)| {
                        let v = self.steps.ecx.variable(i);
                        self.define_dn(f, v)
                    })
                    .collect();

                for f in fields_of_dir(shape, tag, fields, Dir::Up) {
                    self.define_up(f, Predicate::Any)
                }

                let inner = self.build(body);

                let up = fields_of_dir(shape, tag, fields, Dir::Up).map(|f| self.consume_up(f)).collect();

                let receive = if mode.has_dn_channel() {
                    self.steps.receive(conn.dn(), tag, dn_src, dn_predicates)
                } else {
                    assert!(dn_predicates.is_empty());
                    self.steps.accepting()
                };

                let send = if mode.has_up_channel() {
                    self.steps.send(conn.up(), tag, up)
                } else {
                    assert!(up.is_empty());
                    self.steps.accepting()
                };

                if token_has_body {
                    self.steps.seq_from([inner, receive, send])
                } else {
                    self.steps.seq_from([receive, inner, send])
                }
            }

            Action::Repeat {
                dir: RepeatMode::Up(nullable),
                ref count,
                ref body,
            } => {
                let (_, max) = match self.predicate(count) {
                    Predicate::Any => (0, None),
                    Predicate::AnyOf(s) => {
                        if s.len() == 1 && let Value::Number(n) = &s[0] && n.is_integer() && !n.is_negative() {
                            (*n.numer(), Some(*n.numer() + 1))
                        } else {
                            todo!()
                        }
                    }
                    Predicate::Range(min, max)
                        if min.is_integer() && !min.is_negative() && max.is_integer() =>
                    {
                        (*min.numer(), Some(*max.numer()))
                    }
                    _ => {
                        //return self.err_step(Diagnostic::InvalidRepeatCountPredicate {
                        //    span: sb.scope.span(count_span),
                        //});
                        todo!()
                    }
                };

                let (count_src, count_used) = self.up_value_src(&count);

                let (init, increment, exit_guard) = if count_used || max.is_some() {
                    let zero = self.steps.ecx.constant(Value::Number(0.into()));
                    let init = self.steps.assign(count_src, zero);
                    let count_var = self.steps.ecx.variable(ValueSrc(count_src, 0));
                    let count_inc = self
                        .steps
                        .ecx
                        .unary(count_var, UnaryOp::BinaryConstNumber(BinOp::Add, 1.into()));
                    let increment = self.steps.assign(count_src, count_inc);
                    let exit_guard = self.steps.guard(count_var, self.predicate(count));
                    (init, increment, exit_guard)
                } else {
                    (
                        self.steps.accepting(),
                        self.steps.accepting(),
                        self.steps.accepting(),
                    )
                };

                let inner = self.build(&body);

                let body = self.steps.seq(inner, increment);
                let repeat = self.steps.repeat(body, nullable);
                self.steps.seq_from([init, repeat, exit_guard])
            }

            Action::Repeat {
                dir: RepeatMode::Dn,
                ref count,
                ref body,
            } => {
                let count_var = self.add_value_src();
                let count = self.use_dn(&count);

                let inner = self.build(body);

                match self.steps.ecx.get(count) {
                    ExprDn::Const(Value::Number(n)) if n == &0.into() => self.steps.accepting(),
                    ExprDn::Const(Value::Number(n)) if n == &1.into() => inner,
                    _ => {
                        let init = self.steps.assign(count_var, count);
                        let count_expr = self.steps.ecx.variable(ValueSrc(count_var, 0));
                        let loop_guard = self
                            .steps
                            .guard(count_expr, Predicate::Range(1.into(), i64::MAX.into()));
                        let count_dec = self
                            .steps
                            .ecx
                            .unary(count_expr, UnaryOp::BinaryConstNumber(BinOp::Sub, 1.into()));
                        let decrement = self.steps.assign(count_var, count_dec);
                        let exit_guard = self
                            .steps
                            .guard(count_expr, Predicate::Range(0.into(), 1.into()));

                        let body = self.steps.seq_from([loop_guard, inner, decrement]);
                        let repeat = self.steps.repeat(body, true);
                        self.steps.seq_from([init, repeat, exit_guard])
                    }
                }
            }

            Action::For {
                count,
                ref vars,
                ref body,
            } => {
                let var_init: Vec<(Option<ExprDnId>, Option<Predicate>)> = vars
                    .iter()
                    .map(|v| {
                        (
                            self.expr_lower.use_dn(&mut self.steps.ecx, &v.outer),
                            self.expr_lower.predicate(&v.outer),
                        )
                    })
                    .collect();

                let mut up_parts: Vec<Vec<ExprDnId>> = vars.iter().map(|_| Vec::new()).collect();

                let seq: Vec<_> = (0..count)
                    .map(|iter| {
                        for (var, (dn, predicate)) in vars.iter().zip(var_init.iter()) {
                            // Initialize var with element from outer
                            self.expr_lower.set(
                                var.var,
                                dn.map(|d| self.steps.ecx.index(d, iter)),
                                predicate.as_ref().map(|p| p.index(iter)),
                            );
                        }

                        // Expand an iteration of the body
                        let inner = self.build(body);

                        for (var, u) in vars.iter().zip(up_parts.iter_mut()) {
                            u.push(
                                self.expr_lower
                                    .take_up(var.var)
                                    .unwrap_or(ExprDnId::INVALID),
                            );
                        }

                        inner
                    })
                    .collect();

                for (var, parts) in vars.iter().zip(up_parts.into_iter()) {
                    // concatenate up values from each iteration
                    let concat = self
                        .steps
                        .ecx
                        .concat(parts.into_iter().map(ConcatElem::Elem).collect());

                    self.use_up(&var.outer, concat);
                }

                self.steps.seq_from(seq)
            }

            Action::Alt {
                dir: Dir::Dn,
                ref scrutinee,
                ref arms,
            } => {
                let scrutinee_dn = self.use_dn(scrutinee);

                let arms = arms
                    .iter()
                    .map(|arm| {
                        let predicate = self.define_dn(&arm.discriminant, scrutinee_dn);

                        let inner = self.build(&arm.body);

                        let guard = self.steps.guard(scrutinee_dn.clone(), predicate);
                        self.steps.seq(guard, inner)
                    })
                    .collect();
                self.steps.alt(arms)
            }

            Action::Alt {
                dir: Dir::Up,
                ref scrutinee,
                ref arms,
            } => {
                let (src, used) = self.up_value_src(&scrutinee);
                let scrutinee_src = used.then_some(src);

                let arms = arms
                    .iter()
                    .map(|arm| {
                        self.define_up(&arm.discriminant, Predicate::Any);

                        let inner = self.build(&arm.body);

                        let exit = if let Some(scrutinee_src) = scrutinee_src {
                            let val = self.consume_up(&arm.discriminant);
                            self.steps.assign(scrutinee_src, val)
                        } else {
                            self.steps.accepting()
                        };
                        self.steps.seq(inner, exit)
                    })
                    .collect();
                self.steps.alt(arms)
            }

            Action::Any { ref arms } => {
                let arms = arms.iter().map(|a| self.build(a)).collect();
                self.steps.alt(arms)
            }

            Action::Pass => self.steps.pass(),
            Action::Error(ref r) => self.steps.invalid(r.clone()),
        }
    }
}
