use crate::entitymap::{EntityMap, entity_key};
use crate::core::{ChannelId, ConnectionId, ExprDn, MatchSet, MessagePatternSet, Predicate, ProcessChain, Shape, Step, StepId, ValueSrcId};
use super::PrimitiveProcess;

type VariantId = usize;
entity_key!(pub TaskId);
entity_key!(pub InsnId);
entity_key!(pub CounterId);
entity_key!(pub PrimitiveId);

#[derive(Debug)]
pub enum Insn {
    Nop,

    /// Down-evaluate a message and send it on the specified channel
    Send(ChannelId, VariantId, Vec<ExprDn>),

    /// Wait for data ready on channel, consume message from channel and set variables
    Consume(ChannelId, VariantId, Option<ValueSrcId>, Vec<Predicate>),

    /// Wait for data ready on channel, test received message from channel by up-evaluating expression, without
    /// consuming the message or writing variables. If not matched, jump.
    Test(ChannelId, MessagePatternSet, InsnId),

    Primitive(PrimitiveId, Vec<ChannelId>),

    Assign(ValueSrcId, ExprDn),
    TestVal(ExprDn, Vec<Predicate>, InsnId),
    TestValOrFail(ExprDn, Predicate),

    /// Start a new thread of execution
    Fork(TaskId, InsnId),

    Join(TaskId),

    /// 
    End,

    /// Unconditional jump
    Jump(InsnId),

    /// Failed to match
    Fail,
}

struct Compiler<'a> {
    program: &'a ProcessChain,
    insns: EntityMap<InsnId, Insn>,
    tasks: EntityMap<TaskId, ()>,
    primitives: EntityMap<PrimitiveId, Arc<dyn PrimitiveProcess + 'static>>,
}

impl Compiler<'_> {
    fn next_ip(&self) -> InsnId {
        self.insns.next_key()
    }

    fn emit(&mut self, i: Insn) {
        self.insns.push(i);
    }

    fn emit_placeholder(&mut self) -> InsnId {
        self.insns.push(Insn::Fail)
    }

    fn backpatch(&mut self, ip: InsnId, i: Insn) {
        self.insns[ip] = i;
    }

    fn new_task(&mut self) -> TaskId {
        self.tasks.push(())
    }

    fn new_primitive(&mut self, p: Arc<dyn PrimitiveProcess>) -> PrimitiveId {
        self.primitives.push(p)
    }

    fn seq_prep(&mut self, prev_followlast: &Option<MatchSet>, next_first: &MatchSet) {
        match (prev_followlast, next_first) {
            (None, MatchSet::Receive { .. }) => { },
            (None | Some(MatchSet::Receive { .. }), MatchSet::Send { chan, variant, send, .. }) => {
                self.emit(Insn::Send(*chan, *variant, send.clone()));
            }
            (None | Some(MatchSet::Receive { .. }), MatchSet::Guard { .. }) => {}
            (None, MatchSet::Process) => {}
            (Some(MatchSet::Receive { .. }), MatchSet::Receive { .. }) => {}
            (Some(MatchSet::Guard { .. }), MatchSet::Guard { .. }) => {}
            (Some(MatchSet::Send { chan: d1, variant: v1, send: s1, .. }),
             MatchSet::Send { chan: d2, variant: v2, send: s2, .. })
             if d1 == d2 && v1 == v2 && s1 == s2 => {}
            (Some(_), _) => {
                panic!("Invalid followlast -> first: {prev_followlast:?} {next_first:?}")
            }
        }
    }

    fn test_matchset(&self, m: &MatchSet, if_false: InsnId) -> Insn {
        match m {
            MatchSet::Process | MatchSet::Send { .. } => Insn::Nop,
            MatchSet::Receive { chan, receive } => {
                Insn::Test(*chan, receive.clone(), if_false)
            }
            MatchSet::Guard { expr, test } => {
                Insn::TestVal(expr.clone(), test.clone(), if_false)
            },
            
        }
    }

    fn compile_step(&mut self, step: StepId) {
        match self.program.steps[step] {
            Step::Fail => {
                self.emit(Insn::Fail);
            }
            Step::Accept => {}
            Step::Pass => todo!(),
            Step::Stack { lo, conn, hi } => {
                let lo_info = &self.program.step_info[lo];
                let hi_info = &self.program.step_info[hi];

                let i = self.emit_placeholder();
                let task = self.new_task();

                self.seq_prep(&None, &hi_info.first);
                self.compile_step(hi);
                if self.program.connections[conn].mode.has_dn_channel() {
                    self.emit(Insn::Send(conn.dn(), 0, vec![]));
                }
                if self.program.connections[conn].mode.has_up_channel() {
                    self.emit(Insn::Consume(conn.up(), 0, None, vec![]));
                }
                self.emit(Insn::End);

                self.backpatch(i, Insn::Fork(task, self.next_ip()));

                self.seq_prep( &None, &lo_info.first);
                self.compile_step(lo);
                if self.program.connections[conn].mode.has_dn_channel() {
                    self.emit(Insn::Consume(conn.dn(), 0, None, vec![]));
                }
                if self.program.connections[conn].mode.has_up_channel() {
                    self.emit(Insn::Send(conn.up(), 0, vec![]));
                }
                self.emit(Insn::Join(task));
            }

            Step::Primitive(ref p, ref chans) => {
                let id = self.new_primitive(p.clone());
                self.emit(Insn::Primitive(id, chans.clone()));
            }

            Step::Send { .. } => {
                // handled in seq_prep
            }

            Step::Receive { chan, variant, val, ref msg}=> {
                self.emit(Insn::Consume(chan, variant, Some(val), msg.clone()));
            }

            Step::Seq(s1, s2) => {
                let i1 = &self.program.step_info[s1];
                let i2 = &self.program.step_info[s2];

                self.compile_step(s1);
                self.seq_prep(&i1.followlast, &i2.first);
                self.compile_step(s2);
            }

            Step::Repeat(inner, _) => {
                let inner_info = &self.program.step_info[inner];

                let test_ip = self.emit_placeholder();

                self.compile_step(inner);
                self.seq_prep(&inner_info.followlast, &inner_info.first);
                self.emit(Insn::Jump(test_ip));

                self.backpatch(test_ip, self.test_matchset(&inner_info.first, self.next_ip()));
            }

            Step::Alt(ref opts) => {
                let mut jumps_to_final = vec![];

                // resolve checks that there is at least one alt arm
                let (&last, rest) = opts.split_last().unwrap();

                for &body in rest {
                    let inner_info = &self.program.step_info[body];
                    let test = self.emit_placeholder();

                    self.compile_step(body);

                    jumps_to_final.push(self.emit_placeholder());

                    self.backpatch(test, self.test_matchset(&inner_info.first, self.next_ip()));
                }

                // Final arm doesn't need a test or jump to end
                self.compile_step(last);

                for ip in jumps_to_final {
                    self.backpatch(ip, Insn::Jump(self.next_ip()));
                }
            }

            Step::Assign(dst, ref src) => {
                self.emit(Insn::Assign(dst, src.clone()))
            }

            Step::Guard(ref val, ref pred) => {
                self.emit(Insn::TestValOrFail(val.clone(), pred.clone()))
            }
        }
    }
}

pub struct CompiledProgram {
    insns: EntityMap<InsnId, Insn>,
    vars: EntityMap<ValueSrcId, ()>,
    tasks: EntityMap<TaskId, ()>,
    connections: EntityMap<ConnectionId, Shape>,
    conn_dn: ConnectionId,
    conn_up: Option<ConnectionId>,
    primitives: EntityMap<PrimitiveId, Arc<dyn PrimitiveProcess>>,
}

pub fn compile(program: &ProcessChain) -> CompiledProgram {
    let mut compiler = Compiler {
        program,
        insns: EntityMap::new(),
        tasks: EntityMap::new(),
        primitives: EntityMap::new(),
    };

    let _root_task = compiler.new_task();

    compiler.seq_prep(&None, &program.step_info[program.root].first);
    compiler.compile_step(program.root);
    compiler.emit(Insn::End);

    for (ip, insn) in compiler.insns.iter() {
        debug!("{ip:4?} {insn:?}");
    }

    CompiledProgram {
        insns: compiler.insns,
        vars: program.vars.clone(),
        tasks: compiler.tasks,
        connections: program.connections.clone(),
        conn_dn: program.conn_dn,
        conn_up: program.up.as_ref().map(|c| c.1),
        primitives: compiler.primitives,
    }
}

use std::{task::{Context, Poll}, sync::Arc, future::Future, pin::Pin};
use futures_lite::FutureExt;
use crate::Value;
use super::channel::{Channel, ChannelMessage, SeqChannels};

pub struct ProgramExec {
    program: Arc<CompiledProgram>,
    failed: bool,
    tasks: EntityMap<TaskId, InsnId>,
    registers: EntityMap<ValueSrcId, Vec<Value>>,
    channels: EntityMap<ChannelId, Channel>,
    primitives: EntityMap<PrimitiveId, Option<Pin<Box<dyn Future<Output = Result<(), ()>>>>>>,
}

const DONE_IP: InsnId = InsnId::from(u32::MAX);
const INITIAL_TASK: TaskId = TaskId::from(0);

impl EntityMap<ValueSrcId, Vec<Value>> {
    fn down_eval(&self, expr: &ExprDn) -> Value {
        expr.eval(&|var| {
            self[var.0][var.1 as usize].clone()
        })
    }

    fn up_eval_test(&mut self, p: &Predicate, v: &Value) -> bool {
        p.test(v)
    }
}

impl ProgramExec {
    pub fn new(program: Arc<CompiledProgram>, channels_lo: SeqChannels, channels_hi: SeqChannels) -> ProgramExec {
        let mut channels: EntityMap<ChannelId, Channel> = program.connections.iter().flat_map(|_| [Channel::new(), Channel::new()]).collect::<Vec<_>>().into();

        if let Some(c) = channels_lo.dn { channels[program.conn_dn.dn()] = c; }
        if let Some(c) = channels_lo.up { channels[program.conn_dn.up()] = c; }
        if let Some(c) = channels_hi.dn { channels[program.conn_up.unwrap().dn()] = c; }
        if let Some(c) = channels_hi.up { channels[program.conn_up.unwrap().up()] = c; }

        let mut tasks: EntityMap<TaskId, InsnId> = program.tasks.iter().map(|_| DONE_IP).collect();
        tasks[INITIAL_TASK] = InsnId::from(0);

        ProgramExec {     
            tasks,
            failed: false,
            registers: program.vars.iter().map(|_| Vec::new()).collect(),
            channels,
            primitives: program.primitives.iter().map(|_| None).collect(),
            program,
        }
    }

    pub fn poll_all(&mut self, cx: &mut Context) -> Poll<Result<(), ()>> {
        if self.failed {
            return Poll::Ready(Err(()));
        }

        for task in self.tasks.keys() {
            if self.tasks[task] != DONE_IP {
                self.poll_task(cx, task);
            }
        }

        if self.failed {
            return Poll::Ready(Err(()));
        } else if self.tasks[INITIAL_TASK] == DONE_IP {
            Poll::Ready(Result::Ok(()))
        } else {
            Poll::Pending
        }
    }
    
    fn poll_task(&mut self, cx: &mut Context, task: TaskId) {
        let p = &*self.program;
        loop {
            let ip = self.tasks[task];
            let mut next_ip = InsnId::from(u32::from(ip) + 1);
            debug!("Task {task:3}: {ip:4} {insn:?}", task = u32::from(task), ip = u32::from(ip), insn = p.insns[ip]);
            match p.insns[ip] {
                Insn::Nop => {}
                Insn::Send(chan, variant, ref expr) => {
                    let values = expr.iter().map(|e| {
                        self.registers.down_eval(e)
                    }).collect();
                    self.channels[chan].send(ChannelMessage { variant, values });
                }
                Insn::Consume(chan, variant, reg, ref exprs) => {
                    let Poll::Ready(rx) = self.channels[chan].poll_receive(cx) else { return };
                    let rx = rx.pop();
                    debug!("  Consume {rx:?}");
    
                    if rx.variant != variant {
                        self.failed = true;
                        return;
                    };
    
                    assert_eq!(rx.values.len(), exprs.len());
    
                    for (p, v) in exprs.iter().zip(&rx.values) {
                        if !self.registers.up_eval_test(p, v) {
                            debug!("  failed to match");
                            self.failed = true;
                            return;
                        }
                    }
                    
                    if let Some(reg) = reg {
                        self.registers[reg] = rx.values;
                    }
                }
                Insn::Test(chan, ref pat, if_false) => {
                    let Poll::Ready(rx) = self.channels[chan].poll_receive(cx) else { return };
                    let msg = rx.peek();

                    let matched = pat.iter().any(|p| {
                        p.variant == msg.variant && p.fields.iter().zip(msg.values.iter()).all(|(p, v)| {
                            self.registers.up_eval_test(p, v)
                        })
                    });

                    if !matched {
                        next_ip = if_false;
                    }
                }
                Insn::Primitive(id, ref channels) => {
                    let ch = channels.iter().map(|&id| self.channels[id].clone()).collect();
                    let fut = self.primitives[id].get_or_insert_with(|| p.primitives[id].run(ch));
                    match fut.poll(cx) {
                        Poll::Pending => return,
                        Poll::Ready(Ok(_)) => {
                            drop(self.primitives[id].take());
                        }
                        Poll::Ready(Err(_)) => {
                            self.failed = true;
                            return;
                        }
                    }
                }
                Insn::Assign(l, ref r) => {
                    let v = self.registers.down_eval(r);
                    log::debug!("assigned {l:?} = {v}");
                    self.registers[l].clear();
                    self.registers[l].push(v);
                }
                Insn::TestVal(ref e, ref preds, if_false) => {
                    let v = self.registers.down_eval(e);
                    if !preds.iter().any(|pred| self.registers.up_eval_test(pred, &v)) {
                        next_ip = if_false;
                    }
                }
                Insn::TestValOrFail(ref e, ref pred) => {
                    let v = self.registers.down_eval(e);
                    log::debug!("testing value {v}");
                    if !self.registers.up_eval_test(pred, &v) {
                        self.failed = true;
                        return;
                    }
                }
                Insn::Fork(other, dest) => {
                    self.tasks[other] = next_ip;
                    next_ip = dest;
                }
                Insn::Join(other) => {
                    if self.tasks[other] != DONE_IP {
                        return;
                    }
                }
                Insn::End => {
                    self.tasks[task] = DONE_IP;
                    cx.waker().wake_by_ref(); // Wake the joining task
                    return;
                }
                Insn::Jump(dest) => {
                    next_ip = dest;
                }
                Insn::Fail => {
                    self.failed = true;
                    return;
                }
            }
            self.tasks[task] = next_ip;
        }
    }
}

impl Future for ProgramExec {
    type Output = Result<(), ()>;

    fn poll(mut self: std::pin::Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.poll_all(cx)
    }
}
