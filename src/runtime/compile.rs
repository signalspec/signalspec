use crate::entitymap::{EntityMap, entity_key};
use crate::core::{Shape, ExprDn, ProcessChain, MatchSet, MessagePatternSet, StepId, Step, Predicate, ValueSrcId};
use super::PrimitiveProcess;

type VariantId = usize;
entity_key!(pub TaskId);
entity_key!(pub InsnId);
entity_key!(pub ChanId);
entity_key!(pub CounterId);
entity_key!(pub PrimitiveId);

#[derive(Debug)]
pub enum Insn {
    Nop,

    /// Down-evaluate a message and send it on the specified channel
    Send(ChanId, VariantId, Vec<ExprDn>),

    /// Wait for data ready on channel, consume message from channel and set variables
    Consume(ChanId, VariantId, Option<ValueSrcId>, Vec<Predicate>),

    /// Wait for data ready on channel, test received message from channel by up-evaluating expression, without
    /// consuming the message or writing variables. If not matched, jump.
    Test(ChanId, MessagePatternSet, InsnId),

    Primitive(PrimitiveId, Vec<ChanId>),

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

#[derive(Clone, Copy, Debug)]
struct ChannelIds {
    dn_tx: Option<ChanId>,
    dn_rx: Option<ChanId>,
    up_tx: Option<ChanId>, 
    up_rx: Option<ChanId>,
}

impl ChannelIds {
    fn without_up(self) -> ChannelIds {
        Self { up_rx: None, up_tx: None, ..self }
    }

    fn only_downward(self) -> ChannelIds {
        ChannelIds { dn_rx: None, up_tx: None, ..self }
    }

    fn only_upward(self) -> ChannelIds {
        ChannelIds { dn_tx: None, up_rx: None, ..self }
    }

    fn to_vec(self) -> Vec<ChanId> {
        [self.dn_tx, self.dn_rx, self.up_tx, self.up_rx].into_iter().flatten().collect()
    }
}

struct Compiler<'a> {
    program: &'a ProcessChain,
    insns: EntityMap<InsnId, Insn>,
    tasks: EntityMap<TaskId, ()>,
    counters: EntityMap<CounterId, ()>,
    channels: EntityMap<ChanId, ()>,
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

    fn new_counter(&mut self) -> CounterId {
        self.counters.push(())
    }

    fn new_channel(&mut self) -> ChanId {
        self.channels.push(())
    }

    fn new_channels(&mut self, shape: &Shape) -> (Option<ChanId>, Option<ChanId>) {
        (shape.mode.has_dn_channel().then(|| self.new_channel()), shape.mode.has_up_channel().then(|| self.new_channel()))
    }

    fn new_primitive(&mut self, p: Arc<dyn PrimitiveProcess>) -> PrimitiveId {
        self.primitives.push(p)
    }

    fn seq_prep(&mut self, channels: ChannelIds, prev_followlast: &Option<MatchSet>, next_first: &MatchSet) {
        match (prev_followlast, next_first) {
            (None, MatchSet::Receive { .. }) => { },
            (None | Some(MatchSet::Receive { .. }), MatchSet::Send { dir: Dir::Dn, variant, send, .. }) => {
                if let Some(c) = channels.dn_tx {
                    self.emit(Insn::Send(c, *variant, send.clone()));
                }
            }
            (None | Some(MatchSet::Receive { .. }), MatchSet::Send { dir: Dir::Up, .. }) => {}
            (None | Some(MatchSet::Receive { .. }), MatchSet::Guard { .. }) => {}
            (None, MatchSet::Process) => {}
            (Some(MatchSet::Receive { .. }), MatchSet::Receive { .. }) => {}
            (Some(MatchSet::Guard { .. }), MatchSet::Guard { .. }) => {}
            (Some(MatchSet::Send { dir: d1, variant: v1, send: s1, .. }),
             MatchSet::Send { dir: d2, variant: v2, send: s2, .. })
             if d1 == d2 && v1 == v2 && s1 == s2 => {}
            (Some(_), _) => {
                panic!("Invalid followlast -> first: {prev_followlast:?} {next_first:?}")
            }
        }
    }

    fn test_matchset(&self, channels: ChannelIds, m: &MatchSet, if_false: InsnId) -> Insn {
        match m {
            MatchSet::Process | MatchSet::Send { .. } => Insn::Nop,
            MatchSet::Receive { dir: Dir::Up, receive } => {
                Insn::Test(channels.up_rx.unwrap(), receive.clone(), if_false)
            }
            MatchSet::Receive { dir: Dir::Dn, receive, .. } => {
                Insn::Test(channels.dn_rx.unwrap(), receive.clone(), if_false)
            },
            MatchSet::Guard { expr, test } => {
                Insn::TestVal(expr.clone(), test.clone(), if_false)
            },
            
        }
    }

    fn compile_step(&mut self, step: StepId, channels: ChannelIds) {
        match self.program.steps[step] {
            Step::Fail => {
                self.emit(Insn::Fail);
            }
            Step::Accept => {}
            Step::Pass => todo!(),
            Step::Stack { lo, ref shape, hi} => {
                let lo_info = &self.program.step_info[lo];
                let hi_info = &self.program.step_info[hi];
                let (c_dn, c_up) = self.new_channels(shape);

                let i = self.emit_placeholder();
                let task = self.new_task();

                let channels_hi = ChannelIds {
                    dn_tx: c_dn,
                    dn_rx: c_up,
                    up_tx: channels.up_tx,
                    up_rx: channels.up_rx,
                };
                self.seq_prep(channels_hi, &None, &hi_info.first);
                self.compile_step(hi, channels_hi);
                if let Some(c) = c_dn {
                    self.emit(Insn::Send(c, 0, vec![]));
                }
                if let Some(c) = c_up {
                    self.emit(Insn::Consume(c, 0, None, vec![]));
                }
                self.emit(Insn::End);

                self.backpatch(i, Insn::Fork(task, self.next_ip()));

                let channels_lo = ChannelIds {
                    dn_tx: channels.dn_tx,
                    dn_rx: channels.dn_rx,
                    up_tx: c_up,
                    up_rx: c_dn,
                };
                self.seq_prep(channels_lo, &None, &lo_info.first);
                self.compile_step(lo, channels_lo);
                if let Some(c) = c_dn {
                    self.emit(Insn::Consume(c, 0, None, vec![]));
                }
                if let Some(c) = c_up {
                    self.emit(Insn::Send(c, 0, vec![]));
                }
                self.emit(Insn::Join(task));
            }

            Step::Primitive(ref p) => {
                let id = self.new_primitive(p.clone());
                let chans = channels.to_vec();
                self.emit(Insn::Primitive(id, chans));
            }

            Step::Send { dir: Dir::Dn, .. } => {
                // handled in seq_prep
            }

            Step::Send { dir: Dir::Up, variant, ref msg  } => {
                if let Some(c) = channels.up_tx {
                    self.emit(Insn::Send(c, variant, msg.clone()));
                }
            }

            Step::Receive { dir: Dir::Dn, variant, val, ref msg}=> {
                if let Some(c) = channels.dn_rx {
                    self.emit(Insn::Consume(c, variant, Some(val), msg.clone()));
                }
            }

            Step::Receive { dir: Dir::Up, variant, val, ref msg } => {
                if let Some(c) = channels.up_rx {
                    self.emit(Insn::Consume(c, variant, Some(val), msg.clone()));
                }
            }

            Step::Seq(s1, s2) => {
                let i1 = &self.program.step_info[s1];
                let i2 = &self.program.step_info[s2];

                self.compile_step(s1, channels);
                self.seq_prep(channels, &i1.followlast, &i2.first);
                self.compile_step(s2, channels);
            }

            Step::Repeat(inner, _) => {
                let inner_info = &self.program.step_info[inner];

                let test_ip = self.emit_placeholder();

                self.compile_step(inner, channels);
                self.seq_prep(channels, &inner_info.followlast, &inner_info.first);
                self.emit(Insn::Jump(test_ip));

                self.backpatch(test_ip, self.test_matchset(channels, &inner_info.first, self.next_ip()));
            }

            Step::Alt(ref opts) => {
                let mut jumps_to_final = vec![];

                // resolve checks that there is at least one alt arm
                let (&last, rest) = opts.split_last().unwrap();

                for &body in rest {
                    let inner_info = &self.program.step_info[body];
                    let test = self.emit_placeholder();

                    self.compile_step(body, channels);

                    jumps_to_final.push(self.emit_placeholder());

                    self.backpatch(test, self.test_matchset(channels, &inner_info.first, self.next_ip()));
                }

                // Final arm doesn't need a test or jump to end
                self.compile_step(last, channels);

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
    channels: EntityMap<ChanId, ()>,
    primitives: EntityMap<PrimitiveId, Arc<dyn PrimitiveProcess>>,
}

pub fn compile(program: &ProcessChain) -> CompiledProgram {
    let mut compiler = Compiler {
        program,
        insns: EntityMap::new(),
        counters: EntityMap::new(),
        tasks: EntityMap::new(),
        channels: EntityMap::new(),
        primitives: EntityMap::new(),
    };

    let _root_task = compiler.new_task();

    let channels = ChannelIds {
        dn_tx: program.shape_dn.mode.has_dn_channel().then(|| compiler.new_channel()),
        dn_rx: program.shape_dn.mode.has_up_channel().then(|| compiler.new_channel()),
        up_tx: program.shape_up.as_ref().map_or(false, |s| s.mode.has_up_channel()).then(|| compiler.new_channel()),
        up_rx: program.shape_up.as_ref().map_or(false, |s| s.mode.has_dn_channel()).then(|| compiler.new_channel()),
    };

    compiler.seq_prep(channels, &None, &program.step_info[program.root].first);
    compiler.compile_step(program.root, channels);
    compiler.emit(Insn::End);

    for (ip, insn) in compiler.insns.iter() {
        debug!("{ip:4?} {insn:?}");
    }

    CompiledProgram {
        insns: compiler.insns,
        vars: program.vars.clone(),
        tasks: compiler.tasks,
        channels: compiler.channels,
        primitives: compiler.primitives,
    }
}

use std::{task::{Context, Poll}, sync::Arc, future::Future, pin::Pin};
use futures_lite::FutureExt;
use crate::{Dir, Value};
use super::channel::{Channel, ChannelMessage, SeqChannels};

pub struct ProgramExec {
    program: Arc<CompiledProgram>,
    failed: bool,
    tasks: EntityMap<TaskId, InsnId>,
    registers: EntityMap<ValueSrcId, Vec<Value>>,
    channels: EntityMap<ChanId, Channel>,
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
        let mut channels = Vec::with_capacity(program.channels.len());
        channels.extend([channels_lo.dn, channels_lo.up, channels_hi.dn, channels_hi.up].into_iter().flatten());
        channels.resize_with(program.channels.len(), Channel::new);

        let mut tasks: EntityMap<TaskId, InsnId> = program.tasks.iter().map(|_| DONE_IP).collect();
        tasks[INITIAL_TASK] = InsnId::from(0);

        ProgramExec {     
            tasks,
            failed: false,
            registers: program.vars.iter().map(|_| Vec::new()).collect(),
            channels: channels.into(),
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
