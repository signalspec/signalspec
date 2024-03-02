use crate::entitymap::{EntityMap, entity_key};
use crate::core::{Shape, ExprDn, ProcessChain, MatchSet, MessagePatternSet, StepId, Step, AltUpArm, AltDnArm, Predicate, ValueSrcId, ShapeMode};
use super::PrimitiveProcess;

type VariantId = usize;
entity_key!(pub TaskId);
entity_key!(pub InsnId);
entity_key!(pub ChanId);
entity_key!(pub CounterId);
entity_key!(pub PrimitiveId);

#[derive(Debug)]
pub enum Insn {
    /// Down-evaluate a message and send it on the specified channel
    Send(ChanId, VariantId, Vec<ExprDn>),

    /// Wait for data ready on channel, consume message from channel and set variables
    Consume(ChanId, VariantId, Vec<(Predicate, ValueSrcId)>),

    /// Wait for data ready on channel, test received message from channel by up-evaluating expression, without
    /// consuming the message or writing variables. If not matched, jump.
    Test(ChanId, MessagePatternSet, InsnId),

    Primitive(PrimitiveId, Vec<ChanId>),

    CounterReset(CounterId, i64),
    CounterUpEval(CounterId, i64, Option<i64>, ValueSrcId),
    CounterDownEval(CounterId, ExprDn),
    CounterDecrement(CounterId, InsnId),
    CounterIncrement(CounterId),

    Assign(ValueSrcId, ExprDn),
    TestVal(ExprDn, Predicate, InsnId),

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
            (None, MatchSet::MessageUp { .. }) => { },
            (None | Some(MatchSet::MessageUp { .. }), MatchSet::MessageDn { variant, send, .. }) => {
                if let Some(c) = channels.dn_tx {
                    self.emit(Insn::Send(c, *variant, send.clone()));
                }
            }
            (None, MatchSet::Process) => {}
            (Some(MatchSet::MessageUp { .. }), MatchSet::MessageUp { .. }) => {}
            (Some(MatchSet::MessageDn { variant: v1, send: s1, .. }),
             MatchSet::MessageDn { variant: v2, send: s2, .. })
             if v1 == v2 && s1 == s2 => {}
            (Some(_), _) => {
                panic!("Invalid followlast -> first: {prev_followlast:?} {next_first:?}")
            }
        }
    }

    fn test_matchset(&self, channels: ChannelIds, m: &MatchSet, if_false: InsnId) -> Insn {
        match m {
            MatchSet::Process => todo!(),
            MatchSet::MessageUp { receive } => {
                Insn::Test(channels.up_rx.unwrap(), receive.clone(), if_false)
            }
            MatchSet::MessageDn { receive, .. } => {
                Insn::Test(channels.dn_rx.unwrap(), receive.clone(), if_false)
            },
        }
    }

    fn compile_step(&mut self, step: StepId, channels: ChannelIds) {
        match self.program.steps[step] {
            Step::Invalid(_) => panic!("Compiling invalid step"),
            Step::Pass => todo!(),
            Step::Stack { lo, ref shape, hi} => {
                if let Step::TokenTransaction { variant } = self.program.steps[lo] {
                    if let (ShapeMode::Async, Some(dn_rx), Some(dn_tx)) = (shape.mode, channels.dn_rx, channels.dn_tx) {
                        // For ShapeMode::Async, emit all downward sends before upward receives
                        self.compile_step(hi, channels.only_downward());
                        self.emit(Insn::Send(dn_tx, variant, vec![]));
                        self.compile_step(hi, channels.only_upward());
                        self.emit(Insn::Consume(dn_rx, variant, vec![]));
                    } else {
                        self.compile_step(hi, channels);

                        if let Some(c) = channels.dn_tx {
                            self.emit(Insn::Send(c, variant, vec![]));
                        }

                        if let Some(c) = channels.dn_rx {
                            self.emit(Insn::Consume(c, variant, vec![]));
                        }
                    }
                    return;
                }

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
                    self.emit(Insn::Consume(c, 0, vec![]));
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
                    self.emit(Insn::Consume(c, 0, vec![]));
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

            Step::Token { variant, ref up, .. }=> {
                if let Some(c) = channels.dn_rx {
                    self.emit(Insn::Consume(c, variant, up.clone()));
                }
            }

            Step::TokenTop { inner_mode, variant, ref dn_vars, ref dn, ref up, inner } => {
                let inner_info = &self.program.step_info[inner];

                match inner_mode {
                    ShapeMode::Up | ShapeMode::Null => {},
                    ShapeMode::Dn | ShapeMode::Sync | ShapeMode::Async => {
                        if let Some(c) = channels.up_rx {
                            let dn_and_vars = dn.iter().cloned().zip(dn_vars.iter().cloned()).collect();
                            self.emit(Insn::Consume(c, variant, dn_and_vars));
                        }

                        self.seq_prep(channels.without_up(), &None, &inner_info.first);
                    },
                }

                self.compile_step(inner, channels.without_up());

                if let Some(c) = channels.up_tx {
                    self.emit(Insn::Send(c, variant, up.clone()));
                }
            }

            Step::TokenTransaction { .. } => {
                todo!(); // like `pass`, normally handled in `stack`
            }

            Step::TokenTopTransaction { variant, inner, .. } => {
                self.compile_step(inner, channels);

                if let Some(c) = channels.up_rx {
                    self.emit(Insn::Consume(c, variant, vec![]));
                }

                if let Some(c) = channels.up_tx {
                    self.emit(Insn::Send(c, variant, vec![]));
                }
            }

            Step::Seq(ref steps) => {
                let mut prev_followlast = None;
                for &step in steps.iter() {
                    let inner_info = &self.program.step_info[step];

                    if let Some(prev_followlast) = prev_followlast {
                        self.seq_prep(channels, prev_followlast, &inner_info.first);
                    }

                    self.compile_step(step, channels);

                    prev_followlast = Some(&inner_info.followlast);
                }
            }

            Step::RepeatUp { min, max, inner, count } => {
                let inner_info = &self.program.step_info[inner];
                let counter = self.new_counter();

                self.emit(Insn::CounterReset(counter, 0));
                let test_ip = self.emit_placeholder();
                self.compile_step(inner, channels);
                self.seq_prep(channels, &inner_info.followlast, &inner_info.first);
                self.emit(Insn::CounterIncrement(counter));
                self.emit(Insn::Jump(test_ip));

                self.backpatch(test_ip, self.test_matchset(channels, &inner_info.first, self.next_ip()));
                self.emit(Insn::CounterUpEval(counter, min, max, count));
            }

            Step::RepeatDn{ ref count, inner } => {
                let inner_info = &self.program.step_info[inner];
                let counter = self.new_counter();
                self.emit(Insn::CounterDownEval(counter, count.clone()));
                let test_ip = self.emit_placeholder();

                self.seq_prep(channels, &None, &inner_info.first);
                self.compile_step(inner, channels);
                self.emit(Insn::Jump(test_ip));

                self.backpatch(test_ip, Insn::CounterDecrement(counter, self.next_ip()));
            }

            Step::AltUp(ref opts, ref dests) => {
                let mut jumps_to_final = vec![];

                fn inner(s: &mut Compiler, body: StepId, channels: ChannelIds, dests: &Vec<ValueSrcId>, vals: &Vec<ExprDn>) {
                    s.compile_step(body, channels);

                    for (&id, e) in dests.iter().zip(vals.iter()) {
                        s.emit(Insn::Assign(id, e.clone()));
                    }
                }

                // resolve checks that there is at least one alt arm
                let (last, rest) = opts.split_last().unwrap();

                for &AltUpArm { ref vals, body } in rest {
                    let inner_info = &self.program.step_info[body];
                    let test = self.emit_placeholder();

                    inner(self, body, channels, dests, vals);

                    jumps_to_final.push(self.emit_placeholder());

                    self.backpatch(test, self.test_matchset(channels, &inner_info.first, self.next_ip()));
                }

                {
                    // Final arm doesn't need a test or jump to end
                    let &AltUpArm { ref vals, body } = last;
                    inner(self, body, channels, dests, vals);
                }

                for ip in jumps_to_final {
                    self.backpatch(ip, Insn::Jump(self.next_ip()));
                }
            }

            Step::AltDn(ref srcs, ref opts) => {
                let mut jumps_to_final = vec![];

                for &AltDnArm { ref vals, body } in opts {
                    let inner_info = &self.program.step_info[body];
                    let jumps_to_next: Vec<_> = vals.iter().map(|_| self.emit_placeholder()).collect();

                    self.seq_prep(channels, &None, &inner_info.first);
                    self.compile_step(body, channels);

                    jumps_to_final.push(self.emit_placeholder());

                    for ((e, pred), &test_ip) in srcs.iter().zip(vals).zip(jumps_to_next.iter()) {
                        self.backpatch(test_ip, Insn::TestVal(e.clone(), pred.clone(), self.next_ip()));
                    }

                }
                self.emit(Insn::Fail);

                for ip in jumps_to_final {
                    self.backpatch(ip, Insn::Jump(self.next_ip()));
                }
            }
        }
    }
}

pub struct CompiledProgram {
    insns: EntityMap<InsnId, Insn>,
    vars: EntityMap<ValueSrcId, ()>,
    tasks: EntityMap<TaskId, ()>,
    counters: EntityMap<CounterId, ()>,
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
        counters: compiler.counters,
        channels: compiler.channels,
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
    registers: EntityMap<ValueSrcId, Option<Value>>,
    channels: EntityMap<ChanId, Channel>,
    counters: EntityMap<CounterId, i64>,
    primitives: EntityMap<PrimitiveId, Option<Pin<Box<dyn Future<Output = Result<(), ()>>>>>>,
}

const DONE_IP: InsnId = InsnId::from(u32::MAX);
const INITIAL_TASK: TaskId = TaskId::from(0);

impl EntityMap<ValueSrcId, Option<Value>> {
    fn down_eval(&self, expr: &ExprDn) -> Value {
        expr.eval(&|var| {
            self[var].clone().unwrap()
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
            registers: program.vars.iter().map(|_| None).collect(),
            channels: channels.into(),
            counters: program.counters.iter().map(|_| 0).collect(),
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
                Insn::Send(chan, variant, ref expr) => {
                    let values = expr.iter().map(|e| {
                        self.registers.down_eval(e)
                    }).collect();
                    self.channels[chan].send(ChannelMessage { variant, values });
                }
                Insn::Consume(chan, variant, ref exprs) => {
                    let Poll::Ready(rx) = self.channels[chan].poll_receive(cx) else { return };
                    let rx = rx.pop();
                    debug!("  Consume {rx:?}");
    
                    if rx.variant != variant {
                        self.failed = true;
                        return;
                    };
    
                    assert_eq!(rx.values.len(), exprs.len());
    
                    for ((p, id), v) in exprs.iter().zip(rx.values.into_iter()) {
                        if !self.registers.up_eval_test(p, &v) {
                            debug!("  failed to match");
                            self.failed = true;
                            return;
                        }
                        self.registers[*id] = Some(v);
                    };
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
                Insn::CounterReset(counter, val) => {
                    self.counters[counter] = val;
                }
                Insn::CounterUpEval(counter, min, max, id) => {
                    let v = self.counters[counter];
                    if v < min || v > max.unwrap_or(i64::MAX) {
                        self.failed = true;
                        return;
                    }
                    self.registers[id] = Some(v.into());
                }
                Insn::CounterDownEval(counter, ref expr) => {
                    let v = self.registers.down_eval(expr);
                    self.counters[counter] = i64::try_from(v).unwrap().try_into().unwrap();
                }
                Insn::CounterDecrement(counter, if_zero) => {
                    if self.counters[counter] == 0 {
                        next_ip = if_zero;
                    } else {
                        self.counters[counter] -= 1;
                    }
                }
                Insn::CounterIncrement(counter) => {
                    self.counters[counter] += 1;
                }
                Insn::Assign(l, ref r) => {
                    let v = self.registers.down_eval(r);
                    self.registers[l] = Some(v);
                }
                Insn::TestVal(ref e, ref pred, if_false) => {
                    let v = self.registers.down_eval(e);
                    if !self.registers.up_eval_test(pred, &v) {
                        next_ip = if_false;
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
