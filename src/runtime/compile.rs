
use crate::{core::{ExprDn, Expr, ProcessChain, MatchSet, MessagePatternSet, StepId, Step, ValueId, Dir}, Shape, PrimitiveProcess};

type TaskId = usize;
type InsnId = usize;
type ChanId = usize;
type VariantId = usize;
type CounterId = usize;
type PrimitiveId = usize;

#[derive(Debug)]
pub enum Insn {
    /// Down-evaluate a message and send it on the specified channel
    Send(ChanId, VariantId, Vec<ExprDn>),

    /// Block waiting for data ready on a channel
    Receive(ChanId),

    /// Consume received message from channel and up-evaluate it into expr
    Consume(ChanId, VariantId, Vec<Expr>),

    /// Test received message from channel by up-evaluating expression, without
    /// consuming the message or writing variables. If not matched, jump.
    Test(ChanId, MessagePatternSet, InsnId),

    ChannelOpen(ChanId),
    ChannelClose(ChanId),

    Primitive(PrimitiveId, Vec<ChanId>),

    CounterReset(CounterId, u32),
    CounterUpEval(CounterId, Expr),
    CounterDownEval(CounterId, ExprDn),
    CounterDecrement(CounterId, InsnId),
    CounterIncrement(CounterId),

    IterDnStart(ValueId, ExprDn),
    IterDnStep(ValueId),
    IterUpStep(ValueId),
    IterUpExit(ValueId, Expr),

    Assign(Expr, ExprDn),
    TestAssign(Expr, ExprDn, InsnId),

    /// Start a new thread of exxecution
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
struct Channels {
    dn_tx: Option<ChanId>,
    dn_rx: Option<ChanId>,
    up_tx: Option<ChanId>, 
    up_rx: Option<ChanId>,
}

impl Channels {
    fn without_up(self) -> Channels {
        Self { up_rx: None, up_tx: None, ..self }
    }

    fn to_vec(self) -> Vec<ChanId> {
        [self.dn_tx, self.dn_rx, self.up_tx, self.up_rx].into_iter().flatten().collect()
    }
}

struct Compiler<'a> {
    program: &'a ProcessChain,
    insns: Vec<Insn>,
    tasks: Vec<()>,
    counters: Vec<()>,
    channels: Vec<()>,
    primitives: Vec<Arc<dyn PrimitiveProcess + 'static>>,
}

impl Compiler<'_> {
    fn next_ip(&self) -> usize {
        self.insns.len()
    }

    fn emit(&mut self, i: Insn) {
        self.insns.push(i)
    }

    fn emit_placeholder(&mut self) -> InsnId {
        let ip = self.next_ip();
        self.emit(Insn::Fail);
        ip
    }

    fn backpatch(&mut self, ip: InsnId, i: Insn) {
        self.insns[ip] = i;
    }

    fn new_task(&mut self) -> TaskId {
        let id = self.tasks.len();
        self.tasks.push(());
        id
    }

    fn new_counter(&mut self) -> CounterId {
        let id = self.counters.len();
        self.counters.push(());
        id
    }

    fn new_channel(&mut self) -> ChanId {
        let id = self.channels.len();
        self.channels.push(());
        id
    }

    fn new_channels(&mut self, shape: &Shape) -> (Option<ChanId>, Option<ChanId>) {
        let dir = shape.direction();
        (dir.down.then(|| self.new_channel()), dir.up.then(|| self.new_channel()))
    }

    fn new_primitive(&mut self, p: Arc<dyn PrimitiveProcess>) -> PrimitiveId {
        let id = self.primitives.len();
        self.primitives.push(p);
        id
    }

    fn seq_prep(&mut self, channels: Channels, prev_followlast: &Option<MatchSet>, next_first: &MatchSet) {
        match (prev_followlast, next_first) {
            (None, MatchSet::MessageUp { .. }) => {
                if let Some(c) = channels.up_rx {
                    self.emit(Insn::Receive(c))
                }
            },
            (None, MatchSet::MessageDn { variant, send, .. }) => {
                if let Some(c) = channels.dn_tx {
                    self.emit(Insn::Send(c, *variant, send.clone()));
                }

                if let Some(c) = channels.dn_rx {
                    self.emit(Insn::Receive(c))
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

    fn test_matchset(&self, channels: Channels, m: &MatchSet, if_false: InsnId) -> Insn {
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

    fn compile_step(&mut self, step: StepId, channels: Channels) {
        match self.program.steps[step.0 as usize] {
            Step::Stack { lo, ref shape, hi} => {
                let lo_info = &self.program.step_info[lo.0 as usize];
                let hi_info = &self.program.step_info[hi.0 as usize];
                let (c_dn, c_up) = self.new_channels(shape);

                if let Some(c) = c_up {
                    self.emit(Insn::ChannelOpen(c));
                }

                if let Some(c) = c_dn {
                    self.emit(Insn::ChannelOpen(c));
                }

                let i = self.emit_placeholder();
                let task = self.new_task();

                let channels_hi = Channels {
                    dn_tx: c_dn,
                    dn_rx: c_up,
                    up_tx: channels.up_tx,
                    up_rx: channels.up_rx,
                };
                self.seq_prep(channels_hi, &None, &hi_info.first);
                self.compile_step(hi, channels_hi);
                if let Some(c) = c_dn {
                    self.emit(Insn::ChannelClose(c));
                }
                self.emit(Insn::End);

                self.backpatch(i, Insn::Fork(task, self.next_ip()));

                let channels_lo = Channels {
                    dn_tx: channels.dn_tx,
                    dn_rx: channels.dn_rx,
                    up_tx: c_up,
                    up_rx: c_dn,
                };
                self.seq_prep(channels_lo, &None, &lo_info.first);
                self.compile_step(lo, channels_lo);
                if let Some(c) = c_up {
                    self.emit(Insn::ChannelClose(c));
                }
                self.emit(Insn::Join(task));
            }

            Step::Primitive(ref p) => {
                let id = self.new_primitive(p.clone());
                let chans = channels.to_vec();
                self.emit(Insn::Primitive(id, chans));
            }

            Step::Token { variant, ref receive, ..}=> {
                if let Some(c) = channels.dn_rx {
                    self.emit(Insn::Consume(c, variant, receive.clone()));
                }
            }

            Step::TokenTop { top_dir, variant, ref send, ref receive, inner } => {
                let inner_info = &self.program.step_info[inner.0 as usize];

                match top_dir {
                    Dir::Up => {},
                    Dir::Dn => {
                        if let Some(c) = channels.up_rx {
                            self.emit(Insn::Consume(c, variant, send.clone()));
                        }

                        self.seq_prep(channels.without_up(), &None, &inner_info.first);
                    },
                }

                self.compile_step(inner, channels.without_up());

                if let Some(c) = channels.up_tx {
                    self.emit(Insn::Send(c, variant, receive.clone()));
                }
            }

            Step::Seq(ref steps) => {
                let mut prev_followlast = None;
                for &step in steps {
                    let inner_info = &self.program.step_info[step.0 as usize];

                    if let Some(prev_followlast) = prev_followlast {
                        self.seq_prep(channels, prev_followlast, &inner_info.first);
                    }

                    self.compile_step(step, channels);
                    prev_followlast = Some(&inner_info.followlast);
                }
            }

            Step::RepeatUp(ref count, inner) => {
                let inner_info = &self.program.step_info[inner.0 as usize];
                let counter = self.new_counter();

                self.emit(Insn::CounterReset(counter, 0));
                let test_ip = self.emit_placeholder();
                self.compile_step(inner, channels);
                self.seq_prep(channels, &inner_info.followlast, &inner_info.first);
                self.emit(Insn::CounterIncrement(counter));
                self.emit(Insn::Jump(test_ip));

                self.backpatch(test_ip, self.test_matchset(channels, &inner_info.first, self.next_ip()));
                self.emit(Insn::CounterUpEval(counter, count.clone()));
            }

            Step::RepeatDn(ref count, inner) => {
                let inner_info = &self.program.step_info[inner.0 as usize];
                let counter = self.new_counter();
                self.emit(Insn::CounterDownEval(counter, count.clone()));
                let test_ip = self.emit_placeholder();

                self.seq_prep(channels, &None, &inner_info.first);
                self.compile_step(inner, channels);
                self.emit(Insn::Jump(test_ip));

                self.backpatch(test_ip, Insn::CounterDecrement(counter, self.next_ip()));
            }

            Step::Foreach { iters, ref vars_dn, ref vars_up, inner} => {
                assert!(iters > 0);

                let inner_info = &self.program.step_info[inner.0 as usize];

                let counter = self.new_counter();
                self.emit(Insn::CounterReset(counter, iters - 1)); // tested at end of loop

                for &(id, ref e) in vars_dn {
                    self.emit(Insn::IterDnStart(id, e.clone()));
                }

                let loop_head = self.next_ip();

                for &(id, _) in vars_dn {
                    self.emit(Insn::IterDnStep(id));
                }

                if !vars_dn.is_empty() {
                    self.seq_prep(channels, &None, &inner_info.first);
                }

                self.compile_step(inner, channels);

                for &(id, _) in vars_up {
                    self.emit(Insn::IterUpStep(id));
                }

                let exit_check = self.emit_placeholder();

                if vars_dn.is_empty() {
                    self.seq_prep(channels, &inner_info.followlast, &inner_info.first);
                }

                self.emit(Insn::Jump(loop_head));

                self.backpatch(exit_check, Insn::CounterDecrement(counter, self.next_ip()));

                for &(id, ref e) in vars_up {
                    self.emit(Insn::IterUpExit(id, e.clone()));
                }
            }

            Step::AltUp(ref opts) => {
                let mut jumps_to_final = vec![];

                for &(ref vals, inner) in opts {
                    let inner_info = &self.program.step_info[inner.0 as usize];
                    let test = self.emit_placeholder();

                    self.compile_step(inner, channels);

                    for &(ref l, ref r) in vals {
                        self.emit(Insn::Assign(r.clone(), l.clone()));
                    }

                    jumps_to_final.push(self.emit_placeholder());

                    self.backpatch(test, self.test_matchset(channels, &inner_info.first, self.next_ip()));
                }

                self.emit(Insn::Fail);

                for ip in jumps_to_final {
                    self.backpatch(ip, Insn::Jump(self.next_ip()));
                }
            }

            Step::AltDn(ref opts) => {
                let mut jumps_to_final = vec![];

                for &(ref vals, inner) in opts {
                    let inner_info = &self.program.step_info[inner.0 as usize];
                    let jumps_to_next: Vec<_> = vals.iter().map(|_| self.emit_placeholder()).collect();

                    self.seq_prep(channels, &None, &inner_info.first);
                    self.compile_step(inner, channels);

                    jumps_to_final.push(self.emit_placeholder());

                    for (&test_ip, &(ref l, ref r)) in jumps_to_next.iter().zip(vals) {
                        self.backpatch(test_ip, Insn::TestAssign(l.clone(), r.clone(), self.next_ip()));
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
    insns: Vec<Insn>,
    tasks: Vec<()>,
    counters: Vec<()>,
    channels: Vec<()>,
    primitives: Vec<Arc<dyn PrimitiveProcess>>,
}

pub fn compile(program: &ProcessChain) -> CompiledProgram {
    let mut compiler = Compiler {
        program,
        insns: Vec::new(),
        counters: Vec::new(),
        tasks: Vec::new(),
        channels: Vec::new(),
        primitives: Vec::new(),
    };

    let _root_task = compiler.new_task();

    let dn_dir = program.shape_dn.direction();
    let up_dir = program.shape_up.as_ref().map(|x| x.direction());

    let channels = Channels {
        dn_tx: dn_dir.down.then(|| compiler.new_channel()),
        dn_rx: dn_dir.up.then(|| compiler.new_channel()),
        up_tx: up_dir.map_or(false, |d| d.up).then(|| compiler.new_channel()),
        up_rx: up_dir.map_or(false, |d| d.down).then(|| compiler.new_channel()),
    };

    compiler.compile_step(program.root, channels);
    compiler.emit(Insn::End);

    for (ip, insn) in compiler.insns.iter().enumerate() {
        debug!("{ip:4} {insn:?}");
    }

    CompiledProgram {
        insns: compiler.insns,
        tasks: compiler.tasks,
        counters: compiler.counters,
        channels: compiler.channels,
        primitives: compiler.primitives,
    }
}

use std::{task::{Context, Poll}, sync::Arc, future::Future, pin::Pin};
use futures_lite::{ready, FutureExt};
use vec_map::VecMap;
use crate::Value;
use super::channel::{Channel, ChannelMessage};

pub struct ProgramExec {
    program: Arc<CompiledProgram>,
    tasks: Vec<InsnId>,
    registers: VecMap<Value>,
    channels: Vec<Channel>,
    counters: Vec<u32>,
    iters: VecMap<Vec<Value>>,
    primitives: Vec<Option<Pin<Box<dyn Future<Output = Result<(), ()>>>>>>,
}

impl ProgramExec {
    pub fn new(program: Arc<CompiledProgram>, mut channels: Vec<Channel>) -> ProgramExec {
        channels.resize_with(program.channels.len(), || Channel::new());

        let mut tasks: Vec<_> = program.tasks.iter().map(|_| !0).collect();
        tasks[0] = 0;

        ProgramExec {     
            tasks,
            registers: VecMap::new(),
            channels,
            counters: program.counters.iter().map(|_| 0).collect(),
            iters: VecMap::with_capacity(program.counters.len()),
            primitives: program.primitives.iter().map(|_| None).collect(),
            program,
        }
    }

    fn poll_all(&mut self, cx: &mut Context) -> Poll<Result<(), ()>> {
        for task in 0..self.tasks.len() {
            if self.tasks[task] != !0 {
                match self.poll_task(cx, task) {
                    Poll::Ready(Ok(_)) | Poll::Pending => {},
                    Poll::Ready(Err(_)) => return Poll::Ready(Err(())),
                }
            }
        }
    
        if self.tasks[0] != !0 {
            Poll::Pending
        } else {
            Poll::Ready(Result::Ok(()))
        }
    }
    
    fn poll_task(&mut self, cx: &mut Context, task: TaskId) -> Poll<Result<(), ()>> {
        let p = &*self.program;
        loop {
            let ip = self.tasks[task];
            let mut next_ip = ip + 1;
            debug!("Task {task:3}: {ip:4} {insn:?}", insn=p.insns[ip]);
            match p.insns[ip] {
                Insn::Send(chan, variant, ref expr) => {
                    let values = expr.iter().map(|e| {
                        e.eval(&mut |var| self.registers[var].clone())
                    }).collect();
                    self.channels[chan].send(ChannelMessage { variant, values });
                }
                Insn::Receive(chan) => {
                    ready!(self.channels[chan].poll_receive(cx));
                }
                Insn::Consume(chan, variant, ref exprs) => {
                    let mut rx = self.channels[chan].read();

                    if rx.is_end() {
                        debug!("  consume at end");
                        return Poll::Ready(Err(()))
                    }
                    
                    let rx = rx.pop().unwrap();
                    debug!("  Consume {rx:?}");
    
                    if rx.variant != variant {
                        return Poll::Ready(Err(()));
                    };
    
                    assert_eq!(rx.values.len(), exprs.len());
    
                    let matched = exprs.iter().zip(rx.values.into_iter()).all(|(e, v)| {
                        e.eval_up(&mut |var, val| { self.registers.insert(var, val); true }, v)
                    });
    
                    if !matched {
                        debug!("  failed to match");
                        return Poll::Ready(Err(()))
                    }
                }
                Insn::Test(chan, ref pat, if_false) => {
                    let read = self.channels[chan].read();

                    let matched = if read.is_end() {
                        false
                    } else {
                        let rx = read.peek().unwrap();
    
                        pat.iter().any(|p| {
                            p.variant == rx.variant && p.fields.iter().zip(rx.values.iter()).all(|(e, v)| {
                                e.eval_up(&mut |_, _| true, v.clone())
                            })
                        })
                    };

                    if !matched {
                        next_ip = if_false;
                    }
                }
                Insn::ChannelOpen(ch) => {
                    self.channels[ch].set_closed(false)
                }
                Insn::ChannelClose(ch) => {
                    self.channels[ch].set_closed(true)
                }
                Insn::Primitive(id, ref channels) => {
                    let ch = channels.iter().map(|&id| self.channels[id].clone()).collect();
                    let fut = self.primitives[id].get_or_insert_with(|| p.primitives[id].run(ch));
                    match ready!(fut.poll(cx)) {
                        Ok(_) => drop(self.primitives[id].take()),
                        Err(_) => return Poll::Ready(Err(())),
                    }
                }
                Insn::CounterReset(counter, val) => {
                    self.counters[counter] = val;
                }
                Insn::CounterUpEval(counter, ref expr) => {
                    let v = Value::Integer(self.counters[counter].into());
                    if !expr.eval_up(&mut |var, val| { self.registers.insert(var, val); true }, v) {
                        return Poll::Ready(Err(()))
                    }
                }
                Insn::CounterDownEval(counter, ref expr) => {
                    let v = expr.eval(&mut |var| self.registers[var].clone());
    
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
                Insn::IterDnStart(var, ref expr) => {
                    let v =  expr.eval(&mut |var| self.registers[var].clone());
                    let mut v = Vec::<Value>::try_from(v).unwrap();
                    v.reverse();
                    self.iters.insert(var, v);
                }
                Insn::IterDnStep(var) => {
                    let v = self.iters[var].pop().unwrap();
                    self.registers.insert(var, v);
                }
                Insn::IterUpStep(var) => {
                    let v = self.registers.remove(var).unwrap();
                    self.iters.entry(var).or_insert_with(|| Vec::new()).push(v);
                }
                Insn::IterUpExit(var, ref expr) => {
                    let v = self.iters.remove(var).unwrap();
                    expr.eval_up(&mut |var, val| { self.registers.insert(var, val); true }, Value::Vector(v));
                }
                Insn::Assign(ref l, ref r) => {
                    let v = r.eval(&mut |var| self.registers[var].clone());
                    if !l.eval_up(&mut |var, val| { self.registers.insert(var, val); true }, v) {
                        return Poll::Ready(Err(()));
                    }
                }
                Insn::TestAssign(ref l, ref r, if_false) => {
                    let v = r.eval(&mut |var| self.registers[var].clone());
                    if !l.eval_up(&mut |var, val| { self.registers.insert(var, val); true }, v) {
                        next_ip = if_false;
                    }
                }
                Insn::Fork(task, dest) => {
                    self.tasks[task] = next_ip;
                    next_ip = dest;
                }
                Insn::Join(task) => {
                    if self.tasks[task] != !0 {
                        return Poll::Pending;
                    }
                }
                Insn::End => {
                    self.tasks[task] = !0;
                    cx.waker().wake_by_ref(); // Wake the joining task
                    return Poll::Ready(Ok(()));
                }
                Insn::Jump(dest) => {
                    next_ip = dest;
                }
                Insn::Fail => {
                    return Poll::Ready(Err(()));
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
