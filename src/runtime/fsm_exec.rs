use std::{future::Future, pin::Pin, sync::Arc, task::{ready, Context, Poll}};

use futures_lite::FutureExt;
use itertools::Itertools;

use crate::{core::{ChannelId, Derivatives, ExprDnId, Predicate, ProcId, ProcessChain, StepId, ValueSrcId}, entitymap::EntityMap, Value};

use super::{channel::SeqChannels, Channel, ChannelMessage};

pub struct FsmExec {
    program: Arc<ProcessChain>,
    state: FsmState,
}
struct FsmState {
    state: StepId,
    channels: EntityMap<ChannelId, Channel>,
    registers: EntityMap<ValueSrcId, Vec<Value>>,
    processes: EntityMap<ProcId, Option<Pin<Box<dyn Future<Output = Result<(), ()>>>>>>,
}

struct FsmBorrow<'a> {
    program: &'a ProcessChain,
    state: &'a mut FsmState,
}

impl FsmExec {
    pub fn new(program: Arc<ProcessChain>, channels_lo: SeqChannels, channels_hi: SeqChannels) -> FsmExec {
        let mut channels: EntityMap<ChannelId, Channel> = program.connections.iter().flat_map(|_| [Channel::new(), Channel::new()]).collect::<Vec<_>>().into();

        if let Some(c) = channels_lo.dn { channels[program.conn_dn.dn()] = c; }
        if let Some(c) = channels_lo.up { channels[program.conn_dn.up()] = c; }
        if let Some(c) = channels_hi.dn { channels[program.up.as_ref().unwrap().1.dn()] = c; }
        if let Some(c) = channels_hi.up { channels[program.up.as_ref().unwrap().1.up()] = c; }

        FsmExec {
            state: FsmState {
                state: program.root,
                registers: program.vars.iter().map(|_| Vec::new()).collect(),
                processes: program.processes.iter().map(|_| None).collect(),
                channels,
            },
            program,
        }
    }

    fn borrow(&mut self) -> FsmBorrow<'_> {
        FsmBorrow {
            program: &self.program,
            state: &mut self.state,
        }
    }

    pub(crate) fn poll(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), ()>> {
        self.borrow().poll(cx)
    }
}

impl FsmBorrow<'_> {
    fn down_eval(&self, expr: ExprDnId) -> Value {
        self.program.exprs.eval(expr, &|var| {
            self.state.registers[var.0][var.1 as usize].clone()
        })
    }

    fn up_eval_test(&self, p: &Predicate, v: &Value) -> bool {
        p.test(v)
    }

    pub(crate) fn poll(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), ()>> {
        loop {
            if self.state.state == StepId::ACCEPT {
                return Poll::Ready(Ok(()));
            } else if self.state.state == StepId::FAIL {
                return Poll::Ready(Err(()));
            }

            let d = self.program.fsm.get(&self.state.state)
                .expect("missing state");
            self.state.state = ready!(self.poll_one(cx, d));
        }
    }

    fn poll_one(
        &mut self,
        cx: &mut Context<'_>,
        d: &Derivatives,
    ) -> Poll<StepId> {
        let stateno: u32 = self.state.state.into();
        Poll::Ready(match *d {
            Derivatives::End => {
                let accepting = self.program.accepting.contains(&self.state.state);
                debug!("{stateno:6} End {accepting:?}");
                if accepting { StepId::ACCEPT } else { StepId::FAIL }
            }
            Derivatives::Send { chan, variant, ref dn, next } => {
                let values = dn.iter().copied().map(|e| {
                    self.down_eval(e)
                }).collect();
                debug!("{stateno:6} Send {chan:?} {variant} {values:?}");
                self.state.channels[chan].send(ChannelMessage { variant, values });
                next
            }
            Derivatives::Receive { chan, ref arms, other } => {
                let rx = ready!(self.state.channels[chan].poll_receive(cx));
                let msg = rx.peek();
    
                let arm = arms.iter().find(|arm| {
                    msg.variant == arm.variant &&
                    arm.predicates.iter().zip_eq(&msg.values).all(|(p, v)| {
                        self.up_eval_test(p, v)
                    })
                });
    
                if let Some(arm) = arm {
                    debug!("{stateno:6} Receive {chan:?} {msg:?} - accept");
                    self.state.registers[arm.var] = rx.pop().values;
                    arm.next
                } else if msg.variant == 0 && self.program.accepting.contains(&self.state.state) {
                    debug!("{stateno:6} Receive {chan:?} {msg:?} - end");
                    StepId::ACCEPT
                } else {
                    debug!("{stateno:6} Receive {chan:?} {msg:?} - skip");
                    other
                }
            }
            Derivatives::Process { id, next, err } => {
                let fut = self.state.processes[id].get_or_insert_with(|| {
                    debug!("{stateno:6} Process {id:?} - start");
                    let def = &self.program.processes[id];
                    let ch = def.channels.iter()
                        .map(|&cid| self.state.channels[cid].clone())
                        .collect();
                    def.func.run(ch)
                });
                let r = ready!(fut.poll(cx));
                debug!("{stateno:6} Process {id:?} - end {r:?}");
                drop(self.state.processes[id].take());
                match r {
                    Ok(()) => next,
                    Err(()) => err,
                }
            }
            Derivatives::Assign { var, val, next } => {
                let x = vec![self.down_eval(val)];
                debug!("{stateno:6} Assign {var:?} = {x:?}");
                self.state.registers[var] = x;
                next
            }
            Derivatives::Switch { ref src, ref arms, other } => {
                let vals = src.iter().copied().map(|e| self.down_eval(e)).collect();
                debug!("{stateno:6} Switch on {vals:?} {arms:?} {other:?}");
    
                let arm = arms.iter().find(|arm| {
                    arm.predicates.iter().zip_eq(&vals).all(|(p, v)| {
                        self.up_eval_test(p, v)
                    })
                });
    
                if let Some(arm) = arm {
                    if let Some(var) = arm.var {
                        self.state.registers[var] = vals;
                    }
                }
    
                arm.map_or(other, |arm| arm.next)
            }
            Derivatives::Select { ref branches, ref end } => {
                for ch in end {
                    self.state.channels[*ch].end(true);
                }
                for b in branches {
                    match self.poll_one(cx, b) {
                        Poll::Ready(next) => {
                            for ch in end {
                                self.state.channels[*ch].end(false);
                            }
                            return Poll::Ready(next);
                        }
                        Poll::Pending => continue,
                    }
                }
                debug!("{stateno:6} Select - waiting");
                return Poll::Pending;
            }
        })
    }
}

