use std::{future::Future, pin::Pin, sync::Arc, task::{ready, Context, Poll}};

use futures_lite::FutureExt;
use itertools::Itertools;

use crate::{core::{ChannelId, Derivatives, ExprDn, Predicate, ProcId, ProcessChain, StepId, ValueSrcId}, entitymap::EntityMap, Value};

use super::{channel::SeqChannels, Channel, ChannelMessage};

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

pub struct FsmExec {
    program: Arc<ProcessChain>,
    channels: EntityMap<ChannelId, Channel>,
    state: StepId,
    registers: EntityMap<ValueSrcId, Vec<Value>>,
    processes: EntityMap<ProcId, Option<Pin<Box<dyn Future<Output = Result<(), ()>>>>>>,
}

impl FsmExec {
    pub fn new(program: Arc<ProcessChain>, channels_lo: SeqChannels, channels_hi: SeqChannels) -> FsmExec {
        let mut channels: EntityMap<ChannelId, Channel> = program.connections.iter().flat_map(|_| [Channel::new(), Channel::new()]).collect::<Vec<_>>().into();

        if let Some(c) = channels_lo.dn { channels[program.conn_dn.dn()] = c; }
        if let Some(c) = channels_lo.up { channels[program.conn_dn.up()] = c; }
        if let Some(c) = channels_hi.dn { channels[program.up.as_ref().unwrap().1.dn()] = c; }
        if let Some(c) = channels_hi.up { channels[program.up.as_ref().unwrap().1.up()] = c; }

        FsmExec {
            state: program.root,
            registers: program.vars.iter().map(|_| Vec::new()).collect(),
            processes: program.processes.iter().map(|_| None).collect(),
            program,
            channels,
        }
    }

    pub(crate) fn poll(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), ()>> {
        loop {
            if self.state == StepId::ACCEPT {
                return Poll::Ready(Ok(()));
            } else if self.state == StepId::FAIL {
                return Poll::Ready(Err(()));
            }

            self.state = ready!(poll_once(
                cx,
                self.state,
                self.program.fsm.get(&self.state).expect("missing state"),
                &self.program,
                &mut self.registers,
                &mut self.channels,
                &mut self.processes,
            ));
        }
    }
}

fn poll_once(
    cx: &mut Context<'_>,
    state: StepId,
    d: &Derivatives,
    program: &ProcessChain,
    registers: &mut EntityMap<ValueSrcId, Vec<Value>>,
    channels: &mut EntityMap<ChannelId, Channel>,
    processes: &mut EntityMap<ProcId, Option<Pin<Box<dyn Future<Output = Result<(), ()>>>>>>,
) -> Poll<StepId> {
    let stateno: u32 = state.into();
    Poll::Ready(match *d {
        Derivatives::End => {
            let accepting = program.accepting.contains(&state);
            debug!("{stateno:6} End {accepting:?}");
            if accepting { StepId::ACCEPT } else { StepId::FAIL }
        }
        Derivatives::Send { chan, variant, ref dn, next } => {
            let values = dn.iter().map(|e| {
                registers.down_eval(e)
            }).collect();
            debug!("{stateno:6} Send {chan:?} {variant} {values:?}");
            channels[chan].send(ChannelMessage { variant, values });
            next
        }
        Derivatives::Receive { chan, ref arms, other } => {
            let rx = ready!(channels[chan].poll_receive(cx));
            let msg = rx.peek();

            let arm = arms.iter().find(|arm| {
                msg.variant == arm.variant &&
                arm.predicates.iter().zip_eq(&msg.values).all(|(p, v)| {
                    registers.up_eval_test(p, v)
                })
            });

            if let Some(arm) = arm {
                debug!("{stateno:6} Receive {chan:?} {msg:?} - accept");
                registers[arm.var] = rx.pop().values;
                arm.next
            } else if msg.variant == 0 && program.accepting.contains(&state) {
                debug!("{stateno:6} Receive {chan:?} {msg:?} - end");
                StepId::ACCEPT
            } else {
                debug!("{stateno:6} Receive {chan:?} {msg:?} - skip");
                other
            }
        }
        Derivatives::Process { id, next, err } => {
            let fut = processes[id].get_or_insert_with(|| {
                debug!("{stateno:6} Process {id:?} - start");
                let def = &program.processes[id];
                let ch = def.channels.iter()
                    .map(|&cid| channels[cid].clone())
                    .collect();
                def.func.run(ch)
            });
            let r = ready!(fut.poll(cx));
            debug!("{stateno:6} Process {id:?} - end {r:?}");
            drop(processes[id].take());
            match r {
                Ok(()) => next,
                Err(()) => err,
            }
        }
        Derivatives::Assign { var, ref val, next } => {
            let x = vec![registers.down_eval(val)];
            debug!("{stateno:6} Assign {var:?} = {x:?}");
            registers[var] = x;
            next
        }
        Derivatives::Switch { ref src, ref arms, other } => {
            let vals = src.iter().map(|e| registers.down_eval(e)).collect();
            debug!("{stateno:6} Switch on {vals:?}");

            let arm = arms.iter().find(|arm| {
                arm.predicates.iter().zip_eq(&vals).all(|(p, v)| {
                    registers.up_eval_test(p, v)
                })
            });

            if let Some(arm) = arm {
                if let Some(var) = arm.var {
                    registers[var] = vals;
                }
            }

            arm.map_or(other, |arm| arm.next)
        }
        Derivatives::Select { ref branches, ref end } => {
            for ch in end {
                channels[*ch].end(true);
            }
            for b in branches {
                match poll_once(cx, state, b, program, registers, channels, processes) {
                    Poll::Ready(next) => {
                        for ch in end {
                            channels[*ch].end(false);
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
