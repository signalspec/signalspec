use std::comm;

use resolve::scope::ValueRef;
use resolve::signal::SignalId;
use ast::Value;

pub trait PrimitiveStep {
	fn display(&self) -> String;
	fn body<'a>(&'a self) -> Option<&'a Step> { None }
	fn exec(&self);
}

pub enum Step {
	NopStep,
	EventStep(SignalId, String, Vec<(ValueRef, ValueRef)>),
	SeqStep(Vec<Step>),
	RepeatStep(Box<Step>),
	//PrimitiveStep(Box<PrimitiveStep>),
}

pub fn print_step_tree(s: &Step, indent: uint) {
	let i = " ".repeat(indent);
	match *s {
		NopStep => println!("{}NOP", i),
		EventStep(id, ref s, ref args) => {
			println!("{}Event: {} {} {}", i, id, s, args);
		}
		SeqStep(ref steps) => {
			println!("{}Seq", i)
			for c in steps.iter() {
				print_step_tree(c, indent+1);
			}
		}
		RepeatStep(box ref inner) => {
			println!("{}Repeat:", i);
			print_step_tree(inner, indent + 1);
		}
		/*PrimitiveStep(ref h) => {
			println!("{}{}", i, h.display());
			h.body().map(|body| {
				print_step_tree(body, indent+1)
			});
		}*/
	}
}

pub struct Connection {
	rx: comm::Receiver<Option<Value>>,
	tx: comm::Sender<Option<Value>>,
	lookahead: Option<(Option<Value>, Option<Value>)>,
}

impl Connection {
	pub fn new() -> (Connection, Connection) {
		let (s1, r1) = comm::channel();
		let (s2, r2) = comm::channel();
		(Connection{ tx: s1, rx: r2, lookahead: None }, Connection{ tx: s2, rx: r1, lookahead: None })
	}
	
	pub fn send(&self, v: Option<Value>) -> Result<(), Option<Value>> {
		self.tx.send_opt(v)
	}
	
	pub fn recv(&self) -> Result<Option<Value>, ()> {
		self.rx.recv_opt()
	}
	
	pub fn apply(&mut self, tokName: &str, args: &[(ValueRef, ValueRef)]) -> bool {
		let &(ref down, ref up) = &args[0];
		
		let down_v = down.const_down();
		let received = match self.lookahead.take() {
			Some((sent, received)) => {
				if sent != down_v { fail!("Committed {}, but sending {}", sent, down_v); }
				received
			}
			None => {
				match self.send(down_v.clone()) {
					Ok(..) => {},
					Err(..) => return false,
				};
				match self.recv() {
					Ok(r) => r,
					Err(..) => return false,
				}
			}
		};
		
		if up.const_up(received.clone()) {
			true
		} else {
			self.lookahead = Some((down_v, received));
			false
		}
	}
}

pub fn exec(s: &Step, parent: &mut Connection) -> bool {
		match *s {
			NopStep => true,
			EventStep(id, ref tokName, ref args) => {
				parent.apply(tokName.as_slice(), args.as_slice())
			}
			SeqStep(ref steps) => {
				for c in steps.iter() {
					match exec(c, parent) {
						true => (),
						false => return false,
					}
				}
				true
			}
			RepeatStep(box ref inner) => {
				loop {
					if !exec(inner, parent) {
						break;
						// TODO: retry that event
					}
				}
				true
			}
			//PrimitiveStep(..) => unimplemented!()
		}
}
