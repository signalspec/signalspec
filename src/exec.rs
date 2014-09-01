use std::comm;

use resolve::scope::ValueRef;
use ast::Value;

pub trait PrimitiveStep {
	fn display(&self) -> String;
	fn body<'a>(&'a self) -> Option<&'a Step> { None }
	fn exec(&self);
}

pub enum Step {
	NopStep,
	TokenStep(/*down*/ ValueRef, /*up*/ ValueRef),
	SeqStep(Vec<Step>),
	RepeatStep(Box<Step>),
	//PrimitiveStep(Box<PrimitiveStep>),
}

fn first(s: &Step) -> Option<(&ValueRef, &ValueRef)> {
	match *s {
		NopStep => None,
		TokenStep(ref down, ref up) => Some((down, up)),
		SeqStep(ref steps) => steps.as_slice().get(0).and_then(first),
		RepeatStep(box ref inner) => first(inner),
	}
}

pub fn print_step_tree(s: &Step, indent: uint) {
	let i = " ".repeat(indent);
	match *s {
		NopStep => println!("{}NOP", i),
		TokenStep(ref down, ref up) => {
			println!("{}Token: {} {}", i, down, up);
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

	pub fn try(&mut self, down: &ValueRef, up: &ValueRef) -> bool {
		let down_v = down.const_down();

		let received = match self.lookahead.take() {
			Some((sent, received)) => {
				debug!("lookahead {}", received);
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

		debug!("recieved {}", received);
		let result = up.const_up(received.as_ref());
		self.lookahead = Some((down_v, received));
		result
	}


	pub fn apply(&mut self, down: &ValueRef, up: &ValueRef) -> bool {
		if self.try(down, up) {
			debug!("matched {}", up);
			self.lookahead.take();
			true
		} else {
			debug!("failed {}", up);
			false
		}
	}
}

pub fn exec(s: &Step, parent: &mut Connection) -> bool {
		match *s {
			NopStep => true,
			TokenStep(ref down, ref up) => {
				parent.apply(down, up)
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
				let (down, up) = first(inner).expect("Loop has no body");
				loop {
					if !parent.try(down, up) {
						break;
					}
					if !exec(inner, parent) {
						return false;
					}
				}
				true
			}
			//PrimitiveStep(..) => unimplemented!()
		}
}
