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
		/*PrimitiveStep(ref h) => {
			println!("{}{}", i, h.display());
			h.body().map(|body| {
				print_step_tree(body, indent+1)
			});
		}*/
	}
}

pub fn exec(s: &Step, parent: &comm::DuplexStream<Option<Value>, Option<Value>>) -> bool {
		match *s {
			NopStep => true,
			EventStep(id, ref tokName, ref args) => {
				// TODO: check name and id
				let &(ref down, ref up) = args.get(0);
				parent.send(down.const_down());
				up.const_up(parent.recv())
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
			//PrimitiveStep(..) => unimplemented!()
		}
}
