
pub trait StepHandler {
	fn display(&self) -> ~str {
		~"Unknown Primitive"
	}

	fn body<'a>(&'a self) -> Option<&'a Step> { None }
}

pub enum Step {
	NopStep,
	CallStep(~Step),
	SeqStep(~[Step]),
	PrimitiveStep(~StepHandler),
}

pub fn print_step_tree(s: &Step, indent: uint) {
	let i = " ".repeat(indent);
	match *s {
		NopStep => println!("{}NOP", i),
		CallStep(~ref c) => {
			println!("{}Call", i);
			print_step_tree(c, indent+1);
		}
		SeqStep(ref steps) => {
			println!("{}Seq", i)
			for c in steps.iter() {
				print_step_tree(c, indent+1);
			}
		}
		PrimitiveStep(ref h) => {
			println!("{}{}", i, h.display());
			h.body().map(|body| {
				print_step_tree(body, indent+1)
			});
		}
	}
}

