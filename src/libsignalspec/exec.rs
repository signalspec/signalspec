
pub trait PrimitiveStep {
	fn display(&self) -> ~str;
	fn body<'a>(&'a self) -> Option<&'a Step> { None }
	fn exec(&self);
}

pub enum Step {
	NopStep,
	CallStep(~Step),
	SeqStep(~[Step]),
	PrimitiveStep(~PrimitiveStep),
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

pub fn exec(s: &Step) {
	match *s {
		NopStep => (),
		CallStep(~ref c) => {
			exec(c);
		}
		SeqStep(ref steps) => {
			for c in steps.iter() {
				exec(c);
			}
		}
		PrimitiveStep(ref h) => {
			h.exec();
		}
	}
}

