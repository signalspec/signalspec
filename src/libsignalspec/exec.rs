use vcd::VCDWriter;

pub trait PrimitiveStep {
	fn display(&self) -> String;
	fn body<'a>(&'a self) -> Option<&'a Step> { None }
	fn exec(&self);
}

pub enum Step {
	NopStep,
	CallStep(Box<Step>),
	SeqStep(Vec<Step>),
	TimeStep(f64), //TODO: valueref
	SignalLevelStep(uint, bool, Box<Step>),
	PrimitiveStep(Box<PrimitiveStep>),
}

pub fn print_step_tree(s: &Step, indent: uint) {
	let i = " ".repeat(indent);
	match *s {
		NopStep => println!("{}NOP", i),
		CallStep(box ref c) => {
			println!("{}Call", i);
			print_step_tree(c, indent+1);
		}
		SeqStep(ref steps) => {
			println!("{}Seq", i)
			for c in steps.iter() {
				print_step_tree(c, indent+1);
			}
		}
		TimeStep(t) => {
			println!("{}Time", i)
		}
		SignalLevelStep(id, v, box ref body) => {
			println!("{}{} Level {}", i, id, v);
			print_step_tree(body, indent+1);
		}
		PrimitiveStep(ref h) => {
			println!("{}{}", i, h.display());
			h.body().map(|body| {
				print_step_tree(body, indent+1)
			});
		}
	}
}

pub fn exec_to_vcd(s: &Step, vcd: &mut VCDWriter) {
	match *s {
		NopStep => (),
		CallStep(box ref c) => {
			exec_to_vcd(c, vcd);
		}
		SeqStep(ref steps) => {
			for c in steps.iter() {
				exec_to_vcd(c, vcd);
			}
		}
		TimeStep(t) => {
			vcd.time((t*1.0e9) as u64);
		}
		SignalLevelStep(id, val, box ref body) => {
			vcd.value(id, val);
			exec_to_vcd(body, vcd);
		}
		PrimitiveStep(ref h) => {
			h.exec();
		}
	}
}

