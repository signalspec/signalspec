use std::io::{ Write, Result as IoResult };

use exec::{Step, Message};
use eval::Expr;
use data::DataMode;
use session::ValueID;

pub type StateId = usize;
pub type CounterId = usize;

#[derive(Clone, Debug)]
pub struct Nfa {
    states: Vec<State>,
    initial: StateId,
    success: StateId,
}

impl Nfa {
    pub fn new() -> Nfa {
        let mut nfa = Nfa { states: vec![], initial: 0, success: 0 };
        nfa.initial = nfa.add_state();
        nfa.success = nfa.add_state();
        nfa
    }

    pub fn add_state(&mut self) -> StateId {
        let id = self.states.len() as StateId;
        self.states.push(State::new());
        id
    }

    pub fn add_transition(&mut self, from: StateId, to: StateId, action: Action) {
        self.states[from as usize].transitions.push(Transition { target: to, action: action});
    }

    pub fn to_graphviz(&self, f: &mut Write) -> IoResult<()> {
        try!(writeln!(f, "digraph G {{"));
        for (id, state) in self.states.iter().enumerate() {
            for transition in &state.transitions {
                let colorstr = match &transition.action {
                    &Action::Epsilon => r#"fontcolor="gray""#,
                    &Action::Lower(..) => r#"fontcolor="blue""#,
                    &Action::UpperBegin(..) | &Action::UpperEnd(..) => r#"fontcolor="green""#,
                    _ => ""
                };
                try!(writeln!(f, "{} -> {} [ label=\"{:?}\" {}];", id, transition.target, transition.action, colorstr));
            }
        }
        try!(writeln!(f, "}}"));
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct State {
    transitions: Vec<Transition>,
}

impl State {
    fn new() -> State {
        State { transitions: vec![] }
    }
}

#[derive(Clone, Debug)]
pub struct Transition {
    target: StateId,
    action: Action,
}

#[derive(Clone, Debug)]
pub enum Action {
    Epsilon,
    Lower(Message),
    UpperBegin(Message),
    UpperEnd(Message),

    RepeatDnInit(CounterId, Expr), // Down-evaluate count, initialize counter to zero
    RepeatDnBack(CounterId), // Increment counter, guard on counter < count
    RepeatDnExit(CounterId), // Guard on counter == count

    RepeatUpInit(CounterId), // initialize counter to zero
    RepeatUpBack(CounterId), // increment counter
    RepeatUpExit(CounterId, Expr), // Guard on up-evaluate count

    // Down-evaluate outer variables, set counter to zero
    ForInit(CounterId, u32, Vec<(ValueID, Expr, DataMode)>),
    // guard on counter < size, pop next inner variables down,
    ForEntry(CounterId, u32, Vec<(ValueID, Expr, DataMode)>),
    // push next outer variables up, increment
    ForBack(CounterId, u32, Vec<(ValueID, Expr, DataMode)>),
    // guard on counter == size, Up-evaluate outer variables
    ForExit(CounterId, u32, Vec<(ValueID, Expr, DataMode)>),
}

pub fn from_step_tree(s: &Step) -> Nfa {
    let mut nfa = Nfa::new();
    let initial = nfa.initial;
    let success = nfa.success;
    from_step_tree_inner(s, &mut nfa, initial, success);

    fn from_step_tree_inner(s: &Step, nfa: &mut Nfa, from: StateId, to: StateId) {
        match *s {
            Step::Nop => nfa.add_transition(from, to, Action::Epsilon),
            Step::Token(ref message) => {
                let m: Message = message.clone();
                nfa.add_transition(from, to, Action::Lower(m))
            }
            Step::TokenTop(ref message, box ref body) => {
                let is = nfa.add_state();
                let ie = nfa.add_state();
                nfa.add_transition(from, is, Action::UpperBegin(message.clone()));
                from_step_tree_inner(body, nfa, is, ie);
                nfa.add_transition(ie, to, Action::UpperEnd(message.clone()));
            }
            Step::Seq(ref steps) => {
                if let Some((last, rest)) = steps.split_last() {
                    let mut conn = from;
                    for c in rest.iter() {
                        let next = nfa.add_state();
                        from_step_tree_inner(c, nfa, conn, next);
                        conn = next;
                    }
                    from_step_tree_inner(last, nfa, conn, to);
                } else {
                    nfa.add_transition(from, to, Action::Epsilon);
                }
            }
            Step::Repeat(ref count, box ref inner, up) => {
                let start = nfa.add_state();
                let end = nfa.add_state();
                let counter = start;

                if up {
                    if count.ignored() {
                        nfa.add_transition(from, start, Action::Epsilon);
                        nfa.add_transition(start, end, Action::Epsilon);
                        nfa.add_transition(start, to, Action::Epsilon);
                    } else {
                        nfa.add_transition(from, start, Action::RepeatUpInit(counter));
                        nfa.add_transition(start, end, Action::RepeatUpBack(counter));
                        nfa.add_transition(start, to, Action::RepeatUpExit(counter, count.clone()));
                    }
                } else {
                    nfa.add_transition(from, start, Action::RepeatDnInit(counter, count.clone()));
                    nfa.add_transition(start, end, Action::RepeatDnBack(counter));
                    nfa.add_transition(start, to, Action::RepeatDnExit(counter));
                }
                from_step_tree_inner(inner, nfa, end, start);
            }
            Step::Foreach(width, ref vars, box ref inner) => {
                let entry = nfa.add_state();
                let start = nfa.add_state();
                let end   = nfa.add_state();
                let counter = entry;

                nfa.add_transition(from,  entry, Action::ForInit(counter, width, vars.clone()));
                nfa.add_transition(entry, start, Action::ForEntry(counter, width, vars.clone()));
                nfa.add_transition(end,   entry, Action::ForBack(counter, width, vars.clone()));
                nfa.add_transition(entry, to,    Action::ForExit(counter, width, vars.clone()));

                from_step_tree_inner(inner, nfa, start, end);
            }
        }
    }

    nfa
}
