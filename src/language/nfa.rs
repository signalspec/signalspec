use std::io::{ Write, Result as IoResult };
use std::collections::HashSet;
use std::mem::replace;
use std::fmt;

use super::step::{ Step, Message };
use super::eval::Expr;
use data::{ Value, DataMode };
use session::ValueID;

pub type StateId = usize;
pub type CounterId = usize;

#[derive(Clone, Debug)]
pub struct Nfa {
    pub states: Vec<State>,
    pub initial: HashSet<StateId>,
    pub accepting: HashSet<StateId>,
}

impl Nfa {
    pub fn new() -> Nfa {
        Nfa { states: vec![], initial: HashSet::new(), accepting: HashSet::new() }
    }

    pub fn add_state(&mut self) -> StateId {
        let id = self.states.len() as StateId;
        self.states.push(State::new());
        id
    }

    pub fn add_transition(&mut self, from: StateId, to: StateId, action: Action) {
        self.states[from as usize].transitions.push(Transition { target: to, action: action});
    }

    pub fn count_incoming_transitions(&self) -> Vec<usize> {
        let mut counts: Vec<usize> = self.states.iter().map(|_| 0).collect();
        for state in &self.states {
            for transition in &state.transitions {
                counts[transition.target] += 1;
            }
        }
        counts
    }

    /// Remove epsilon transitions without duplicating non-epsilon transitions
    pub fn remove_useless_epsilons(&mut self) {
        let incoming_counts = self.count_incoming_transitions();

        for src in 0..self.states.len() {
            let mut new_transitions = vec![];
            for mut transition in replace(&mut self.states[src].transitions, vec![]) {
                if incoming_counts[transition.target] == 1 {
                    let target_transitions = &mut self.states[transition.target].transitions;
                    if transition.is_epsilon() {
                        // If the only transition into a state is an epsilon, replace the epsilon
                        // with the state's outgoing transitions
                        new_transitions.append(target_transitions);

                        if self.accepting.remove(&transition.target) {
                            self.accepting.insert(src);
                        }

                        continue;
                    } else if target_transitions.len() == 1 && target_transitions[0].is_epsilon() {
                        // If there's only one transition into a state whose only outgoing
                        // transition is epsilon, skip the state and go to the outgoing
                        // transition's target directly
                        if self.accepting.remove(&transition.target) {
                            self.accepting.insert(target_transitions[0].target);
                        }

                        transition.target = target_transitions[0].target;
                        target_transitions.clear();
                    }
                }
                new_transitions.push(transition);
            }
            self.states[src].transitions = new_transitions;
        }

        let mut new_initial = HashSet::new();
        for &i in &self.initial {
            let transitions = &mut self.states[i].transitions;
            if incoming_counts[i] == 0 && transitions.iter().all(|x| x.is_epsilon()) {
                for t in transitions.drain(..) {
                    new_initial.insert(t.target);
                }
            } else {
                new_initial.insert(i);
            }
        }
        self.initial = new_initial;
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
                try!(writeln!(f, "{} -> {} [ label=\"{}\" {}];", id, transition.target, transition.action, colorstr));
            }
        }

        for initial in &self.initial {
            try!(writeln!(f, "start -> {};", initial));
        }

        for accepting in &self.accepting {
            try!(writeln!(f, "{} -> end;", accepting));
        }

        try!(writeln!(f, "}}"));
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct State {
    pub transitions: Vec<Transition>,
}

impl State {
    fn new() -> State {
        State { transitions: vec![] }
    }
}

#[derive(Clone, Debug)]
pub struct Transition {
    pub target: StateId,
    pub action: Action,
}

impl Transition {
    fn is_epsilon(&self) -> bool{
        match self.action {
            Action::Epsilon => true,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Action {
    Epsilon,
    Lower(Message),
    UpperBegin(Message),
    UpperEnd(Message),

    RepeatDnInit(CounterId, Expr), // Down-evaluate count, initialize counter to zero
    RepeatDnEntry(CounterId), // Guard on counter < count
    RepeatDnBack(CounterId), // Increment counter
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

    AltDnEnter(Vec<(Expr, Expr)>), // Down-evaluate r, up-evaluate l
    AltUpExit(Vec<(Expr, Expr)>), // Down-evaluate l, up-evaluate r
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        fn alt_pairs(f: &mut fmt::Formatter, text: &str, c: &Vec<(Expr, Expr)>) -> fmt::Result {
            try!(write!(f, "{} (", text));
            for &(ref l, ref r) in c {
                try!(write!(f, "{} = {},", l, r));
            }
            write!(f, ")")
        }

        match *self {
            Action::Epsilon => write!(f, "ε"),
            Action::Lower(ref m) => write!(f, "Lower {}", m),
            Action::UpperBegin(ref m) => write!(f, "UpperBegin {}", m),
            Action::UpperEnd(ref m) => write!(f, "UpperEnd {}", m),

            Action::RepeatDnInit(id, ref e) => write!(f, "RepeatDnInit {} {}", id, e),
            Action::RepeatDnEntry(id) => write!(f, "RepeatDnEntry {}", id),
            Action::RepeatDnBack(id) => write!(f, "RepeatDnBack {}", id),
            Action::RepeatDnExit(id) => write!(f, "RepeatDnExit {}", id),

            Action::RepeatUpInit(id) => write!(f, "RepeatUpInit {}", id),
            Action::RepeatUpBack(id)  => write!(f, "RepeatUpBack {}", id),
            Action::RepeatUpExit(id, ref e) => write!(f, "RepeatUpExit {} {}", id, e),

            Action::ForInit(id, count, _) => write!(f, "ForInit {} {}", id, count),
            Action::ForEntry(id, count, _) => write!(f, "ForEntry {} {}", id, count),
            Action::ForBack(id, count, _) => write!(f, "ForBack {} {}", id, count),
            Action::ForExit(id, count, _) => write!(f, "ForExit {} {}", id, count),

            Action::AltDnEnter(ref c) => alt_pairs(f, "AltDnEnter", c),
            Action::AltUpExit(ref c) => alt_pairs(f, "AltUpExit", c),
        }
    }
}

pub fn from_step_tree(s: &Step) -> Nfa {
    let mut nfa = Nfa::new();
    let initial = nfa.add_state();
    let success = nfa.add_state();
    nfa.initial.insert(initial);
    nfa.accepting.insert(success);
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
                if up {
                    if let Some(Value::Integer(const_count)) = count.up_const() {
                        match const_count {
                            0 => { nfa.add_transition(from, to, Action::Epsilon); return }
                            1 => { from_step_tree_inner(inner, nfa, from, to); return }
                            _ => (),
                        }
                    }
                    let start = nfa.add_state();
                    let end = nfa.add_state();
                    let counter = start;

                    if count.ignored() {
                        nfa.add_transition(from, start, Action::Epsilon);
                        nfa.add_transition(start, end, Action::Epsilon);
                        nfa.add_transition(start, to, Action::Epsilon);
                    } else {
                        nfa.add_transition(from, start, Action::RepeatUpInit(counter));
                        nfa.add_transition(start, end, Action::RepeatUpBack(counter));
                        nfa.add_transition(start, to, Action::RepeatUpExit(counter, count.clone()));
                    }
                    from_step_tree_inner(inner, nfa, end, start);
                } else {
                    let start = nfa.add_state();
                    let end = nfa.add_state();
                    let entry = nfa.add_state();
                    let counter = entry;

                    nfa.add_transition(from, entry, Action::RepeatDnInit(counter, count.clone()));
                    nfa.add_transition(entry, start, Action::RepeatDnEntry(counter));
                    nfa.add_transition(end, entry, Action::RepeatDnBack(counter));
                    nfa.add_transition(entry, to, Action::RepeatDnExit(counter));
                    from_step_tree_inner(inner, nfa, start, end);
                }

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

            Step::Alt(ref arms, up) => {
                for &(ref checks, ref inner) in arms {
                    let start;
                    let end;
                    if up {
                        start = from;
                        if checks.len() > 0 {
                            end = nfa.add_state();
                            nfa.add_transition(end, to, Action::AltUpExit(checks.clone()))
                        } else {
                            end = to;
                        }
                    } else {
                        start = nfa.add_state();
                        end = to;
                        nfa.add_transition(from, start, Action::AltDnEnter(checks.clone()))
                    }

                    from_step_tree_inner(inner, nfa, start, end);
                }
            }
        }
    }

    nfa
}
