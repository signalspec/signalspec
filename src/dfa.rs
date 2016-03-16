use std::collections::{ HashMap, hash_map, HashSet, VecDeque, BTreeMap };
use std::fmt;
use std::mem;
use std::hash::Hash;
use std::io::{ Write, Result as IoResult };

use data::{ Value, Shape };
use eval::{ Expr, ConcatElem, BinOp };
use session::ValueID;
use nfa::{self, Nfa};
use exec::Message;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Side {
    Upper,
    Lower,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum InsnRef {
    ConstantInt(i64),
    UndefVec,
    Argument(usize),
    Transition(usize),
    Message(usize),
    MessageMatch(usize),
}

impl fmt::Debug for InsnRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InsnRef::ConstantInt(i) => write!(f, "#{}", i),
            InsnRef::UndefVec => write!(f, "vec_init"),
            InsnRef::Argument(i) => write!(f, "%a{}", i),
            InsnRef::Transition(i) => write!(f, "%t{}", i),
            InsnRef::Message(i) => write!(f, "%i{}", i),
            InsnRef::MessageMatch(i) => write!(f, "%m{}", i),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum InsnConcatElem {
    Elem(InsnRef),
    Slice(InsnRef, usize),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Insn {
    Const(Value),

    Choose(InsnRef, Vec<(Value, Value)>),
    Concat(Vec<InsnConcatElem>),
    BinaryConst(InsnRef, BinOp, f64),

    VecElem(InsnRef, usize),
    VecSlice(InsnRef, usize, usize),
    VecShift(InsnRef, Option<InsnRef>),

    IntegerInc(InsnRef),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Condition {
    IntegerLt(InsnRef, InsnRef),
    IntegerEq(InsnRef, InsnRef),

    CheckConst(InsnRef, Value),
    CheckRange(InsnRef, f64, f64),
    CheckRangeInt(InsnRef, i64, i64),
}

impl Condition {
    fn mutually_excludes(&self, other: &Condition) -> bool {
        use self::Condition::*;
        match (self, other) {
            (&CheckConst(ref x, ref a), &CheckConst(ref y, ref b)) if x == y && a != b => true,
            (&CheckRangeInt(ref x, lo1, hi1), &CheckRangeInt(ref y, lo2, hi2)) if x == y && (lo1 > hi2 || lo2 > hi1) => true,
            (&IntegerLt(ref x, ref a), &IntegerEq(ref y, ref b)) if x == y && a == b => true,
            (&IntegerEq(ref x, ref a), &IntegerLt(ref y, ref b)) if x == y && a == b => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct Conditions(Vec<Condition>);

impl Conditions {
    fn new() -> Conditions {
        Conditions(vec![])
    }

    fn add(&mut self, c: Condition) -> bool {
        use self::Condition::*;
        use self::InsnRef::ConstantInt;

        match c {
            IntegerLt(ConstantInt(x), ConstantInt(y)) => return x < y,
            IntegerEq(ConstantInt(x), ConstantInt(y)) => return x == y,
            CheckRangeInt(ConstantInt(x), lo, hi) => return x >= lo && x <= hi,
            _ => {
                self.0.push(c);
                true
            }
        }
    }

    fn mutually_excludes(&self, other: &Conditions) -> bool {
        for i in &self.0 {
            for j in &other.0 {
                if i.mutually_excludes(j) {
                    return true;
                }
            }
        }
        false
    }
}

#[derive(Clone)]
pub struct InsnBlock {
    key: fn(usize) -> InsnRef,
    insns: Vec<Insn>,
}

impl fmt::Debug for InsnBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.insns, f)
    }
}

impl InsnBlock {
    fn new(key: fn(usize) -> InsnRef) -> InsnBlock {
        InsnBlock { key: key, insns: Vec::new() }
    }
    fn find(&self, i: &Insn) -> Option<InsnRef> {
        match *i {
            Insn::Const(Value::Integer(x)) => Some(InsnRef::ConstantInt(x)),
            ref i => self.insns.iter().position(|x| x == i).map(self.key)
        }
    }

    fn add(&mut self, insn: Insn) -> InsnRef {
        use self::InsnRef::ConstantInt;

        match insn {
            Insn::IntegerInc(ConstantInt(x)) => return ConstantInt(x + 1),
            _ => (),
        }

        self.find(&insn).unwrap_or_else(||{
            self.insns.push(insn);
            (self.key)(self.insns.len()-1)
        })
    }

    fn add_const(&mut self, v: &Value) -> InsnRef {
        self.add(Insn::Const(v.clone()))
    }

    fn eval_down(&mut self, e: &Expr, vars: &VarMap) -> InsnRef {
        match *e {
            Expr::Ignored => panic!("Ignored value not ignored on down!"), // TODO: should this be defined?
            Expr::Const(ref v) => self.add_const(v),
            Expr::Variable(id, _) => *vars.get(&id).unwrap(),

            Expr::Range(..) | Expr::RangeInt(..) => panic!("Range value not ignored on down"),
            Expr::Union(..) => panic!("Union value not ignored on down"),

            Expr::Flip(box ref d, _) => self.eval_down(d, vars),
            Expr::Choose(box ref head, ref arms) => {
                let inner = self.eval_down(head, vars);
                self.add(Insn::Choose(inner, arms.clone()))
            }
            Expr::Concat(ref items) => {
                let inner_items = items.iter().map(|item| {
                    match *item {
                        ConcatElem::Elem(ref e) => InsnConcatElem::Elem(self.eval_down(e, vars)),
                        ConcatElem::Slice(ref e, size) => InsnConcatElem::Slice(self.eval_down(e, vars), size)
                    }
                }).collect();
                self.add(Insn::Concat(inner_items))
            }
            Expr::BinaryConst(ref a, op, b) => {
                let inner = self.eval_down(a, vars);
                self.add(Insn::BinaryConst(inner, op, b))
            }
        }
    }

    fn eval_up(&mut self, e: &Expr, val: InsnRef, vars: &mut VarMap, conditions: &mut Conditions) -> bool {
        match *e {
            Expr::Ignored => true,
            Expr::Const(ref v) => {
                conditions.add(Condition::CheckConst(val, v.clone()))
            }
            Expr::Variable(id, _) => {
                vars.insert(id, val);
                true
            }

            Expr::Range(lo, hi) => {
                conditions.add(Condition::CheckRange(val, lo, hi))
            }
            Expr::RangeInt(lo, hi) => {
                conditions.add(Condition::CheckRangeInt(val, lo, hi))
            }
            Expr::Union(..) => unimplemented!(),

            Expr::Flip(_, box ref u) => self.eval_up(u, val, vars, conditions),
            Expr::Choose(box ref head, ref arms) => {
                let arms_rev = arms.iter().map(|&(ref x, ref y)| (y.clone(), x.clone())).collect();
                let inner = self.add(Insn::Choose(val, arms_rev));
                self.eval_up(head, inner, vars, conditions)
            }
            Expr::Concat(ref items) => {
                let mut offset = 0;
                items.iter().all(|item| {
                    match *item {
                        ConcatElem::Elem(ref e) => {
                            let item_val = self.add(Insn::VecElem(val, offset));
                            offset += 1;
                            self.eval_up(e, item_val, vars, conditions)
                        }
                        ConcatElem::Slice(ref e, size) => {
                            let slice_val = self.add(Insn::VecSlice(val, offset, size));
                            offset += size;
                            self.eval_up(e, slice_val, vars, conditions)
                        }
                    }
                })
            }
            Expr::BinaryConst(ref a, op, b) => {
                let inner = self.add(Insn::BinaryConst(val, op.invert(), b));
                self.eval_up(a, inner, vars, conditions)
            }
        }
    }
}

#[derive(Debug)]
pub struct Dfa {
    states: Vec<State>,
}

type DfaStateId = usize;

#[derive(Clone, Debug)]
pub struct State {
    accepting: bool,
    insns: InsnBlock,
    transitions: Vec<Transition>,
}

#[derive(Clone, Debug)]
pub struct Transition {
    dest_state: DfaStateId,
    token: usize,
    conditions: Conditions,
    registers: Vec<InsnRef>,
}

// Requires iteration in sorted order
type VarMap = BTreeMap<ValueID, InsnRef>;

#[derive(Debug, Clone)]
struct Thread {
    state: nfa::StateId,
    down_vars: VarMap,
    up_vars: VarMap,
    for_vars: BTreeMap<usize, Vec<(Option<InsnRef>, Option<InsnRef>)>>,
    counters: VarMap,
    conditions: Conditions,
}

impl Thread {
    fn init_counter(&mut self, id: ValueID){
        self.counters.insert(id, InsnRef::ConstantInt(0));
    }

    fn increment_counter(&mut self, insns: &mut InsnBlock, id: ValueID) -> InsnRef {
        let count_reg_loc = self.counters.get_mut(&id).expect("Undefined counter");
        let count_reg = *count_reg_loc;
        *count_reg_loc = insns.add(Insn::IntegerInc(count_reg));
        count_reg
    }

    fn erase_values(&mut self) -> (Vec<InsnRef>, Conditions) {
        let mut vars = vec![];

        for (_, var) in self.counters.iter_mut() {
            let value = mem::replace(var, InsnRef::Argument(vars.len()));
            vars.push(value);
        }

        for (_, var) in self.up_vars.iter_mut() {
            let value = mem::replace(var, InsnRef::Argument(vars.len()));
            vars.push(value);
        }

        for (_, var) in self.down_vars.iter_mut() {
            let value = mem::replace(var, InsnRef::Argument(vars.len()));
            vars.push(value);
        }

        for (_, ref mut l) in self.for_vars.iter_mut() {
            for &mut (ref mut v1, ref mut v2) in l.iter_mut() {
                if let &mut Some(ref mut v) = v1 {
                    let value = mem::replace(v, InsnRef::Argument(vars.len()));
                    vars.push(value);
                }

                if let &mut Some(ref mut v) = v2 {
                    let value = mem::replace(v, InsnRef::Argument(vars.len()));
                    vars.push(value);
                }
            }
        }

        let conditions = mem::replace(&mut self.conditions, Conditions::new());

        (vars, conditions)
    }
}

type NfaStateSet = HashSet<nfa::StateId>;

fn closure<'nfa>(nfa: &'nfa Nfa, shape_down: &Shape, shape_up: &Shape, initial_threads: Vec<Thread>)
    -> (NfaStateSet, InsnBlock, Side, Vec<(&'nfa Message, Thread)>){

    let mut insns = InsnBlock::new(InsnRef::Transition);

    // Set of NFA states reachable without taking a transition accepting a symbol
    let mut closure = HashSet::new();

    // Threads of NFA execution waiting to be explored
    let mut queue: VecDeque<Thread> = initial_threads.into_iter().collect();

    let mut side = None;
    let mut transitions = Vec::new();

    while let Some(thread) = queue.pop_front() {
        debug!("In state {}", thread.state);
        closure.insert(thread.state);

        for transition in &nfa.states[thread.state].transitions {
            let mut thread = thread.clone();
            thread.state = transition.target;
            debug!("Evaluating transition to {}", transition.target);

            use nfa::Action::*;

            match transition.action {
                Epsilon => {
                    queue.push_back(thread)
                }

                Lower(ref msg) => {
                    let variant = &shape_down.variants[msg.tag];
                    let mode = variant.data_mode();

                    if mode.down {
                        // TODO: Output
                    }

                    if mode.up {
                        if side == Some(Side::Upper) {
                            panic!("Depending on up and down data in the same state");
                        }
                        side = Some(Side::Lower);
                        transitions.push((msg, thread));
                    } else {
                        queue.push_back(thread);
                    }

                }
                UpperBegin(ref msg) => {
                    let variant = &shape_up.variants[msg.tag];
                    let mode = variant.data_mode();

                    if mode.down {
                        if side == Some(Side::Lower) {
                            panic!("Depending on up and down data in the same state");
                        }
                        side = Some(Side::Upper);
                        transitions.push((msg, thread));
                    } else {
                        queue.push_back(thread);
                    }
                }
                UpperEnd(ref msg) => {
                    let variant = &shape_up.variants[msg.tag];
                    let mode = variant.data_mode();

                    if mode.up {
                        // TODO: output
                    }
                    queue.push_back(thread);
                }

                RepeatDnInit(..) | RepeatDnBack(..) | RepeatDnExit(..) => {
                    unimplemented!();
                }

                RepeatUpInit(counter_id) => {
                    thread.counters.insert(counter_id, insns.add_const(&Value::Integer(0)));
                    queue.push_back(thread)
                }
                RepeatUpBack(counter_id) => {
                    thread.increment_counter(&mut insns, counter_id);
                    queue.push_back(thread);
                }
                RepeatUpExit(counter_id, ref expr) => {
                    let count_reg = thread.counters.remove(&counter_id).expect("Repeat count should be defined");
                    if insns.eval_up(expr, count_reg, &mut thread.up_vars, &mut thread.conditions) {
                        queue.push_back(thread);
                    }
                }

                ForInit(counter_id, _, ref vars) => {
                    thread.init_counter(counter_id);

                    // Down-evaluate outer variables
                    let for_data = vars.iter().map(|&(_, ref expr, ref datamode)| {(
                        if datamode.down {
                            Some(insns.eval_down(expr, &thread.down_vars))
                        } else { None },
                        if datamode.up {
                            Some(InsnRef::UndefVec)
                        } else { None }
                    )}).collect();

                    thread.for_vars.insert(counter_id, for_data);


                    queue.push_back(thread)
                }

                ForEntry(counter_id, count, ref vars) => {
                    let count_reg = thread.counters.get(&counter_id).expect("Repeat count should be defined").clone();
                    if thread.conditions.add(Condition::CheckRangeInt(count_reg, 0, count as i64 - 1)) {
                        {
                            let for_data = thread.for_vars.get_mut(&counter_id).expect("For-loop variables not initialized at entry");

                            // pop next inner variables down
                            for (&(var, _, ref datamode), &mut (ref mut dn, _)) in vars.iter().zip(for_data.iter_mut()) {
                                if datamode.down {
                                    let dn = dn.as_mut().unwrap();
                                    let element_reg = insns.add(Insn::VecElem(*dn, 0));
                                    *dn = insns.add(Insn::VecShift(*dn, None));
                                    thread.down_vars.insert(var, element_reg);
                                }
                            }
                        }
                        queue.push_back(thread);
                    }
                }

                ForBack(counter_id, _, ref vars) => {
                    thread.increment_counter(&mut insns, counter_id);

                    {
                        let for_data = thread.for_vars.get_mut(&counter_id).expect("For-loop variables not initialized at back");

                        // push next inner variables up
                        for (&(var, _, ref datamode), &mut (_, ref mut up)) in vars.iter().zip(for_data.iter_mut()) {
                            thread.down_vars.remove(&var);
                            if datamode.up {
                                let reg = thread.up_vars.remove(&var).expect("Loop variable not up-evaluated in body");
                                let up = up.as_mut().unwrap();
                                *up = insns.add(Insn::VecShift(*up, Some(reg)));
                            }
                        }
                    }

                    queue.push_back(thread);
                }

                ForExit(counter_id, count, ref vars) => {
                    let count_reg = thread.counters.remove(&counter_id).expect("Repeat count should be defined").clone();
                    if thread.conditions.add(Condition::CheckRangeInt(count_reg, count as i64, count as i64)) {
                        let for_data = thread.for_vars.remove(&counter_id).expect("For-loop variables not initialized at exit");

                        // Up-evaluate outer variables
                        for (&(_, ref expr, ref datamode), (_, up)) in vars.iter().zip(for_data.into_iter()) {
                            if datamode.up {
                                insns.eval_up(expr, up.unwrap(), &mut thread.up_vars, &mut thread.conditions);
                            }
                        }

                        queue.push_back(thread);
                    }
                }
            }
        }
    }

    (closure, insns, side.unwrap_or(Side::Lower), transitions)
}

fn message_match(shape: &Shape, side: Side, msg: &Message, vars: &mut VarMap, block: &mut InsnBlock, conditions: &mut Conditions) {
    let variant = &shape.variants[msg.tag];

    for (idx, (e, (_, dir))) in msg.components.iter().zip(variant.values()).enumerate() {
        if (side == Side::Upper && dir.down) || (side == Side::Lower && dir.up) {
            let rx = InsnRef::Message(idx);
            block.eval_up(e, rx, vars, conditions);
        }
    }
}

fn set_to_sorted_vec<T: Clone + Hash + Ord>(s: &HashSet<T>) -> Vec<T> {
    let mut v: Vec<T> = s.iter().cloned().collect();
    v.sort();
    v
}

struct PendingTransition {
    from_state: usize,
    token: usize,
    conditions: Conditions,
    registers: Vec<InsnRef>,
}

pub fn make_dfa(nfa: &Nfa, shape_down: &Shape, shape_up: &Shape) -> Dfa {
    let mut states = Vec::new();
    let mut state_by_nfa_state_set: HashMap<Vec<usize>, usize> = HashMap::new();
    let mut queue: VecDeque<(Option<PendingTransition>, Vec<Thread>)> = VecDeque::new();
    let mut message_block = InsnBlock::new(InsnRef::MessageMatch);

    queue.push_back((None, nfa.initial.iter().map(|&state| {
        Thread {
            state: state,
            counters: VarMap::new(),
            up_vars: VarMap::new(),
            down_vars: VarMap::new(),
            for_vars: BTreeMap::new(),
            conditions: Conditions::new(),
        }
    }).collect()));

    while let Some((pt, threads)) = queue.pop_front() {
        let (closure, block, side, mut transitions) = closure(nfa, shape_down, shape_up, threads);
        debug!("{:?}", closure);

        let dest_state = match state_by_nfa_state_set.entry(set_to_sorted_vec(&closure)) {
            hash_map::Entry::Occupied(entry) => {
                let state = *entry.get();
                debug!("Transition to existing state {}", state);
                state
            }
            hash_map::Entry::Vacant(entry) => {
                let state = states.len();

                debug!("Creating state {}", state);
                debug!("{:#?}", block);

                entry.insert(state);
                states.push( State {
                    accepting: !nfa.accepting.is_disjoint(&closure),
                    insns: block,
                    transitions: vec![],
                });

                for &mut (ref m, ref mut t) in &mut transitions {
                    // TODO: other direction
                    message_match(shape_down, side, m, &mut t.up_vars, &mut message_block, &mut t.conditions);
                    debug!("{}\n{:#?}", m, t);
                }

                {
                    // Check that message conditions don't overlap
                    // TODO: split transitions into sets where conditions overlap (right now, one transition per set)
                    let mut iter = transitions.iter();
                    while let Some(&(ref m1, ref t1)) = iter.next() {
                        for &(ref m2, ref t2) in iter.clone() {
                            if m1.tag == m2.tag {
                                assert!(t1.conditions.mutually_excludes(&t2.conditions));
                            }
                        }
                    }
                }

                queue.extend(transitions.into_iter().map(|(m, mut t)| {
                    let (regs, conditions) = t.erase_values();
                    (Some(PendingTransition {
                        from_state: state,
                        token: m.tag,
                        conditions: conditions,
                        registers: regs,
                    }), vec![t])
                }));

                state
            }
        };

        if let Some(pt) = pt {
            states[pt.from_state].transitions.push(Transition {
                dest_state: dest_state,
                token: pt.token,
                conditions: pt.conditions,
                registers: pt.registers,
            });
        }
    }

    debug!("{:#?}", message_block);
    debug!("{:?}", state_by_nfa_state_set);

    Dfa { states: states }
}

impl Dfa {
    pub fn to_graphviz(&self, f: &mut Write) -> IoResult<()> {
        try!(writeln!(f, "digraph G {{"));
        for (id, state) in self.states.iter().enumerate() {
            for transition in &state.transitions {
                try!(writeln!(f, "{} -> {} [ label=\"{:?}\"];", id, transition.dest_state, transition.conditions));
            }
        }

        try!(writeln!(f, "start -> 0;"));

        try!(writeln!(f, "}}"));
        Ok(())
    }
}
