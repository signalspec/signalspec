use std::collections::{ HashMap, hash_map, HashSet, VecDeque, BTreeMap };
use vec_map::VecMap;
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

    IntegerAdd(InsnRef, i64),
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
            Insn::IntegerAdd(ConstantInt(x), c) => return ConstantInt(x + c),
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
    message_blocks_lower: VecMap<InsnBlock>,
    message_blocks_upper: VecMap<InsnBlock>,
}

type DfaStateId = usize;

#[derive(Clone, Debug)]
pub struct State {
    accepting: bool,
    insns: InsnBlock,
    transitions: Vec<(Transition, DfaStateId)>,
}

#[derive(Clone, Debug)]
enum MessageSend {
    Lower{ tag: usize, data: Vec<InsnRef> },
    Upper{ tag: usize, data: Vec<InsnRef> }
}

#[derive(Clone, Debug)]
pub struct Transition {
    pre_conditions: Conditions,
    send: Vec<MessageSend>,
    registers: Vec<InsnRef>,

    recv: Option<(Side, usize, Conditions)>,
}

// Requires iteration in sorted order
type VarMap = BTreeMap<ValueID, InsnRef>;

#[derive(Debug, Clone)]
struct Thread {
    state: nfa::StateId,
    prev_states: HashSet<nfa::StateId>,
    down_vars: VarMap,
    up_vars: VarMap,
    for_vars: BTreeMap<usize, Vec<(Option<InsnRef>, Option<InsnRef>)>>,
    counters: VarMap,
    conditions: Conditions,
    send: Vec<MessageSend>,
}

impl Thread {
    fn init_counter(&mut self, id: ValueID){
        self.counters.insert(id, InsnRef::ConstantInt(0));
    }

    fn update_counter(&mut self, insns: &mut InsnBlock, id: usize, offset: i64) -> InsnRef {
        let count_reg_loc = self.counters.get_mut(&id).expect("Undefined counter");
        let count_reg = *count_reg_loc;
        *count_reg_loc = insns.add(Insn::IntegerAdd(count_reg, offset));
        count_reg
    }

    fn erase_values(&mut self) -> (Vec<InsnRef>, Conditions, Vec<MessageSend>) {
        self.prev_states.clear();
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
        let send = mem::replace(&mut self.send, Vec::new());

        (vars, conditions, send)
    }
}

type NfaStateSet = HashSet<nfa::StateId>;

fn closure<'nfa>(nfa: &'nfa Nfa, shape_down: &Shape, shape_up: &Shape, initial_threads: Vec<Thread>)
    -> (NfaStateSet, InsnBlock, Vec<(Option<(Side, &'nfa Message)>, Thread)>){

    let mut insns = InsnBlock::new(InsnRef::Transition);

    // Set of NFA states reachable without taking a transition accepting a symbol
    let mut closure = HashSet::new();

    // Threads of NFA execution waiting to be explored
    let mut queue: VecDeque<Thread> = initial_threads.into_iter().collect();

    let mut transitions = Vec::new();

    while let Some(thread) = queue.pop_front() {
        debug!("In state {}", thread.state);
        closure.insert(thread.state);

        if nfa.states[thread.state].transitions.len() == 0 && thread.send.len() > 0 {
            // No outbound transitions, but data to send, so generate a dummy transition to
            // attach the send.
            transitions.push((None, thread));
            continue;
        }

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
                    let mode = shape_down.data_mode();

                    if mode.down {
                        let regs = msg.components.iter().zip(variant.values())
                            .filter(|&(_, (_, dir))| dir.down)
                            .map(|(ref e, _)| insns.eval_down(e, &thread.down_vars))
                            .collect();
                        thread.send.push(MessageSend::Lower { tag: msg.tag, data: regs });
                    }

                    if mode.up {
                        transitions.push((Some((Side::Lower, msg)), thread));
                        continue;
                    } else {
                        queue.push_back(thread);
                    }

                }
                UpperBegin(ref msg) => {
                    let mode = shape_up.data_mode();

                    if mode.down {
                        transitions.push((Some((Side::Upper, msg)), thread));
                        continue;
                    } else {
                        queue.push_back(thread);
                    }
                }
                UpperEnd(ref msg) => {
                    let variant = &shape_up.variants[msg.tag];
                    let mode = shape_up.data_mode();

                    if mode.up {
                        let mut send = vec![];
                        for (ref e, (_, dir)) in msg.components.iter().zip(variant.values()) {
                            if dir.up {
                                send.push(insns.eval_down(e, &thread.up_vars));
                            }

                            e.each_var(&mut |v| {
                                thread.down_vars.remove(&v);
                                thread.up_vars.remove(&v);
                            });
                        }

                        thread.send.push(MessageSend::Upper{ tag: msg.tag, data: send });
                    }

                    queue.push_back(thread);
                }

                RepeatDnInit(counter_id, ref expr) => {
                    let reg = insns.eval_down(expr, &thread.down_vars);
                    debug!("rdi: {:?}", reg);
                    thread.counters.insert(counter_id, reg);
                    queue.push_back(thread);
                }

                RepeatDnEntry(counter_id) => {
                    let reg = *thread.counters.get(&counter_id).expect("Repeat count should be defined");
                    if thread.conditions.add(Condition::CheckRangeInt(reg, 1, ::std::i64::MAX)) {
                        thread.prev_states.insert(counter_id);
                        queue.push_back(thread);
                    }
                }

                RepeatDnBack(counter_id) => {
                    thread.update_counter(&mut insns, counter_id, -1);

                    if thread.prev_states.remove(&counter_id) {
                        transitions.push((None, thread));
                    } else {
                        queue.push_back(thread);
                    }
                }

                RepeatDnExit(counter_id) => {
                    let reg = thread.counters.remove(&counter_id).expect("Repeat count should be defined");
                    if thread.conditions.add(Condition::CheckRangeInt(reg, 0, 0)) {
                        queue.push_back(thread);
                    }
                }

                RepeatUpInit(counter_id) => {
                    thread.counters.insert(counter_id, insns.add_const(&Value::Integer(0)));
                    queue.push_back(thread)
                }
                RepeatUpBack(counter_id) => {
                    thread.update_counter(&mut insns, counter_id, 1);
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
                        thread.prev_states.insert(counter_id);
                        queue.push_back(thread);
                    }
                }

                ForBack(counter_id, _, ref vars) => {
                    thread.update_counter(&mut insns, counter_id, 1);

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

                    if thread.prev_states.remove(&counter_id) {
                        transitions.push((None, thread));
                    } else {
                        queue.push_back(thread);
                    }
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

    (closure, insns, transitions)
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

pub fn make_dfa(nfa: &Nfa, shape_down: &Shape, shape_up: &Shape) -> Dfa {
    let mut states = Vec::new();
    let mut state_by_nfa_state_set: HashMap<Vec<usize>, usize> = HashMap::new();
    let mut queue: VecDeque<(Option<(DfaStateId, Transition)>, Vec<Thread>)> = VecDeque::new();
    let mut message_blocks_lower = VecMap::new();
    let mut message_blocks_upper = VecMap::new();

    queue.push_back((None, nfa.initial.iter().map(|&state| {
        Thread {
            state: state,
            prev_states: HashSet::new(),
            counters: VarMap::new(),
            up_vars: VarMap::new(),
            down_vars: VarMap::new(),
            for_vars: BTreeMap::new(),
            conditions: Conditions::new(),
            send: Vec::new(),
        }
    }).collect()));

    while let Some((pt, threads)) = queue.pop_front() {
        let (closure, block, exits) = closure(nfa, shape_down, shape_up, threads);
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

                let mut transitions: Vec<(Thread, Transition)> = Vec::new();

                for (m, mut thread) in exits.into_iter() {
                    debug!("{:?} -> {}", m, thread.state);
                    debug!("    {:?}", thread.conditions);

                    let recv = match m {
                        Some((Side::Lower, msg)) => {
                            let mut msg_conditions = Conditions::new();
                            let message_block = message_blocks_lower.entry(msg.tag)
                                .or_insert_with(|| InsnBlock::new(InsnRef::MessageMatch));
                            message_match(shape_down, Side::Lower, msg, &mut thread.up_vars, message_block, &mut msg_conditions);

                            Some((Side::Lower, msg.tag, msg_conditions))
                        }
                        Some((Side::Upper, msg)) => {
                            let mut msg_conditions = Conditions::new();
                            let message_block = message_blocks_upper.entry(msg.tag)
                                .or_insert_with(|| InsnBlock::new(InsnRef::MessageMatch));
                            message_match(shape_up, Side::Upper, msg, &mut thread.down_vars, message_block, &mut msg_conditions);

                            Some((Side::Upper, msg.tag, msg_conditions))
                        }
                        None => None,
                    };

                    let (regs, pre_conditions, send) = thread.erase_values();

                    for &(_, ref t2) in &transitions {
                        if !t2.pre_conditions.mutually_excludes(&pre_conditions) {
                            match (&recv, &t2.recv) {
                                (&Some((ref s1, tag1, ref c1)), &Some((ref s2, tag2, ref c2))) if s1 == s2 => {
                                    if tag1 == tag2 && !c1.mutually_excludes(c2){
                                        panic!("Transitions with the same message tag must have mutually-exclusive conditions");
                                    }
                                },
                                _ => panic!("Transitions with different receive directions must have mutually-exclusive conditions")
                            }
                        }

                    }

                    transitions.push((thread, Transition {
                        pre_conditions: pre_conditions,
                        send: send,
                        registers: regs,
                        recv: recv,
                    }));
                }

                queue.extend(transitions.into_iter().map(|(thread, transition)| {
                    (Some((state, transition)), vec![thread])
                }));

                state
            }
        };

        if let Some((from_state, t)) = pt {
            states[from_state].transitions.push((t, dest_state));
        }
    }

    debug!("lower: {:#?}", message_blocks_lower);
    debug!("upper: {:#?}", message_blocks_upper);
    debug!("states: {:?}", state_by_nfa_state_set);

    Dfa { states: states, message_blocks_lower: message_blocks_lower, message_blocks_upper: message_blocks_upper }
}

impl Dfa {
    pub fn to_graphviz(&self, f: &mut Write) -> IoResult<()> {
        try!(writeln!(f, "digraph G {{"));
        for (id, state) in self.states.iter().enumerate() {
            for &(ref transition, dest_state) in &state.transitions {
                try!(writeln!(f, "{} -> {} [ label=\"{:?}, {:?}\"];",
                    id, dest_state, transition.pre_conditions, transition.recv));
            }
        }

        try!(writeln!(f, "start -> 0;"));

        try!(writeln!(f, "}}"));
        Ok(())
    }
}

use exec::Connection;
pub fn run(dfa: &Dfa, lower: &mut Connection, upper: &mut Connection) -> bool {
    let mut state_num = 0;

    let mut regs = Regs {
        arguments: vec![],
        transition: vec![],
        message: vec![],
        message_match: vec![],
    };

    'state_loop: loop {
        debug!("In state {}", state_num);
        let state = &dfa.states[state_num];
        regs.message_match.clear();
        regs.transition.clear();

        for i in &state.insns.insns {
            let val = regs.eval(i);
            debug!("\t%t{} <= {} = {:?}", regs.transition.len(), val, i);
            regs.transition.push(val);
        }

        let mut received = None;

        for &(ref t, dest_state) in &state.transitions {
            debug!("  Testing transition to {}", dest_state);

            if !t.pre_conditions.0.iter().all(|c| {
                let m = regs.condition(c);
                debug!("    {:?} => {}", c, m);
                m
            }) { continue }

            if let Some((side, tag, ref conditions)) = t.recv {
                if received.is_none() {
                    let (rx, blocks) = match side {
                        Side::Lower => (lower.recv(), &dfa.message_blocks_lower),
                        Side::Upper => (upper.recv(), &dfa.message_blocks_upper),
                    };

                    debug!("\trx {:?}", rx);

                    if let Ok((rx_tag, data)) = rx {
                        regs.message = data;

                        for i in &blocks[rx_tag].insns {
                            let val = regs.eval(i);
                            debug!("\t%m{} <= {} = {:?}", regs.message_match.len(), val, i);
                            regs.message_match.push(val);
                        }

                        received = Some((side, rx_tag));
                    } else {
                        return state.accepting;
                    }
                }

                let (rx_side, rx_tag) = received.unwrap();
                assert_eq!(side, rx_side);

                if tag != rx_tag { continue }

                if !conditions.0.iter().all(|c| {
                    let m = regs.condition(c);
                    debug!("    {:?} => {}", c, m);
                    m
                }) { continue }
            }

            // A transition matched
            state_num = dest_state;

            for send in &t.send {
                match *send {
                    MessageSend::Lower{tag, ref data} => {
                        lower.send((tag, data.iter().map(|&reg| regs.get(reg)).collect())).ok();
                    }
                    MessageSend::Upper{tag, ref data} => {
                        upper.send((tag, data.iter().map(|&reg| regs.get(reg)).collect())).ok();
                    }
                }
            }

            let next_arguments: Vec<_> = t.registers.iter().map(|&i| regs.get(i)).collect();
            for (i, (v, r)) in next_arguments.iter().zip(t.registers.iter()).enumerate() {
                debug!("  %a{} <= {} = {:?}", i, v, r);
            }
            regs.arguments = next_arguments;
            continue 'state_loop;
        }

        debug!("No matching conditions{}", if state.accepting {" in accepting state"} else {""});
        return state.accepting; //TODO: check that other signals have ended?
    }
}

struct Regs {
    arguments: Vec<Value>,
    transition: Vec<Value>,
    message: Vec<Value>,
    message_match: Vec<Value>,
}

impl Regs {
    fn get(&self, i: InsnRef) -> Value {
        match i {
            InsnRef::ConstantInt(i) => Value::Integer(i),
            InsnRef::UndefVec => Value::Vector(Vec::new()),
            InsnRef::Argument(i) => self.arguments[i].clone(),
            InsnRef::Transition(i) => self.transition[i].clone(),
            InsnRef::Message(i) => self.message[i].clone(),
            InsnRef::MessageMatch(i) => self.message_match[i].clone(),
        }
    }


    fn eval(&self, i: &Insn) -> Value {
        use self::Insn::*;
        match *i {
            Const(ref v) => v.clone(),

            Choose(i, ref choices) => {
                let v = self.get(i);
                choices.iter()
                    .find(|& &(ref a, _)|{ *a == v })
                    .map(|&(_, ref b)| b.clone())
                    .unwrap()
            }

            Concat(ref _elems) => {
                unimplemented!();
            }

            BinaryConst(reg, op, c) => {
                if let Value::Number(v) = self.get(reg) {
                    Value::Number(op.eval(v, c))
                } else {
                    panic!("BinaryConst on non-integer");
                }
            }

            VecElem(reg, i) => {
                if let Value::Vector(ref v) = self.get(reg) {
                    v[i].clone()
                } else {
                    panic!("VecElem on non-vector");
                }
            }

            VecSlice(reg, lo, hi) => {
                if let Value::Vector(ref v) = self.get(reg) {
                    Value::Vector(v[lo..hi].to_owned())
                } else {
                    panic!("VecSlice on non-vector");
                }
            }

            VecShift(reg, Some(from_reg)) => {
                if let Value::Vector(ref v) = self.get(reg) {
                    let mut v = v.clone();
                    v.push(self.get(from_reg));
                    Value::Vector(v)
                } else {
                    panic!("VecSlice on non-vector");
                }
            }

            VecShift(reg, None) => {
                if let Value::Vector(ref v) = self.get(reg) {
                    let mut v = v.clone();
                    v.remove(0);
                    Value::Vector(v)
                } else {
                    panic!("VecSlice on non-vector");
                }
            }

            IntegerAdd(reg, c) => {
                if let Value::Integer(i) = self.get(reg) {
                    Value::Integer(i+c)
                } else {
                    panic!("IntegerAdd on non-integer");
                }
            }
        }
    }

    fn condition(&self, c: &Condition) -> bool {
        use self::Condition::*;
        match *c {
            IntegerLt(a, b) => {
                if let (Value::Integer(a), Value::Integer(b)) = (self.get(a), self.get(b)) {
                    a < b
                } else {
                    panic!("IntegerLt on non-integer");
                }
            }

            IntegerEq(a, b) => {
                if let (Value::Integer(a), Value::Integer(b)) = (self.get(a), self.get(b)) {
                    a == b
                } else {
                    panic!("IntegerLt on non-integer");
                }
            }

            CheckConst(a, ref b) => {
                self.get(a) == *b
            }

            CheckRange(a, lo, hi) => {
                if let Value::Number(a) = self.get(a) {
                    a >= lo && a <= hi
                } else {
                    panic!("Non-number in CheckRange");
                }
            }

            CheckRangeInt(a, lo, hi) => {
                if let Value::Integer(a) = self.get(a) {
                    a >= lo && a <= hi
                } else {
                    panic!("Non-integer in CheckRangeInt");
                }
            }
        }
    }
}
