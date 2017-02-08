use std::collections::{ HashMap, HashSet, VecDeque, btree_map, BTreeMap };
use std::iter::Peekable;
use vec_map::VecMap;
use std::fmt;
use std::mem;
use std::cmp;
use std::io::{ Write, Result as IoResult };
use std::fmt::Debug;
use std::cell::Cell;

use data::{ Value, Type };
use protocol::Shape;
use session::ValueID;
use super::step::Message;
use super::eval::{ Expr, ConcatElem, BinOp, SignMode };
use super::nfa::{self, Nfa};
use connection::Connection;

/// The instruction in the DFA Static Single Assignment form. An Insn represents a computation
/// performed on one or more input variables.
#[derive(Clone, PartialEq, Debug)]
pub enum Insn {
    Const(Value),

    Choose(InsnRef, Vec<(Value, Value)>),
    Concat(Vec<InsnConcatElem>),
    BinaryConst(InsnRef, BinOp, Value),

    VecElem(InsnRef, usize),
    VecSlice(InsnRef, usize, usize),
    VecShift(InsnRef, Option<InsnRef>),

    FloatToInt(InsnRef),
    IntToFloat(InsnRef),
    IntToBits { width: usize, arg: InsnRef },
    BitsToInt { width: usize, arg: InsnRef, signed: bool },
}

#[derive(Clone, PartialEq, Debug)]
pub enum InsnConcatElem {
    Elem(InsnRef),
    Slice(InsnRef, usize),
}

/// A value referenced by an Insn.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InsnRef {
    /// A constant Value::Integer (used to initialize counters)
    ConstantInt(i64),

    /// A constant undefined vector (used to initialize for-loop shift registers)
    UndefVec,

    /// A value carried over from the previous DFA state
    Argument(usize),

    /// A value computed by the pre-message Insn basic block
    Transition(usize),

    /// A value received as a component of a message
    Message(usize),

    /// A value computed by the post-message Insn basic block
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

/// A boolean condition used as a predicate to enable transitions
#[derive(Clone, PartialEq, Debug)]
pub enum Condition {
    /// Equal to the value
    Eq(Value),

    /// In the lower-inclusive floating point range
    Range(f64, f64),

    // In the lower-inclusive integer range
    RangeInt(i64, i64),
}

impl Condition {
    fn and(&self, other: &Condition) -> Option<Condition> {
        use self::Condition::*;
        match (self, other) {
            (&Eq(ref v1), &Eq(ref v2)) if v1 == v2 => Some(Eq(v1.clone())),
            (&Range(lo1, hi1), &Range(lo2, hi2)) if !(lo1 >= hi2 || lo2 >= hi1) => Some(Range(f64::max(lo1, lo2), f64::min(hi1, hi2))),
            (&RangeInt(lo1, hi1), &RangeInt(lo2, hi2)) if !(lo1 >= hi2 || lo2 >= hi1) => Some(RangeInt(cmp::max(lo1, lo2), cmp::min(hi1, hi2))),
            _ => None
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Conditions(BTreeMap<InsnRef, Condition>);

impl Conditions {
    fn new() -> Conditions {
        Conditions(BTreeMap::new())
    }

    fn add(&mut self, i: InsnRef, c: Condition) -> bool {
        let c = match c {
            Condition::Eq(Value::Integer(v)) => Condition::RangeInt(v, v + 1),
            Condition::Eq(Value::Number(v)) => Condition::Range(v, v),
            c => c,
        };

        if let (InsnRef::ConstantInt(x), &Condition::RangeInt(lo, hi)) = (i, &c) {
            return x >= lo && x < hi
        }

        use std::collections::btree_map::Entry::*;
        match self.0.entry(i) {
            Occupied(mut e) => {
                let new = e.get().and(&c);
                if let Some(new) = new {
                    e.insert(new);
                    true
                } else { false }
            }
            Vacant(e) => {
                e.insert(c);
                true
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct RangeMap<K, V> {
    items: Vec<(K, K, V)>,
}

impl<K: PartialOrd + Copy + Debug, V: Clone> RangeMap<K, V> {
    pub fn with_one_range(lo: K, hi: K, v: V) -> RangeMap<K, V> {
        RangeMap { items: vec![(lo, hi, v)] }
    }

    fn binary_search(&self, k: K) -> Result<usize, usize> {
        use std::cmp::Ordering;
        self.items.binary_search_by(|&(ref min, ref max, _)| {
            if *min > k {
                Ordering::Greater
            } else if *max <= k {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        })
    }

    pub fn find(&self, k: K) -> Option<&V> {
        self.binary_search(k).ok().map(|idx| &self.items[idx].2)
    }

    pub fn span(&mut self, ins_lo: K, ins_hi: K, default: &V) -> &mut[(K, K, V)] {
        let mut old = PreIter::new(mem::replace(&mut self.items, Vec::new()));

        struct PreIter<I, T> {
            iter: I,
            first: Option<T>,
        }

        impl<I, T> Iterator for PreIter<I, T> where I: Iterator<Item=T> {
            type Item = T;
            fn next(&mut self) -> Option<T> {
                self.first.take().or_else(|| self.iter.next())
            }
        }

        impl<I, T> PreIter<I, T> where I: Iterator<Item=T>{
            fn new<X: IntoIterator<Item=T, IntoIter=I>>(it: X) -> PreIter<I, T> {
                PreIter { iter: it.into_iter(), first: None }
            }

            fn unshift(&mut self, t: T) {
                assert!(self.first.is_none());
                self.first = Some(t);
            }
        }

        while let Some((lo, hi, v)) = old.next() {
            if hi <= ins_lo {
                // completely below new range
                self.items.push((lo, hi, v));
            } else if lo < ins_lo {
                // straddles bottom of new range, split it
                self.items.push((lo, ins_lo, v.clone()));
                old.unshift((ins_lo, hi, v));
                break;
            } else {
                old.unshift((lo, hi, v));
                break;
            }
        }

        let start = self.items.len();

        let mut pos = ins_lo;
        while let Some((lo, hi, v)) = old.next() {
            if lo > pos && lo <= ins_hi {
                // fill gap
                self.items.push((pos, lo, default.clone()));
            }

            if hi <= ins_hi {
                // completely inside range
                self.items.push((lo, hi, v));
                pos = hi;
            } else if lo < ins_hi {
                // straddles top of new range, split it
                self.items.push((lo, ins_hi, v.clone()));
                old.unshift((ins_hi, hi, v));
                pos = ins_hi;
                break;
            } else {
                old.unshift((lo, hi, v));
                break;
            }
        }

        if pos < ins_hi {
            self.items.push((pos, ins_hi, default.clone()))
        }

        let end = self.items.len();
        self.items.extend(old);

        &mut self.items[start..end]
    }

    pub fn map<F, U: Clone + Default>(self, mut f: F) -> RangeMap<K, U> where F: FnMut(V) -> U {
        RangeMap {
            items: self.items.into_iter().map(|(lo, hi, v)| { (lo, hi, f(v)) }).collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ConditionTree<T> {
    Branch(InsnRef, ConditionBranch<T>),
    Leaf(T),
}

#[derive(Clone, Debug)]
pub enum ConditionBranch<T> {
    Symbol(HashMap<Value, ConditionTree<T>>, Box<ConditionTree<T>>),
    Range(RangeMap<f64, ConditionTree<T>>, Box<ConditionTree<T>>),
    RangeInt(RangeMap<i64, ConditionTree<T>>, Box<ConditionTree<T>>),
}

impl<T: Default> Default for ConditionTree<T> {
    fn default() -> ConditionTree<T> { ConditionTree::Leaf(T::default()) }
}

impl<T> ConditionBranch<T> {
    pub fn each_child<F: FnMut(&mut ConditionTree<T>)>(&mut self, mut each_child: F) {
        use self::ConditionBranch::*;

        match *self {
            Symbol(ref mut options, ref mut other) => {
                for opt in options.values_mut() { each_child(opt) }
                each_child(other);
            }
            Range(ref mut options, ref mut other) => {
                for &mut (_, _, ref mut opt) in &mut options.items {
                    each_child(opt);
                }
                each_child(other);
            }
            RangeInt(ref mut options, ref mut other) => {
                for &mut (_, _, ref mut opt) in &mut options.items {
                    each_child(opt);
                }
                each_child(other);
            }
        }
    }
}

impl<T: Clone + Default + Debug> ConditionTree<T> {
    pub fn new() -> ConditionTree<T> { ConditionTree::default() }

    pub fn find<'s, F: FnMut(InsnRef) -> Value>(&'s self, var_fn: &mut F) -> &'s T {
        match *self {
            ConditionTree::Branch(i, ref cond) => {
                let v = var_fn(i);
                debug!("Checking {:?}={} against {:?}", i, v, cond);
                match (v, cond) {
                    (ref s, &ConditionBranch::Symbol(ref options, ref other)) => {
                        options.get(s).unwrap_or(other).find(var_fn)
                    }
                    (Value::Number(i), &ConditionBranch::Range(ref options, ref other)) => {
                        options.find(i).unwrap_or(other).find(var_fn)
                    }
                    (Value::Integer(i), &ConditionBranch::RangeInt(ref options, ref other)) => {
                        options.find(i).unwrap_or(other).find(var_fn)
                    }
                    (a, b) => panic!("Can't match {} against {:?}", a, b)
                }
            }
            ConditionTree::Leaf(ref v) => v,
        }
    }

    fn map<F, U: Clone + Default + Debug>(self, f: &mut F) -> ConditionTree<U> where F: FnMut(T)->U {
        use self::ConditionTree::*;
        use self::ConditionBranch::*;

        match self {
            Branch(i, Symbol(options, other)) => {
                let options = options.into_iter().map(|(k, v)| (k, v.map(f))).collect();
                let other = Box::new(other.map(f));
                Branch(i, Symbol(options, other))
            }
            Branch(i, Range(options, other)) => {
                let other = Box::new(other.map(f));
                Branch(i, Range(options.map(|x| x.map(f)), other))
            }
            Branch(i, RangeInt(options, other)) => {
                let other = Box::new(other.map(f));
                Branch(i, RangeInt(options.map(|x| x.map(f)), other))
            }
            Leaf(v) => Leaf(f(v)),
        }
    }

    pub fn conditions<F: FnMut(&mut T)>(&mut self, conditions: &Conditions, mut each_loc: F) {
        self.conditions_inner(conditions.0.iter().peekable(), &mut each_loc);
    }

    fn conditions_inner<F: FnMut(&mut T)>(&mut self, mut conditions: Peekable<btree_map::Iter<InsnRef, Condition>>, each_loc: &mut F) {
        use self::ConditionTree::*;

        match (self, conditions.peek()) {
            (&mut Leaf(ref mut v), None) => {
                // At a leaf and no more conditions. We're done!
                each_loc(v);
            }
            (&mut Branch(_, ref mut branch), None) => {
                // We have no more conditions, therefore we don't have one for this variable.
                // Recurse into all branches.
                branch.each_child(|c| c.conditions_inner(conditions.clone(), each_loc));
            }
            (&mut Branch(reg, ref mut branch), Some(&(&next_reg, _))) if reg < next_reg => {
                // We have no condition for this variable, because if we did, it would be next
                // Recurse into all branches.
                branch.each_child(|c| c.conditions_inner(conditions.clone(), each_loc));
            }
            (&mut Branch(reg, ref mut branch), Some(&(&next_reg, next_val))) if reg == next_reg => {
                // At an existing level of the tree for this variable. Follow or insert
                // a branch for this condition
                conditions.next();
                match (branch, next_val) {
                    (&mut ConditionBranch::Symbol(ref mut options, ref other), &Condition::Eq(ref v)) => {
                        options.entry(v.clone()).or_insert_with(|| (**other).clone())
                            .conditions_inner(conditions, each_loc);
                    }
                    (&mut ConditionBranch::Range(ref mut options, ref other), &Condition::Range(lo, hi)) => {
                        for &mut (_, _, ref mut opt) in options.span(lo, hi, other) {
                            opt.conditions_inner(conditions.clone(), each_loc);
                        }
                    }
                    (&mut ConditionBranch::RangeInt(ref mut options, ref other), &Condition::RangeInt(lo, hi)) => {
                        for &mut (_, _, ref mut opt) in options.span(lo, hi, other) {
                            opt.conditions_inner(conditions.clone(), each_loc);
                        }
                    }
                    _ => { panic!("Variable type mismatch") }
                }
            }
            (loc, Some(&(&next_reg, next_val))) => {
                // No existing level of the tree for this variable. Insert one.

                conditions.next();
                let here = mem::replace(loc, ConditionTree::default());
                let mut new = here.clone();
                new.conditions_inner(conditions, each_loc);

                let new_branch = match *next_val {
                    Condition::Eq(ref v) => {
                        let mut options = HashMap::new();
                        options.insert(v.clone(), new);
                        ConditionBranch::Symbol(options, Box::new(here))
                    }
                    Condition::Range(lo, hi) => {
                        ConditionBranch::Range(RangeMap::with_one_range(lo, hi, new), Box::new(here))
                    }
                    Condition::RangeInt(lo, hi) => {
                        ConditionBranch::RangeInt(RangeMap::with_one_range(lo, hi, new), Box::new(here))
                    }
                };

                *loc = Branch(next_reg, new_branch);
            }
        }
    }

    fn to_graphviz<F: FnMut(&mut Write, &str, &T) -> IoResult<bool>>(&self, dest: &mut Write, counter: &Cell<u32>, parent_id: &str, leaf_fn: &mut F) -> IoResult<bool> {
        use self::ConditionTree::*;
        use self::ConditionBranch::*;
        use std::ops::Deref;

        match *self {
            Branch(i, ref cond) => {
                let header = format!("{:?}", i);
                let options: Vec<(String, &ConditionTree<T>)> = match *cond {
                    Symbol(ref options, ref other) => {
                        options.iter().map(|(k, v)| (format!("{}", k), v))
                            .chain(Some(("_".to_owned(), other.deref())))
                            .collect()
                    }
                    Range(ref options, ref other) => {
                        options.items.iter().map(|&(min, max, ref v)| (format!("{}..{}", min, max), v))
                            .chain(Some(("_".to_owned(), other.deref())))
                            .collect()
                    }
                    RangeInt(ref options, ref other) => {
                        options.items.iter().map(|&(min, max, ref v)| (format!("{}..{}", min, max), v))
                            .chain(Some(("_".to_owned(), other.deref())))
                            .collect()
                    }
                };

                let mut children = Vec::new();
                let node_id = format!("c{}", counter.get());
                counter.set(counter.get() + 1);

                for (i, (s, child)) in options.into_iter().enumerate() {
                    let port_id = format!("{}:{}", node_id, i);
                    if try!(child.to_graphviz(dest, counter, &port_id, leaf_fn)) {
                        children.push((i, s));
                    }
                }

                if children.len() != 0 {
                    try!(writeln!(dest, r#"    {} -> {}:top;"#, parent_id, node_id));
                    try!(writeln!(dest, r#"    {} [shape="none" label=<<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
<TR><TD COLSPAN="{}" PORT="top"><B>{}</B></TD></TR><TR>"#, node_id, children.len(), header));
                    for &(i, ref s) in &children {
                        try!(writeln!(dest, r#"<TD PORT="{}">{}</TD>"#, i, s));
                    }
                    try!(writeln!(dest, r#"</TR></TABLE>>];"#));
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            Leaf(ref v) => {
                leaf_fn(dest, parent_id, v)
            }
        }
    }
}

#[derive(Clone, Debug)]
enum RecvTree<T> {
    Fail,
    Recv {
        side: Side,
        send: Vec<MessageSend>,
        block: InsnBlock,
        tree: ConditionTree<Option<(Vec<MessageSend>, T)>>,
    },
    Loop {
        send_lower: Vec<MessageSend>,
        send_upper: Vec<MessageSend>,
        transition: T
    },
}

impl<T> Default for RecvTree<T> {
    fn default() -> RecvTree<T> { RecvTree::Fail }
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
            Insn::BinaryConst(ConstantInt(x), op, Value::Integer(v)) => return ConstantInt(op.eval(x, v)),
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
            Expr::Variable(id, _) => *vars.get(&id).unwrap_or_else(|| panic!("invalid variable reference {}", id)),

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
            Expr::BinaryConst(ref a, op, ref b) => {
                let inner = self.eval_down(a, vars);
                self.add(Insn::BinaryConst(inner, op, b.clone()))
            }

            Expr::FloatToInt(ref expr) => {
                let inner = self.eval_down(expr, vars);
                self.add(Insn::FloatToInt(inner))
            }

            Expr::IntToBits { ref expr, width, ..} => {
                let inner = self.eval_down(expr, vars);
                self.add(Insn::IntToBits{ arg: inner, width: width })
            }

            Expr::Chunks { ref expr, width } => {
                let inner_width = if let Type::Vector(count, _) = expr.get_type() { count }
                                  else { panic!("Chunks on non-vector ") };

                let inner = self.eval_down(expr, vars);

                let insn = Insn::Concat((0..(inner_width / width)).map(|i| {
                    InsnConcatElem::Elem(self.add(Insn::VecSlice(inner, i*width, (i+1)*width)))
                }).collect());

                self.add(insn)
            }

        }
    }

    fn eval_up(&mut self, e: &Expr, val: InsnRef, vars: &mut VarMap, conditions: &mut Conditions) -> bool {
        match *e {
            Expr::Ignored => true,
            Expr::Const(ref v) => {
                conditions.add(val, Condition::Eq(v.clone()))
            }
            Expr::Variable(id, _) => {
                vars.insert(id, val);
                true
            }

            Expr::Range(lo, hi) => {
                conditions.add(val, Condition::Range(lo, hi))
            }
            Expr::RangeInt(lo, hi) => {
                conditions.add(val, Condition::RangeInt(lo, hi))
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
            Expr::BinaryConst(ref a, op, ref b) => {
                let inner = self.add(Insn::BinaryConst(val, op.invert(), b.clone()));
                self.eval_up(a, inner, vars, conditions)
            }

            Expr::FloatToInt(ref expr) => {
                let inner = self.add(Insn::IntToFloat(val));
                self.eval_up(expr, inner, vars, conditions)
            }

            Expr::IntToBits { ref expr, width, signed } => {
                let tc = signed == SignMode::TwosComplement;
                let inner = self.add(Insn::BitsToInt{arg: val, width: width, signed: tc});
                self.eval_up(expr, inner, vars, conditions)
            }

            Expr::Chunks { ref expr, width } => {
                let inner_width = if let Type::Vector(count, _) = expr.get_type() { count }
                                  else { panic!("Chunks on non-vector") };

                let insn = Insn::Concat((0..(inner_width / width)).map(|i| {
                    InsnConcatElem::Slice(self.add(Insn::VecElem(val, i)), width)
                }).collect());
                let insn = self.add(insn);

                self.eval_up(expr, insn, vars, conditions)
            }
        }
    }
}

/// A compiled DFA
#[derive(Debug)]
pub struct Dfa {
    states: Vec<State>,
}

type DfaStateId = usize;

/// A DFA state
#[derive(Clone, Debug)]
pub struct State {
    /// True if this is an accepting state (DFA accepts if input signal ends in this state)
    accepting: bool,

    /// The instructions that are executed prior to receiving a message. These values are available
    /// in the Transition pre_conditions to determine whether to accept a message on the upper or
    /// lower sides (e.g. based on counter values).
    insns: InsnBlock,

    /// The set of transitions leaving this state, and the corresponding destination state
    transitions: ConditionTree<RecvTree<Transition>>,
}

impl State {
    fn new() -> State {
        fn dummy_block_fn(_: usize) -> InsnRef { unreachable!() }
        State {
            accepting: false,
            insns: InsnBlock::new(dummy_block_fn),
            transitions: ConditionTree::new(),
        }
    }
}

pub type MessageSend = Vec<Option<InsnRef>>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Side {
    Upper,
    Lower,
}

/// A transition out of a state
#[derive(Clone, Debug)]
pub struct Transition {
    dest_state: usize,

    /// State to persist across the transition. A list of live registers to pack into the
    /// arguments array, which can be accessed by the next state without regard for which
    /// incoming transition was taken.
    registers: Vec<InsnRef>,
}

// Requires iteration in sorted order
type VarMap = BTreeMap<ValueID, InsnRef>;

/// A thread of NFA execution used to construct the DFA
#[derive(Debug, Clone)]
struct Thread {
    state: nfa::StateId,
    prev_states: HashSet<nfa::StateId>,
    down_vars: VarMap,
    up_vars: VarMap,
    for_vars: BTreeMap<usize, Vec<(Option<InsnRef>, Option<InsnRef>)>>,
    counters: VarMap,
    conditions: Conditions,
    send_lower: Vec<MessageSend>,
    send_upper: Vec<MessageSend>,
}

impl Thread {
    fn init_counter(&mut self, id: ValueID){
        self.counters.insert(id, InsnRef::ConstantInt(0));
    }

    fn update_counter(&mut self, insns: &mut InsnBlock, id: usize, offset: i64) -> InsnRef {
        let count_reg_loc = self.counters.get_mut(&id).expect("Undefined counter");
        let count_reg = *count_reg_loc;
        *count_reg_loc = insns.add(Insn::BinaryConst(count_reg, BinOp::Add, Value::Integer(offset)));
        count_reg
    }

    /// Extract the set of values that are persisted across DFA transitions. This appends the actual
    /// register values to an arguments array, and replaces them with a reference to the arguments
    /// array by index. The resulting thread will be identical for any transition into the same
    /// DFA state, so code in that state can be generated without regard for the previous state.
    fn erase_vars(&mut self, vars: &mut Vec<InsnRef>) {
        self.prev_states.clear();

        for (_, var) in self.counters.iter_mut() {
            let value = mem::replace(var, InsnRef::Argument(vars.len()));
            vars.push(value);
        }

        for (id, var) in self.up_vars.iter_mut() {
            debug!("up-var {} lives across transition", id);
            let value = mem::replace(var, InsnRef::Argument(vars.len()));
            vars.push(value);
        }

        for (id, var) in self.down_vars.iter_mut() {
            debug!("down-var {} lives across transition", id);
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
    }

    /// Take the conditions and sent values from the thread, returning them and replacing them
    /// with empty lists.
    fn erase_values(&mut self) -> (Conditions, Vec<MessageSend>, Vec<MessageSend>) {
        let conditions = mem::replace(&mut self.conditions, Conditions::new());
        let send_lower = mem::replace(&mut self.send_lower, Vec::new());
        let send_upper = mem::replace(&mut self.send_upper, Vec::new());

        (conditions, send_lower, send_upper)
    }
}

type NfaStateSet = HashSet<nfa::StateId>;

fn closure<'nfa>(nfa: &'nfa Nfa, shape_down: &Shape, shape_up: &Shape, initial_threads: Vec<Thread>)
    -> (NfaStateSet, InsnBlock, Vec<(Option<(Side, &'nfa Message)>, Thread)>){

    let mut insns = InsnBlock::new(InsnRef::Transition);

    // Set of NFA states reachable without taking a transition receiving a message
    let mut closure = HashSet::new();

    // Threads of NFA execution waiting to be explored
    let mut queue: VecDeque<Thread> = initial_threads.into_iter().collect();

    // Discovered transitions that leave this DFA state
    let mut transitions = Vec::new();

    let format_up = shape_up.format();
    let format_down = shape_down.format();

    while let Some(thread) = queue.pop_front() {
        debug!("In state {}", thread.state);
        closure.insert(thread.state);

        if nfa.states[thread.state].transitions.len() == 0 && thread.send_lower.len() + thread.send_upper.len() > 0 {
            // No outbound transitions, but data to send, so generate a dummy transition to
            // attach the send.
            transitions.push((None, thread));
            continue;
        }

        for transition in &nfa.states[thread.state].transitions {
            let mut thread = thread.clone();
            thread.state = transition.target;
            debug!("Evaluating transition to {}", transition.target);

            use super::nfa::Action::*;

            match transition.action {
                Epsilon => {
                    queue.push_back(thread)
                }

                Lower(ref msg) => {
                    let mode = shape_down.data_mode();

                    if mode.down {
                        let regs = msg.iter().zip(format_down.iter())
                            .map(|(e, fmt)| {
                                match e.as_ref() {
                                    Some(x) if fmt.dir.down => Some(insns.eval_down(x, &thread.down_vars)),
                                    _ => None
                                }
                            }).collect();
                        thread.send_lower.push(regs);
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
                    let mode = shape_up.data_mode();

                    if mode.up {
                        let mut send = msg.iter().zip(format_up.iter()).map(|(e, fmt)| {
                            e.as_ref().and_then(|e| {
                                if fmt.dir.up {
                                    Some(insns.eval_down(e, &thread.up_vars))
                                } else { None }
                            })
                        }).collect();
                        thread.send_upper.push(send);
                    }

                    for e in msg.iter().filter_map(Option::as_ref) {
                        e.each_var(&mut |v| {
                            thread.down_vars.remove(&v);
                            thread.up_vars.remove(&v);
                        })
                    }

                    queue.push_back(thread);
                }

                RepeatDnInit(counter_id, ref expr) => {
                    let reg = insns.eval_down(expr, &thread.down_vars);
                    thread.counters.insert(counter_id, reg);
                    queue.push_back(thread);
                }

                RepeatDnEntry(counter_id) => {
                    let reg = *thread.counters.get(&counter_id).expect("Repeat count should be defined");
                    if thread.conditions.add(reg, Condition::RangeInt(1, ::std::i64::MAX)) {
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
                    if thread.conditions.add(reg, Condition::RangeInt(0, 1)) {
                        queue.push_back(thread);
                    }
                }

                RepeatUpInit(counter_id) => {
                    thread.init_counter(counter_id);
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
                    if thread.conditions.add(count_reg, Condition::RangeInt(0, count as i64)) {
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
                    if thread.conditions.add(count_reg, Condition::RangeInt(count as i64, count as i64 + 1)) {
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

                AltDnEnter(ref c) => {
                    for &(ref pat, ref r) in c {
                        let v = insns.eval_down(r, &thread.down_vars);
                        insns.eval_up(pat, v, &mut thread.down_vars, &mut thread.conditions);
                    }
                    queue.push_back(thread);
                }

                AltUpExit(ref c) => {
                    for &(ref pat, ref r) in c {
                        let v = insns.eval_down(pat, &thread.up_vars);
                        insns.eval_up(r, v, &mut thread.up_vars, &mut thread.conditions);
                    }
                    queue.push_back(thread);
                }
            }
        }
    }

    (closure, insns, transitions)
}

fn message_match(shape: &Shape, side: Side, msg: &Message, vars: &mut VarMap, block: &mut InsnBlock) -> Conditions {
    let mut conditions = Conditions::new();

    for (idx, e) in msg.iter().enumerate() {
        if let &Some(ref e) = e {
            let rx = InsnRef::Message(idx);
            block.eval_up(e, rx, vars, &mut conditions);
        }
    }
    conditions
}

pub fn make_dfa(nfa: &Nfa, shape_down: &Shape, shape_up: &Shape) -> Dfa {
    debug!("\nshape_down: {:#?}\n\nshape_up: {:#?}", shape_down, shape_up);
    let mut states: Vec<State> = Vec::new();
    let mut state_by_nfa_entry_states: HashMap<Vec<usize>, usize> = HashMap::new();
    let mut queue: VecDeque<(DfaStateId, Vec<Thread>)> = VecDeque::new();

    states.push(State::new());
    queue.push_back((0, nfa.initial.iter().map(|&state| {
        Thread {
            state: state,
            prev_states: HashSet::new(),
            counters: VarMap::new(),
            up_vars: VarMap::new(),
            down_vars: VarMap::new(),
            for_vars: BTreeMap::new(),
            conditions: Conditions::new(),
            send_lower: Vec::new(),
            send_upper: Vec::new(),
        }
    }).collect()));

    while let Some((state, threads)) = queue.pop_front() {
        let state_key = threads.iter().map(|x| x.state).collect::<Vec<_>>();

        // Walk the NFA states reachable from this state, and collect the NFA threads at
        // the point they they transition out of this DFA state.
        let (closure, block, exits) = closure(nfa, shape_down, shape_up, threads);

        debug!("Creating DFA state {} = NFA states {:?} ({:?})", state, state_key, closure);
        debug!("{:#?}", block);

        states[state].insns = block;
        states[state].accepting = !nfa.accepting.is_disjoint(&closure);

        // Group the transition threads into mutually-exclusive groups. These will become
        // DFA transitions.
        let mut tt = ConditionTree::<RecvTree<Vec<Thread>>>::new();

        for (recv_msg, mut thread) in exits {
            debug!("{:?} -> {}", recv_msg, thread.state);
            debug!("    conditions: {:?}", thread.conditions);
            debug!("    counters: {:?}", thread.counters);
            debug!("    send_lower: {:#?}", thread.send_lower);
            debug!("    send_upper: {:#?}", thread.send_upper);

            let (pre_cond, send_lower, send_upper) = thread.erase_values();

            tt.conditions(&pre_cond, |loc| {
                match recv_msg {
                    Some((side, msg)) => {
                        let mut thread = thread.clone();
                        let (send_side, send_other, shape);
                        match side {
                            Side::Lower => {
                                send_side = &send_lower;
                                send_other = &send_upper;
                                shape = shape_down;
                            }
                            Side::Upper => {
                                send_side = &send_upper;
                                send_other = &send_lower;
                                shape = shape_up;
                            }
                        }

                        match *loc {
                            RecvTree::Fail => {
                                *loc = RecvTree::Recv {
                                    side: side,
                                    send: send_side.clone(),
                                    block: InsnBlock::new(InsnRef::MessageMatch),
                                    tree: ConditionTree::new()
                                };
                            }
                            RecvTree::Recv { side: existing_side, send: ref existing_send, .. } if side == existing_side => {
                                if send_side != existing_send { panic!("Overlapping transitions must output the same data"); }
                            }
                            _ => panic!("Transitions with different receive directions must have mutually-exclusive conditions")
                        }

                        if let RecvTree::Recv { ref mut block, ref mut tree, .. } = *loc {
                            let conditions = {
                                let vars = match side {
                                    Side::Lower => &mut thread.up_vars,
                                    Side::Upper => &mut thread.down_vars,
                                };
                                message_match(shape, side, msg, vars, block)
                            };

                            tree.conditions(&conditions, |loc| {
                                match *loc {
                                    None => {
                                        *loc = Some((send_other.clone(), vec![thread.clone()]));
                                    }
                                    Some((ref send, ref mut threads)) => {
                                        if send != send_other {
                                             panic!("Overlapping transitions must output the same data");
                                        }
                                        threads.push(thread.clone());
                                    }
                                }
                            })
                        } else { unreachable!() }
                    }
                    None => {
                        match *loc {
                            RecvTree::Fail => {
                                *loc = RecvTree::Loop { send_lower: send_lower.clone(), send_upper: send_upper.clone(), transition: vec![thread.clone()] };
                            }
                            RecvTree::Loop { send_lower: ref lower, send_upper: ref upper, ref mut transition } => {
                                if &send_lower == lower && &send_upper == upper {
                                    transition.push(thread.clone());
                                } else {
                                    panic!("Overlapping transitions must output the same data");
                                }
                            }
                            _ => panic!("Transitions with different receive directions must have mutually-exclusive conditions")
                        }
                    }
                }
            });
        }

        let transitions = {
        let mut thread_map = |mut threads: Vec<Thread>| -> Transition {
            let mut regs = Vec::new();

            threads.sort_by_key(|x| x.state);

            let mut state_key = threads.iter().map(|x| x.state).collect::<Vec<_>>();
            state_key.dedup();

            if threads.len() != state_key.len() {
                info!("Ambiguity with {:?}", threads);
            }

            for thread in &mut threads {
                thread.erase_vars(&mut regs);
            }

            debug!("Generating transition to NFA states {:?}", state_key);
            debug!("Regs: {:?}", regs);

            // Find the destination state if it exists, or make a new one
            let dest_state = *state_by_nfa_entry_states.entry(state_key).or_insert_with(|| {
                let state = states.len();
                states.push(State::new());
                queue.push_back((state, threads));
                state
            });

            debug!("  = DFA state {}", dest_state);

            Transition {
                registers: regs,
                dest_state: dest_state,
            }
        };

        tt.map(&mut |loc| {
            match loc {
                RecvTree::Fail => RecvTree::Fail,
                RecvTree::Recv { side, send, block, tree } => {
                    let tree = tree.map(&mut |loc| {
                        loc.map(|(send, threads)| (send, thread_map(threads)))
                    });

                    RecvTree::Recv { side: side, send: send, block: block, tree: tree }
                }
                RecvTree::Loop { send_lower, send_upper, transition } => {
                    RecvTree::Loop { send_lower: send_lower, send_upper: send_upper, transition: thread_map(transition) }
                }
            }

        })};

        states[state].transitions =  transitions;
    }

    debug!("states: {:?}", state_by_nfa_entry_states);

    Dfa { states: states }
}

impl Dfa {
    pub fn to_graphviz(&self, f: &mut Write) -> IoResult<()> {
        let condition_count = Cell::new(0);
        try!(writeln!(f, "digraph G {{"));

        for (id, state) in self.states.iter().enumerate() {
            try!(writeln!(f, "  subgraph    state{} {{", id));
            if state.accepting {
                try!(writeln!(f, "    {} [peripheries=2];", id));
            } else {
                try!(writeln!(f, "    {};", id));
            }
            try!(state.transitions.to_graphviz(f, &condition_count, &format!("{}", id), &mut |f, parent_port, loc| {
                match *loc {
                    RecvTree::Fail => {Ok(false)},
                    RecvTree::Recv { side, ref send, ref block, ref tree } => {
                        let node = format!("r{}", condition_count.get());
                        condition_count.set(condition_count.get() + 1);

                        try!(writeln!(f, r#"    {} -> {}:top [label="{:?}"]"#, parent_port, node, send));
                        let color = match side { Side::Lower => "blue", Side::Upper => "green" };
                        try!(writeln!(f, r#"    {} [shape="none" fontcolor="{}" label=<<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
    <TR><TD PORT="top"><B>{:?}</B></TD></TR></TABLE>>];";"#, node, color, side));

                        let port = format!("{}_p", node);
                        try!(tree.to_graphviz(f, &condition_count, &port, &mut |f, parent_port, out| {
                            if let &Some((ref send, ref transition)) = out {
                                try!(writeln!(f, r#"    {} -> {} [label="{:?}"]"#, parent_port, transition.dest_state, send));
                                Ok(true)
                            } else {
                                Ok(false)
                            }
                        }));
                        Ok(true)
                    }
                    RecvTree::Loop { ref send_lower, ref send_upper, ref transition } => {
                        try!(writeln!(f, r#"    {} -> {} [label="{:?} / {:?}"]"#, parent_port, transition.dest_state, send_lower, send_upper));
                        Ok(true)
                    }
                }
            }));
            try!(writeln!(f, "  }}\n"));
        }

        try!(writeln!(f, "start -> 0;"));

        try!(writeln!(f, "}}"));
        Ok(())
    }
}

pub fn run(dfa: &Dfa, lower: &mut Connection, upper: &mut Connection) -> bool {
    let mut state_num = 0;

    let mut regs = Regs {
        arguments: vec![],
        transition: vec![],
        message: vec![],
        message_match: vec![],
    };

    fn send(regs: &Regs, conn: &mut Connection, msgs: &[MessageSend], dbg_str: &str) {
        for msg_regs in msgs {
            let msg = msg_regs.iter().map(|&reg| reg.map(|reg| regs.get(reg))).collect();
            debug!("send {} {:?}", dbg_str, msg);
            conn.send(msg).ok();
        }
    }

    loop {
        debug!("In state {}", state_num);
        let state = &dfa.states[state_num];
        regs.message_match.clear();
        regs.transition.clear();

        for i in &state.insns.insns {
            let val = regs.eval(i);
            debug!("\t%t{} <= {} = {:?}", regs.transition.len(), val, i);
            regs.transition.push(val);
        }

        let transition = match state.transitions.find(&mut |r| regs.get(r)) {
            &RecvTree::Fail => {
                debug!("No matching pre-conditions{}", if state.accepting {" in accepting state"} else {""});
                return state.accepting; //TODO: check that other signals have ended?
            }
            &RecvTree::Recv { side, send: ref send_side, ref block, ref tree } => {
                let rx = match side {
                    Side::Lower => {
                        send(&regs, lower, send_side, "lower");
                        lower.recv()
                    }
                    Side::Upper => {
                        send(&regs, upper, send_side, "upper");
                        upper.recv()
                    }
                };

                debug!("\trx {:?}", rx);

                let data = if let Ok(r) = rx { r } else {
                    debug!("input completed");
                    return state.accepting;
                };

                // Evaluate the message-match block
                regs.message = data.into_iter().map(|x| x.unwrap_or(Value::Integer(0))).collect();
                for i in &block.insns {
                    let val = regs.eval(i);
                    debug!("\t%m{} <= {} = {:?}", regs.message_match.len(), val, i);
                    regs.message_match.push(val);
                }

                if let &Some((ref send_other, ref transition)) = tree.find(&mut |r| regs.get(r)) {
                    match side {
                        Side::Lower => send(&regs, upper, send_other, "upper"),
                        Side::Upper => send(&regs, lower, send_other, "lower"),
                    }

                    transition
                } else {
                    debug!("No matching conditions{}", if state.accepting {" in accepting state"} else {""});
                    return state.accepting; //TODO: check that other signals have ended?
                }
            }
            &RecvTree::Loop { ref send_lower, ref send_upper, ref transition } => {
                send(&regs, lower, send_lower, "lower");
                send(&regs, upper, send_upper, "upper");
                transition
            }
        };

        state_num = transition.dest_state;
        let next_arguments: Vec<_> = transition.registers.iter().map(|&i| regs.get(i)).collect();
        for (i, (v, r)) in next_arguments.iter().zip(transition.registers.iter()).enumerate() {
            debug!("  %a{} <= {} = {:?}", i, v, r);
        }
        regs.arguments = next_arguments;
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

            Concat(ref elems) => {
                let mut out = Vec::new();
                for i in elems {
                    match *i {
                        InsnConcatElem::Elem(e) => out.push(self.get(e)),
                        InsnConcatElem::Slice(s, w) => match self.get(s) {
                            Value::Vector(mut v) => {
                                assert_eq!(v.len(), w);
                                out.append(&mut v);
                            }
                            _ => return Value::Integer(0) //panic!("Concat slice passed non-vector {}", x)
                        }
                    }
                }
                Value::Vector(out)
            }

            BinaryConst(reg, op, ref c) => {
                match (self.get(reg), c) {
                    (Value::Number(a),  &Value::Number(b))  => Value::Number(op.eval(a, b)),
                    (Value::Integer(a), &Value::Integer(b)) => Value::Integer(op.eval(a, b)),
                    (Value::Complex(a), &Value::Complex(b)) => Value::Complex(op.eval(a, b)),
                    (Value::Complex(a), &Value::Number(b)) => Value::Complex(op.eval(a, b)),
                    (Value::Number(a),  &Value::Complex(b)) => Value::Complex(op.eval(a, b)),
                    (a, b) => panic!("BinaryConst on mismatched types {} {:?} {}", a, op, b)
                }
            }

            VecElem(reg, i) => {
                if let Value::Vector(ref v) = self.get(reg) {
                    v.get(i).unwrap_or(&Value::Integer(0)).clone()
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
                    if v.len() > 0 {
                        v.remove(0);
                    }
                    Value::Vector(v)
                } else {
                    panic!("VecSlice on non-vector");
                }
            }

            FloatToInt(arg) => {
                if let Value::Number(v) = self.get(arg) {
                    Value::Integer(v as i64)
                } else {
                    panic!("IntToBits on non-integer");
                }
            }

            IntToFloat(arg) => {
                if let Value::Integer(v) = self.get(arg) {
                    Value::Number(v as f64)
                } else {
                    panic!("IntToBits on non-integer");
                }
            }

            IntToBits { width, arg } => {
                if let Value::Integer(v) = self.get(arg) {
                    let v = v as u64;
                    Value::Vector((0..width).rev().map(|bit| Value::Integer(((v >> bit) & 1) as i64) ).collect())
                } else {
                    panic!("IntToBits on non-integer");
                }
            }
            BitsToInt { width, arg, signed } => {
                if let Value::Vector(bits) = self.get(arg) {
                    assert_eq!(bits.len(), width);
                    let v = bits.iter().fold(0, |acc, x| {
                        match *x {
                            Value::Integer(bit) => (acc << 1) | (bit as u64),
                            _ => panic!("Expected bit")
                        }
                    });

                    let v = if signed {
                        ((v << (64-width)) as i64) >> (64-width) // sign-extend
                    } else {
                        v as i64
                    };

                    Value::Integer(v)
                } else {
                    return Value::Integer(0) //panic!("BitsToInt on non-vector")
                }
            }
        }

    }
}
