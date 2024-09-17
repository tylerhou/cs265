use bril_rs::{Code, Function, Instruction};

use crate::cfg::{ControlFlowGraph, Node};
/// Monotone framework
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

pub enum Direction {
    Forward,
    Backward,
}

// Associate each CFG node with a lattice value, and monotone transfer function.

pub trait Semilattice
where
    Self: PartialEq + Sized + Clone,
{
    fn join(&self, other: &Self) -> Self;
    fn bot(&self) -> Self;
    // fn top(&self) -> Self;

    fn leq(&self, other: &Self) -> bool {
        // a <= b iff a ⊔ b = b
        let join = self.join(other);
        join == *other
    }
}

#[derive(Clone)]
struct IntersectionSemilattice<'a, T> {
    universe: &'a HashSet<T>,
    set: HashSet<T>,
}

impl<T> Semilattice for HashSet<T>
where
    T: Eq + Hash + Clone,
{
    fn join(&self, other: &Self) -> Self {
        self.union(other).cloned().collect()
    }

    fn bot(&self) -> Self {
        HashSet::new()
    }

    fn leq(&self, other: &Self) -> bool {
        // assert!(self.universe == other.universe);
        // self.set.is_subset(&other.set)
        self.is_subset(other)
    }
}

impl<'a, T> PartialEq for IntersectionSemilattice<'a, T>
where
    T: Eq + Hash,
{
    fn eq(&self, other: &Self) -> bool {
        self.set == other.set
    }
}

impl<'a, T> Semilattice for IntersectionSemilattice<'a, T>
where
    T: Eq + Hash + Clone,
{
    fn bot(&self) -> Self {
        Self {
            universe: self.universe,
            set: self.universe.clone(),
        }
    }
    fn join(&self, other: &Self) -> Self {
        Self {
            universe: self.universe,
            set: self.set.intersection(&other.set).cloned().collect(),
        }
    }
    fn leq(&self, other: &Self) -> bool {
        self.set.is_superset(&other.set)
    }
}

/// 1. Lattice
/// 2. Initial value for initial node (i.e. entry or exit)
/// 3. Monotone transfer function for each node
pub struct MonotoneFramework<T, F> {
    pub direction: Direction,
    pub initial_value: T,
    pub transfer: F,
}

impl<T, F> MonotoneFramework<T, F>
where
    T: Semilattice,
    F: Fn(Node, &T) -> T,
{
    pub fn run(&self, cfg: &ControlFlowGraph) -> HashMap<Node, T> {
        let (flow, initial) = match self.direction {
            Direction::Forward => (&cfg.flow, Node::Entry),
            Direction::Backward => (&cfg.flow_r, Node::Exit),
        };
        let mut values: HashMap<Node, T> = HashMap::new();
        for node in flow.keys() {
            values.insert(*node, self.initial_value.bot());
        }
        values.insert(initial, self.initial_value.clone());
        let mut worklist: Vec<Node> = flow.keys().cloned().collect();
        while !worklist.is_empty() {
            let node = worklist.pop().unwrap();
            for next in &flow[&node] {
                let old = &values[next];
                let new = (self.transfer)(node, &values[&node]);
                if !new.leq(old) {
                    values.insert(*next, old.join(&new));
                    worklist.push(*next);
                }
            }
        }
        values
    }
}

pub fn live_variables(func: &Function) -> HashMap<Node, HashSet<String>> {
    type L = HashSet<String>;
    let direction = Direction::Backward;
    let initial_value: L = HashSet::new();
    // TODO: optimize using bitvectors
    let transfer = |x: Node, l: &L| -> L {
        match x {
            Node::Entry => l.clone(),
            Node::Exit => l.bot(),
            Node::Inst(offset) => {
                use Instruction::*;
                if let Code::Instruction(inst) = &func.instrs[offset] {
                    match inst {
                        Constant { dest, .. } => {
                            let mut l = l.clone();
                            l.remove(dest);
                            l
                        }
                        Value { dest, args, .. } => {
                            let mut l = l.clone();
                            l.remove(dest);
                            let gen: HashSet<String> = args.iter().cloned().collect();
                            l.union(&gen).cloned().collect()
                        }
                        Effect { args, .. } => {
                            let gen: HashSet<String> = args.iter().cloned().collect();
                            l.union(&gen).cloned().collect()
                        }
                    }
                } else {
                    unreachable!();
                }
            }
        }
    };
    let analysis = MonotoneFramework {
        direction,
        initial_value,
        transfer,
    };
    let cfg = ControlFlowGraph::new(func);
    analysis.run(&cfg)
}
