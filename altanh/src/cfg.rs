use std::{
    collections::{HashMap, HashSet},
    io,
};

use crate::util::{next_inst, resolve_labels};
use bril_rs::{Code, EffectOps, Function, Instruction};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Node {
    Entry,
    Exit,
    Inst(usize),
}

impl From<usize> for Node {
    fn from(value: usize) -> Self {
        Node::Inst(value)
    }
}

pub struct ControlFlowGraph<'a> {
    pub flow: HashMap<Node, HashSet<Node>>,
    pub flow_r: HashMap<Node, HashSet<Node>>,
    pub func: &'a Function,
    pub labels: HashMap<String, usize>,
}

impl<'a> ControlFlowGraph<'a> {
    pub fn new(func: &'a Function) -> Self {
        let mut cfg = ControlFlowGraph {
            flow: HashMap::new(),
            flow_r: HashMap::new(),
            func,
            labels: resolve_labels(func),
        };

        // Find the first instruction
        let first_inst: Node = next_inst(0, func).map_or(Node::Exit, Node::Inst);

        cfg.flows(Node::Entry, first_inst);

        for (i, inst) in func.instrs.iter().enumerate() {
            if let Code::Instruction(inst) = inst {
                match inst {
                    Instruction::Effect {
                        labels,
                        op: EffectOps::Jump | EffectOps::Branch,
                        ..
                    } => {
                        for target in labels {
                            let target: Node = if cfg.labels.contains_key(target) {
                                cfg.labels[target].into()
                            } else {
                                Node::Exit
                            };
                            cfg.flows(i, target);
                        }
                    }
                    Instruction::Effect {
                        op: EffectOps::Return,
                        ..
                    } => cfg.flows(i, Node::Exit),
                    _ => {
                        let next_inst: Node = next_inst(i + 1, func).map_or(Node::Exit, Node::Inst);
                        cfg.flows(i, next_inst);
                    }
                }
            }
        }

        cfg
    }

    pub fn flows<S, T>(&mut self, from: S, to: T)
    where
        S: Into<Node>,
        T: Into<Node>,
    {
        let from: Node = from.into();
        let to: Node = to.into();
        if let None = self.flow.get(&from) {
            self.flow.insert(from, HashSet::new());
            self.flow_r.insert(from, HashSet::new());
        }
        if let None = self.flow.get(&to) {
            self.flow.insert(to, HashSet::new());
            self.flow_r.insert(to, HashSet::new());
        }
        self.flow.get_mut(&from).unwrap().insert(to);
        self.flow_r.get_mut(&to).unwrap().insert(from);
    }

    pub fn dot<F>(&self, f: &mut F) -> io::Result<()>
    where
        F: io::Write,
    {
        let node_id = |x: Node| match x {
            Node::Entry => "entry".to_string(),
            Node::Exit => "exit".to_string(),
            Node::Inst(index) => index.to_string(),
        };

        let node_label = |x: Node| match x {
            Node::Entry | Node::Exit => node_id(x),
            Node::Inst(index) => self.func.instrs[index].to_string(),
        };

        writeln!(f, "digraph {{")?;

        for node in self.flow.keys() {
            writeln!(f, "  {} [label=\"{}\"];", node_id(*node), node_label(*node))?;
        }

        for (src, dsts) in &self.flow {
            for dst in dsts {
                writeln!(f, "  {} -> {};", node_id(*src), node_id(*dst))?;
            }
        }

        writeln!(f, "}}")
    }

    pub fn resolve(&self, label: &str) -> Node {
        if let Some(index) = self.labels.get(label) {
            Node::Inst(*index)
        } else {
            Node::Exit
        }
    }
}
