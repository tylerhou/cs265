use std::collections::HashMap;

use bril_rs::*;

/// Returns the next non-label instruction starting from index `i` inclusive.
pub fn next_inst(mut i: usize, func: &Function) -> Option<usize> {
    while i < func.instrs.len() {
        match &func.instrs[i] {
            Code::Instruction(_) => return Some(i),
            _ => i += 1,
        }
    }
    None
}

/// Resolve labels to instruction offsets. Labels are resolved to the next
/// non-label instruction. If a label is the last instruction, it is not resolved.
pub fn resolve_labels(func: &Function) -> HashMap<String, usize> {
    let mut queue: Vec<String> = vec![];
    let mut map: HashMap<String, usize> = HashMap::new();
    for (i, code) in func.instrs.iter().enumerate() {
        match code {
            Code::Label { label } => queue.push(label.clone()),
            Code::Instruction(_) => {
                // resolve queued labels
                while let Some(label) = queue.pop() {
                    map.insert(label, i);
                }
            }
        }
    }
    map
}
