use crate::monotone::live_variables;
use bril_rs::{Code, Function, Instruction};

/// Program optimizations

// TODO(altanh): this only does one "step" of DCE, it needs to be run until
// fixpoint. Should really do the "strongly live variables" analysis instead.
/// Dead code elimination.
/// - Removes instructions that are not used.
pub fn dce(func: &Function) -> Option<Function> {
    let lv = live_variables(func);
    let mut new_instrs = vec![];
    let mut did_work = false;
    for (i, instr) in func.instrs.iter().enumerate() {
        use Instruction::*;
        match instr {
            Code::Instruction(Constant { dest, .. }) => {
                if lv[&i.into()].contains(dest) {
                    new_instrs.push(instr.clone());
                } else {
                    did_work = true;
                }
            }
            Code::Instruction(Value { dest, .. }) => {
                if lv[&i.into()].contains(dest) {
                    new_instrs.push(instr.clone());
                } else {
                    did_work = true;
                }
            }
            _ => new_instrs.push(instr.clone()),
        }
    }
    if !did_work {
        None
    } else {
        Some(Function {
            name: func.name.clone(),
            args: func.args.clone(),
            instrs: new_instrs,
            return_type: func.return_type.clone(),
        })
    }
}
