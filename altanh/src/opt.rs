use crate::monotone::{conditional_constant, observable_variables, ConstantLattice};
use bril_rs::{Code, ConstOps, EffectOps, Function, Instruction, Literal, Type};

/// Program optimizations

/// Dead code elimination.
/// - Removes instructions that are not used.
pub fn dce(func: &Function) -> Function {
    let lv = observable_variables(func);
    let mut new_instrs = vec![];
    for (i, instr) in func.instrs.iter().enumerate() {
        use Instruction::*;
        match instr {
            Code::Instruction(Constant { dest, .. }) => {
                if lv[&i.into()].contains(dest) {
                    new_instrs.push(instr.clone());
                }
            }
            Code::Instruction(Value { dest, .. }) => {
                if lv[&i.into()].contains(dest) {
                    new_instrs.push(instr.clone());
                }
            }
            _ => new_instrs.push(instr.clone()),
        }
    }
    if new_instrs.is_empty() {
        new_instrs.push(Code::Instruction(Instruction::Effect {
            args: vec![],
            funcs: vec![],
            labels: vec![],
            op: EffectOps::Nop,
        }));
    }
    Function {
        name: func.name.clone(),
        args: func.args.clone(),
        instrs: new_instrs,
        return_type: func.return_type.clone(),
    }
}

/// Condtional constant propagation.
pub fn cc(func: &Function) -> Function {
    let cc_results = conditional_constant(func);
    let mut new_instrs: Vec<Code> = vec![];
    for (i, code) in func.instrs.iter().enumerate() {
        // if let Some((env, r)) = cc_results.get(&i.into()) {
        if let Code::Instruction(instr) = code {
            let (env, r) = &cc_results[&i.into()];
            // if i is dead, skip
            if !r.contains(&i.into()) {
                continue;
            }
            let mut concretize_args = |args: Vec<String>| {
                let mut new_args = vec![];
                for (j, arg) in args.into_iter().enumerate() {
                    // TODO(altanh): this is sussy, should ensure global
                    // uniqueness tbh
                    let new_arg: String = format!("__cc{}", j);
                    match env.get(&arg) {
                        Some(ConstantLattice::Int(z)) => {
                            new_instrs.push(Code::Instruction(Constant {
                                dest: new_arg.clone(),
                                op: ConstOps::Const,
                                const_type: Type::Int,
                                value: Literal::Int(*z),
                            }));
                            new_args.push(new_arg);
                        }
                        Some(ConstantLattice::Bool(b)) => {
                            new_instrs.push(Code::Instruction(Constant {
                                dest: new_arg.clone(),
                                op: ConstOps::Const,
                                const_type: Type::Bool,
                                value: Literal::Bool(*b),
                            }));
                            new_args.push(new_arg);
                        }
                        _ => {
                            new_args.push(arg.clone());
                        }
                    }
                }
                new_args
            };
            use bril_rs::Instruction::*;
            // If any arguments are constant, emit a constant instruction and use
            // that instead
            match instr.clone() {
                Value {
                    args,
                    dest,
                    funcs,
                    labels,
                    op,
                    op_type,
                } => {
                    let args = concretize_args(args);
                    new_instrs.push(Code::Instruction(Value {
                        args,
                        dest,
                        funcs,
                        labels,
                        op,
                        op_type,
                    }));
                }
                Effect {
                    args,
                    funcs,
                    labels,
                    op,
                } => {
                    if op == EffectOps::Branch {
                        match env.get(&args[0]) {
                            Some(ConstantLattice::Bool(true)) => {
                                // Jump directly to true branch
                                new_instrs.push(Code::Instruction(Effect {
                                    args: vec![],
                                    funcs: vec![],
                                    labels: vec![labels[0].clone()],
                                    op: EffectOps::Jump,
                                }));
                            }
                            Some(ConstantLattice::Bool(false)) => {
                                // Jump directly to false branch
                                new_instrs.push(Code::Instruction(Effect {
                                    args: vec![],
                                    funcs: vec![],
                                    labels: vec![labels[1].clone()],
                                    op: EffectOps::Jump,
                                }));
                            }
                            _ => {
                                // Do nothing
                                new_instrs.push(Code::Instruction(instr.clone()));
                            }
                        }
                    } else {
                        let args = concretize_args(args);
                        new_instrs.push(Code::Instruction(Effect {
                            args,
                            funcs,
                            labels,
                            op,
                        }))
                    }
                }
                instr => new_instrs.push(Code::Instruction(instr)),
            }
        } else {
            // Label
            new_instrs.push(code.clone());
        }
    }
    Function {
        name: func.name.clone(),
        args: func.args.clone(),
        instrs: new_instrs,
        return_type: func.return_type.clone(),
    }
}
