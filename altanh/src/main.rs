use altanh::cfg::ControlFlowGraph;
use bril_rs::load_program;

const WRITE_CFG: bool = false;

fn main() {
    let prog = load_program();

    let mut new_funcs = vec![];

    for func in &prog.functions {
        let cfg = ControlFlowGraph::new(func);

        if WRITE_CFG {
            // open file for writing using the function name
            let mut file = std::fs::File::create(format!("dot/{}.dot", func.name)).unwrap();
            // write the graph to the file
            cfg.dot(&mut file).unwrap();
        }

        let func = altanh::opt::cc(&func);
        let func = altanh::opt::dce(&func);

        if WRITE_CFG {
            let cfg = ControlFlowGraph::new(&func);
            // open file for writing using the function name
            let mut file = std::fs::File::create(format!("dot/{}_opt.dot", &func.name)).unwrap();
            // write the graph to the file
            cfg.dot(&mut file).unwrap();
        }

        new_funcs.push(func);
    }

    // print the optimized program
    let new_prog = bril_rs::Program {
        functions: new_funcs,
    };
    bril_rs::output_program(&new_prog);
}
