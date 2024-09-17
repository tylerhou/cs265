use altanh::cfg::ControlFlowGraph;
use altanh::monotone::live_variables;
use bril_rs::load_program;

const WRITE_CFG: bool = false;

fn main() {
    let prog = load_program();

    let mut new_funcs = vec![];

    for func in &prog.functions {
        let cfg = ControlFlowGraph::new(func);
        let live_vars = live_variables(func);

        if WRITE_CFG {
            // open file for writing using the function name
            let mut file = std::fs::File::create(format!("dot/{}.dot", func.name)).unwrap();
            // write the graph to the file
            cfg.dot(&mut file).unwrap();
        }

        // run DCE until fixpoint
        let mut new_func = func.clone();
        while let Some(f) = altanh::opt::dce(&new_func) {
            new_func = f;
        }

        if WRITE_CFG {
            let cfg = ControlFlowGraph::new(&new_func);
            // open file for writing using the function name
            let mut file =
                std::fs::File::create(format!("dot/{}_dce.dot", &new_func.name)).unwrap();
            // write the graph to the file
            cfg.dot(&mut file).unwrap();
        }

        new_funcs.push(new_func);
    }

    // print the optimized program
    let new_prog = bril_rs::Program {
        functions: new_funcs,
    };
    bril_rs::output_program(&new_prog);
}
