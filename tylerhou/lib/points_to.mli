module Points_to : sig
  type target =
    | Exactly of Var.Set.t
    | All_memory_locations
  [@@deriving compare, equal, sexp_of]

  type t = target Var.Map.t [@@deriving compare, equal, sexp_of]
end

module Analysis : Dataflow.S with type lattice := Points_to.t


module Analysis_by_program_point : sig
  type t = Analysis.Block.instr_with_lattice Program_point.Map.t
  [@@deriving compare, equal, sexp_of]
end

val analyze : Bril.Func.t -> Analysis_by_program_point.t
