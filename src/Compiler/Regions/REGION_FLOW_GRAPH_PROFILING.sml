
signature REGION_FLOW_GRAPH_PROFILING =
  sig

    (* This module builds a region flow graph used 
       as an extra tool when profiling programs.

       The graph is generated on the expression found
       in MUL_EXP.

       The following flags from the interact environment
       directs the generation of the region flow graph
       and whether it is exported or not.

	 "region_profiling"
	 "generate_lambda_code_with_program_points"
	 "generate_vcg_graph"
    *)

    type place
    type 'a at
    type phsize
    type StringTree
    type pp = int
      
    val reset_graph  : unit -> unit
    val add_nodes    : ((place*phsize) list) * string -> unit
    (* Add edge (p_formal -----> p_actual) with storage mode *)
    (* add_edges((formals,func_name),actuals) *)
    val add_edges    : (((place*phsize) list)*string)*((place*pp)at list) -> unit
    val layout_graph : unit -> StringTree
    val export_graph : TextIO.outstream -> unit
  end


