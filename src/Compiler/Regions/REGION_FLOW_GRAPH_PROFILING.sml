signature REGION_FLOW_GRAPH_PROFILING =
sig
  type place
  type 'a at
  type phsize
  type StringTree
  type pp = int
  val reset_graph : unit -> unit
  val add_nodes : (place * phsize) list * string -> unit
  (* An edge connects a formal region to its actual instantiation. *)
  val add_edges : ((place * phsize) list * string) * (place * pp) at list -> unit
  val layout_graph : unit -> StringTree
end
