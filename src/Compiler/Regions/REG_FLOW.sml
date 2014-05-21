(*$REG_FLOW*)

(* Region Flow Analysis: first pass of Storage Mode Analysis *)

signature REG_FLOW =
sig
   type place and mul and qmularefset and effect and ('a,'b,'c)LambdaPgm

   val mk_graph: (place, place*mul, qmularefset ref)LambdaPgm -> unit

   (* reachable_in_graph_with_insertion finds all the region variables
      that can be reached from a given node (of either kind) in the graph *)

   val reachable_in_graph_with_insertion : effect -> place list

   (* reachable_with_insertion(nodelist): find all the region variables
      which can be reached from a node which is a member of nodelist.
   *)

   val reachable_with_insertion: effect list -> effect list


end
   
