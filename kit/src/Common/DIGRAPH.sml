
(*$DIGRAPH*)
(* 
 * Directed graphs with an equivalence relation on the nodes           
 * 
 * -- Lars Birkedal and Mads Tofte
 *)

signature DIGRAPH =
  sig

    type 'info node  
    type 'info graph                    

    (* 
     * A directed graph is represented as a collection of nodes, each with an
     * out-set to other nodes. This out-set is a list, with one element for each
     * edge. Every node contains some info, determined by the user. (Hence graphs
     * are polymorphic in an 'info type.
     *   Moreover, every node is accessed through a union-find data structure.
     * This makes it possible to "union" (i.e., collapse) two nodes in 
     * the graph and automatically make all nodes that used to point 
     * to either of the two to point to the common canonical node.
     * The original nodes are still accessible, but one must use the 
     * "find" operation to get the canonical node. Some of the 
     * operations below assume that the argument is a canonical 
     * node (see comments below).
     *)

    val mk_graph : unit -> '_info graph 
      (* returns a reference to an empty graph *)

    val nodes: 'info graph -> 'info node list
      (* nodes(g) returns the nodes of g  - in an arbitrary order *)

    val subgraph: 'info node list -> 'info graph
      (* subgraph l  constructs a graph consisting of the nodes that occur in
         l plus all nodes that can be reached from nodes in l.
      *)

    val add_node_to_graph : 'info node * 'info graph -> 'info graph
      (* add_node_to_graph(n,g): adds the node n to the graph g *)
      (* n is assumed not already to be in g (this is not checked) *)

    val mk_node  : '_info -> '_info node 
      (* mk_node(i): makes a node with info i *)

    val mk_edge  : 'info node * 'info node -> unit (* nodes canonical *)
      (* mk_edge(n1,n2):  makes an edge _from_ n1 _to_ n2 *)
      (* WARNING: if g1 and g2 are disjoint graphs, n1 is a node from g1
         and n2 is a node from g2 then  mk_edge(n1,n2)  connects g1 and g2
         after which g1 by itself is no longer a graph; however, a new
         graph can be created from g1 thus: subgraph(nodes_of g1)
      *)

    val add_edges  : 'info node * 'info node list -> unit (* first argument canonical *)
      (* mk_edges(n1,l):  makes an edge _from_ n1 _to_ every element in l *)

    val out_of_node: 'info node -> 'info node list
      (* out_of_node(n):  list of targets of out-edges of n *)

    val eq_nodes : ('info node * 'info node) ->    (* nodes canonical *)
                   bool
      (* equality by reference *)

    val visit_all: 'info node -> unit
      (* visit_all(n): mark all unmarked nodes reachble from n *)

    val unvisit_all: 'info node -> unit
      (* unvisit_all(n): unmark all marked nodes reachble from n *)

    val unvisit: 'info node list -> unit

    val get_visited: 'info node -> bool ref

    val union    : ('info * 'info -> 'info) -> 
                   ('info node * 'info node) -> (* nodes canonical *)
                   'info node
                   
    (* union(info_combine)(n1,n2) uses the union of union-find collapse n1 and n2
       into a single node. The out-edges of the resulting node is the append of the
       out-edges of n1 and n2 (without removal of duplicate edges). The function parameter
       "info_combine" merges the user-defined info of n1 and n2 into a single piece of info,
       which is put on the resulting node.
    *)

    val union_graph : ('info * 'info -> 'info) -> 'info graph -> 'info node 
      (* Takes the union of all nodes in the given graph (typcically a scc) *)

    val union_without_edge_duplication : 
                   ('_info * '_info -> '_info) -> 
                   ('_info -> bool) -> 
                   ('_info node * '_info node) -> (* nodes canonical *)
                   '_info node
                   
    (* union_without_edge_duplication(info_combine)(n1,n2): like   union,
       but with removal of duplicate out-edges.
    *)

    val union_left : 
                   ('info * 'info -> 'info) -> 
                   ('info node * 'info node) -> (* nodes canonical *)
                   'info node
                   
    (* union_left(info_combine)(n1,n2): like   union,
       but with resulting nodes gets precisely the out-edges that n1 has
    *)

    val find     : 'info node -> 'info node             
    val get_info : 'info node -> 'info         (* node canonical *)
    and set_info : 'info node -> 'info -> unit (* node canonical *)
    val find_info : 'info node -> 'info        (* The node need not be canonical *)
    val find_visited: 'info node -> bool ref        (* The node need not be canonical *)
    val find_rep_and_info : 'info node -> 'info node * 'info  (* The argument node need 
                                                     not be canonical *)

    val topsort  : 'info node list -> 'info node list 
      (*
       * topsort(l): returns the nodes reachable from l, topologically sorted,
       * if l does not contain cycles. There are no repetitions in the list that is
       * produced. l may contain duplicates. If l contains cycles, the returned list
       * can be regarded as a topological sort of the graph obtained from subgraph(l)
       * by deleting back-edges.
       *)

    val dfs      : 'info graph -> 'info node list 
      (* dfs(g): returns the nodes of g in depth first search order *)

    val bottom_up_dfs      : 'info graph -> 'info node list 
      (* bottom_up_dfs(g): same as dfs, but leaf nodes are listed first *)

    type StringTree  (* from pretty-printer *)

    val scc      : ('_info -> StringTree) -> '_info graph -> '_info graph list 
      (* scc layout_info g:  returns the strongly connected components of g 
       * (the layout_info function is only used when tracing the algorithm)
       *)


    val bottom_up_eval : ('info * 'info list -> unit) -> 'info graph -> unit 
      (*
       * bottom_up_eval f g : evaluates the graph g bottom up using function f.
       * f is supposed to be applied to the info of a node n and the info of all 
       * the nodes in its out-set after bottom_up_eval of these nodes.
       * The graph is suppossed to acyclic (this can be ensured by proper use
       * of scc and union_graph).
       *)

(* DO NOT USE; mads
    val remove_cycles : '_info graph -> unit
      (*
         remove_cycles g: g is supposed to be a graph, with nodes listed
         in the order given by scc (i.e., bottom up - no non-trivial cycles).
         remove_cycles now removes all trivial cycles from g and also
         makes sure that for every pair (n1, n2) of nodes there is at most
         one edge from n1 to n2.
      *)
*)

    val quotient: ('_info -> StringTree) -> ('_info * '_info -> '_info) -> 
                  '_info graph -> '_info graph
    (* 
       quotient layout_info info_combine g: 

	First the sccs g are found, then the nodes in every scc are collapsed
	(using info_combine to merge info) and then redundant edges (including
	trivial cycles) are removed.  The operation is destructive: it updates
	the out sets of g.  The resulting graph will only have one node for
	every scc of g.

    *)

    val graft: 'info node list * 'info node -> 
               'info node * 'info node list
      (* (n', ns') = graft (ns,n): add those nodes 
         that can be reached from ns as children of n, provided they are not
         already reachable from n. The nodes of ns are known not to be transparent.
         After the call, n' is find n and ns' is the list of 
         nodes n'' for which and edge from n' to n'' was added.
      *)

    val multi_graft: ('info node  -> 'info node option) ->
                     ('info node * 'info node)list -> 
                     ('info node * 'info node list)list
     (*
      * pl = multi_graft bound_to_free_no_transparent (l):
      *
        The list l represents a substitution. We assume that   map #1 l 
        is a listing of nodes in bottom-up depth-first order and that there
        are no two elements of this list that belong to the same strongly connected
        component. Also, we assume that none of the nodes in   map #1 l  are
        reachable from any of the nodes in  map #2 l, i.e., we assume that
        the support of the substitution is disjoint from the range.

        The function bound_to_free_no_transparent takes as argument a
        list of nodes  reachable from  a node in 
        the domain of the substitution and returns the result of
        replaing the bound nodes with the nodes they are to be instantiated
        to and also removing transparent nodes.

        After the call, pl is a list of pairs of the form (n', ns'). We have
        map #1 pl = map #2 l and, for each pair (n', ns') in pl, we have that
        ns' is the list of nodes n'' for which an edge from n' to n'' was added.
     *)


    (* layout_node layout_leaf node   gives a pretty-printed version
       of node. No side-effects. *)

    val layout_node : ('info -> StringTree) -> 'info node -> StringTree



    (* layout_graph layout_leaf g   gives a pretty-printed version
       of every node in (the list) g, each node shown with its immediate
       successors. No side-effects.  *)

    val layout_graph : ('info -> StringTree) -> 'info graph -> StringTree

    (* layout_nodes: same as layout_graph *)

    val layout_nodes : ('info -> StringTree) -> 'info node list -> StringTree

    (* layout_nodes_deep layout_info l :
       lays out all the nodes in l, each node with all its successors.
       Shared nodes are printed with an "@" symbol. 
       Sets and clears visited field of nodes.
    *)

    val layout_nodes_deep : ('info -> StringTree) -> 'info node list -> StringTree

  end;

