(*
 * Directed graphs with an equivalence relation on the nodes
 *
 * -- Lars Birkedal and Mads Tofte
 *)


structure DiGraph: DIGRAPH  =
  struct
    structure PP = PrettyPrint
    structure UF = UnionFindPoly

    fun say s = TextIO.output(TextIO.stdOut, s ^ "\n")

    type StringTree = PP.StringTree

    (* ---------------------------------------------------------------------- *)
    (*    General Abbreviations                                               *)
    (* ---------------------------------------------------------------------- *)

    fun pp(t):string      = PP.flatten(PP.format(!Flags.colwidth, t))
    fun log s             = TextIO.output(!Flags.log,s ^ "\n")
    fun die errmsg        = Crash.impossible ("DiGraph." ^ errmsg)

    nonfix footnote
    fun footnote(x,y) = x
    infix footnote

    (* ---------------------------------------------------------------------- *)
    (*   Graph representation and operations on graphs                        *)
    (* ---------------------------------------------------------------------- *)

    (*
     * A directed graph is represented by a list of nodes, with out-sets for
     * each node. However, the user is prevented from using this fact (through
     * data abstraction). The reason is that not every list of nodes constitutes
     * a graph: the list must be "closed", i.e, whenever node n1 is in the list and
     * node n2 can be reached from n1 then n2 is also in the list.
     *   Another representation invariant is that in between graph operations are
     * invoked, all nodes must have status of being not visited.
     *)

    datatype 'info graphnode =
      GRAPHNODE of {info: 'info,
                    visited: bool ref,
                    df_num : int ref, (* depth-first search sequence number; used in scc *)
                    out : 'info graph (* out-edges *)
                    }
    withtype 'info node  = 'info graphnode UF.Element
         and 'info graph = 'info graphnode UF.Element list

    fun mk_graph () : '_info graph = []

    val add_node_to_graph : 'info node * 'info graph -> 'info graph = (op ::)

    fun mk_node (info : '_info) : '_info node =
      UF.mkElement (GRAPHNODE  {info = info,
                                visited = ref false,
                                df_num = ref 0,
                                out = []})

    fun mk_edge (n1: 'info node, n2 : 'info node) : unit =
      case UF.find_info n1 of
        GRAPHNODE{info,out,visited, df_num} =>
          UF.set_info n1 (GRAPHNODE{info=info,out=n2::out,visited=visited,df_num=df_num})

    fun add_edges (n1: 'info node, l : 'info node list) : unit =
      case UF.find_info n1 of
        GRAPHNODE{info,out,visited, df_num} =>
          UF.set_info n1 (GRAPHNODE{info=info,out=l @ out,visited=visited,df_num=df_num})

    fun eq_nodes (n1: 'info node, n2 : 'info node) : bool =
      UF.eq_Elements (n1,n2)

    fun union (info_combine : 'info * 'info -> 'info)
              (n1 : 'info node, n2 : 'info node) : 'info node =
      let
        fun node_combine (GRAPHNODE{info=info1,
                                    out=out1,...},
                          GRAPHNODE{info=info2,
                                    out=out2,
                                    df_num, visited})=
          GRAPHNODE{info=info_combine(info1,info2),
                    out=out1@out2,
                    visited = visited, df_num = df_num}
      in
        UF.union node_combine (n1,n2)
      end

    fun union_left (info_combine : 'info * 'info -> 'info)
              (n1 : 'info node, n2 : 'info node) : 'info node =
      let
        fun node_combine (GRAPHNODE{info=info1,
                                    out=out1,
                                    visited, df_num},
                          GRAPHNODE{info=info2,
                                    out=out2,...})=
          GRAPHNODE{info=info_combine(info1,info2),
                    out=out1,
                    visited = visited,
                    df_num = df_num}
      in
        UF.union node_combine (n1,n2)
      end

    fun find_info (n: 'info node): 'info =
         case UF.find_info n  of
            GRAPHNODE{info,...} => info

    fun find_rep_and_info(n: 'info node) : 'info node * 'info  =
         case UF.find_rep_and_info n of
            (n',GRAPHNODE{info,...}) => (n',info)

    fun union_graph (info_combine: 'info * 'info -> 'info) (* returns canonical node *)
                    (g as (n::g'): 'info graph) : 'info node =
        List.foldl (union info_combine) n g'
      | union_graph info_combine [] = die "union_graph"

    fun set_info (n : 'info node) (info' : 'info) :  unit =
      case UF.find_info n of
        GRAPHNODE{info,out,visited,df_num} =>
          UF.set_info n (GRAPHNODE{info=info',out=out,visited = visited, df_num = df_num})

    fun out_of_node (n : 'info node) : 'info graph =
      case UF.find_info n of GRAPHNODE{out,...} => out

    fun set_out_of_node(n: 'info node) (newout: 'info graph) =
      case UF.find_info n of
        GRAPHNODE{info,out,visited,df_num} =>
          UF.set_info n (GRAPHNODE{info = info, out = newout, visited=visited, df_num=df_num})

    (* ---------------------------------------------------------------------- *)
    (*   Selectors and Predicates on Attributes                               *)
    (* ---------------------------------------------------------------------- *)

    fun find_visited(n: 'info node) : bool ref =  (* n not necessarily cononical *)
      case UF.find_info n of
        GRAPHNODE{visited, ...} => visited

    fun set_visited(n: 'info node)(b: bool): unit =
      case UF.find_info n of
        GRAPHNODE{visited, ...} => visited:=b

    fun node_is_visited (n: 'info node) =
      case UF.find_info(n) of
        GRAPHNODE{visited as ref b, ...} => b

    fun find_dfnumber(n: 'info node) : int ref =
      case UF.find_info n of
        GRAPHNODE{df_num, ...} => df_num

    fun set_dfnumber(n: 'info node)(i: int): unit =
      case UF.find_info n of
        GRAPHNODE{df_num, ...} => df_num:=i

    fun visit g = List.app (fn node => set_visited node true) g

    fun visit_canonical g = List.app (fn node => set_visited node true) g

    fun unvisit g = List.app (fn node => set_visited node false) g

    (* reset_df_num g: set the depth-first number of every node in the list g to 0 *)
    fun reset_df_num g = List.app (fn node => set_dfnumber node 0) g

    fun union_without_edge_duplication
              (info_combine : '_info * '_info -> '_info)
              (visit_children: '_info -> bool)
              (n1 : '_info node, n2 : '_info node) : '_info node =
      let
        val visited_nodes = ref []
        fun onto([]: '_info node list, acc: '_info node list) = acc
          | onto((n: '_info node) :: rest, acc) =
              onto(rest,
                   let val r = find_visited n
                   in if !r then acc
                      else (r:= true;
                            visited_nodes:= n:: !visited_nodes;
                            if visit_children(find_info n)
                              then onto(out_of_node n, acc)
                            else n:: acc)
                   end)

        fun node_combine (GRAPHNODE{info=info1,
                                    out=out1,
                                    ...},
                          GRAPHNODE{info=info2,
                                    out=out2,
                                    df_num,visited}) =
          GRAPHNODE{info=info_combine(info1,info2),
                    out= (* visit all nodes in out2, then cons unvisited nodes
                             from out1 onto out2 and finally unvisit all nodes in
                             the result : *)
                         ((*visit out2;*)
                          let val result = onto(out1,onto(out2,[]))
                          in unvisit(!visited_nodes);
                             result
                          end),
                    df_num = df_num, visited=visited}
      in
        UF.union node_combine (n1,n2)
      end


    (* ---------------------------------------------------------------------- *)
    (*   Basic Pretty Printing                                                *)
    (* ---------------------------------------------------------------------- *)

    fun layout_node layout_info n : StringTree =
      case UF.find_info n of
        GRAPHNODE{info,...} => layout_info info

    fun layout_node_with_outset layout_info n : StringTree =
      (*
       * layout_node_with_outset layout_info n:
       *       produces a string tree representing the info of n plus edges
       *       to immediate successors of n
       *)
        case out_of_node n of
          [] => (* leaf *) layout_node layout_info n
        | ns =>
              PP.NODE{start = "", finish = "", childsep = PP.NOSEP, indent = 0,
                      children = [layout_node layout_info n,
                                  PP.NODE{start = "(", finish = ")", indent = 2, childsep = PP.RIGHT",",
                                          children = map (layout_node layout_info) ns}]}

    (* Pickler *)
    local
	val pu_boolref = Pickle.refOneGen Pickle.bool
	val pu_intref = Pickle.refOneGen Pickle.int
    in
	fun pu {maybeNewHashInfo: 'info -> int option,
		dummy: 'info,
		register: 'info node Pickle.pu -> 'info node Pickle.pu} (pu_info : 'info Pickle.pu)
	    : 'info node Pickle.pu * 'info node list Pickle.pu =
	let
	    val dummy : 'info graphnode = GRAPHNODE{info=dummy, visited=ref false,
						    df_num=ref 0, out=nil}
	    fun toInt (GRAPHNODE _) = 0

	    val pu_node : 'info graphnode Pickle.pu -> 'info node Pickle.pu
		= Pickle.cache "DiGraph.node" (register o Pickle.nameGen "DiGraph.node" o UF.pu dummy)

	    val pu_graph : 'info graphnode Pickle.pu -> 'info graph Pickle.pu
		= Pickle.cache "DiGraph.graph" (Pickle.nameGen "DiGraph.graph" o Pickle.listGen o pu_node)

	    val pu_graphnode =
		let
		    fun maybeHash (GRAPHNODE {info,...}) = maybeNewHashInfo info

		    fun fun_GRAPHNODE (pu : 'info graphnode Pickle.pu) : 'info graphnode Pickle.pu =
			Pickle.maybeNewHash maybeHash
			(Pickle.con1 (fn (i,v,d,t) => GRAPHNODE{info=i,visited=v,df_num=d,out=t})
			 (fn GRAPHNODE{info=i,visited=v,df_num=d,out=t} => (i,v,d,t))
			 (Pickle.tup4Gen0(pu_info,pu_boolref,pu_intref,pu_graph pu)))
		in
		    Pickle.dataGen("DiGraph.graphnode",toInt,[fun_GRAPHNODE])
		end
	in (pu_node pu_graphnode, pu_graph pu_graphnode)
	end
    end

    (* ---------------------------------------------------------------------- *)
    (*   Graph algorithms                                                     *)
    (* ---------------------------------------------------------------------- *)

    fun nodes g = g;


    fun visit_all (n: 'info node) : unit =
      (* mark all nodes reachable from n as visited *)
      let val r = find_visited n
      in if !r then ()
         else (r:= true; visit_all' (out_of_node n))
      end
    and visit_all' [] = ()
      | visit_all' (n::rest) = (visit_all n; visit_all' rest)


    fun unvisit_all (n: 'info node) : unit =
      (* mark all nodes reachable from n as visited *)
      let val r = find_visited n
      in if !r then (r:= false; unvisit_all' (out_of_node n))
         else ()
      end
    and unvisit_all' [ ] = ()
      | unvisit_all' (n::rest) = (
          unvisit_all n;
          unvisit_all' rest)

    fun bottom_up_dfs (g : 'info graph) : 'info node list =
      (*
       * bottom_up_dfs(g): returns the nodes of g in depth first search order, with
       * leaf nodes listed first. Actually, g need not be closed (i.e., the may be
       * nodes reachable from nodes in g that are not themselves listed in g) and there
       * may be repeated nodes in g. In any
       * case, the list returned lists all unmarked elements reachable from g and
       * then unmarks all unmarked nodes reachable from the result.
       *
       *)
      let
        fun search (n: 'info node, ns : 'info node list) : 'info node list =
          let
            val r = find_visited n
          in
            if !r then ns
            else (r := true;
                  search'(out_of_node n,n::ns))
          end
        and search'([], ns) = ns
          | search'(x::xs,ns) = search'(xs, search(x, ns))

        val result = search'(g,[])

      in
           unvisit result;
           result
      end

    fun dfs (g : 'info graph) : 'info node list =
      (*
       * dfs(g): returns the nodes of g in depth first search order
       *
       *)
      List.rev(bottom_up_dfs g)

    fun subgraph l =  dfs l



    fun topsort (l : 'info node list) : 'info node list =
      (*
       * topsort(l): returns the nodes reachable from l, topologically sorted,
       * if l does not contain cycles. If l contains cycles, the returned list
       * can be regarded as a topological sort of the graph obtained from subgraph(l)
       * by deleting back-edges.
       *
       * Remark: a topological sort is not the same as a listing in depth-first
       * search order, even when there are no cycles;
       * with the latter one is not sure that children come after
       * all their ancestors.
       *)
      let

        fun search' (b,[]) = b
          | search' (b,x::xs) =
              search'(search(x, b), xs)

        and search (n: 'info node, ns : 'info node list) : 'info node list =
          let
            val r = find_visited n
          in
            if !r then ns
            else (r := true;
                  n :: search'(ns,(out_of_node n)))
          end

        val result = search'([], l)

      in
           unvisit result;
           result
      end

    local (* Stack module inlined *)

      type '_a stack = ('_a list) ref

      exception EmptyStack

      fun empty () =
        (* Return an empty stack (must be typed somehow) *)
        (ref []) : '_a stack

      fun push (s : '_a stack, e: '_a) =
        (* Push an element onto the stack *)
        s := e :: (!s)

      fun peek (s : '_a stack) =
        (* Take a look at the top element of the stack *)
        (hd (!s))
        handle Hd => raise EmptyStack

      fun drop (s: '_a stack) =
        (* Remove top element from the stack *)
        (s := tl (!s); ())
        handle Tl => raise EmptyStack

      fun pop (s : '_a stack) =
        (* Return and remove the top element of the stack *)
        let
  	val res = peek s
        in
  	drop s;
  	res
        end

      fun clear (s : '_a stack) =
        (* Remove all elements from the stack *)
        s := !(empty ())
   in


    fun scc (layout_info : '_info -> StringTree) (g : '_info graph) : '_info graph list =
      (*
       * scc layout_info g: returns the strongly connected components of g.
       *
       * This algorithm is based on
       *   Aho, Hopcroft, Ullman: The Design and Analysis of Computer Algorithms,
       *   Addison Wesley, 1974, pp. 187--195
       *
       * Main changes are that the attribute dfnumber is used to check whether
       * a node is on the stack (<> 0) or not (dfnumber = 0), and that we have
       * avoided the attribute lowlink, by letting the searchc procedure (here
       * called lowlink) return lowlink.
       *)
      let
        (* Tracing stuff *)
       (* val TRACE = false
        val trace = if TRACE then log else fn x => ()*)

        val scc_list = ref ([] : '_info graph list)
        fun min_list(smallest,[]) = smallest
          | min_list(smallest:int,x::xs) =
              if x < smallest then min_list(x, xs) else min_list(smallest, xs)
        fun min'(x,y) = if y = 0 then x else Int.min (x, y)
        val count = ref 1
        fun inc r = r := !r + 1
        val stack : '_info node stack = empty ()

        fun lowlink(n: '_info node) =
          let
            (*val _ = trace ("lowlink : " ^ (pp(layout_node layout_info n)))*)
          in
            (find_visited n) := true;
            (find_dfnumber n) := !count;
            inc count;
            push(stack,n);
            let
              val low = !(find_dfnumber n)
              val low =
                min_list (low,(map (fn w =>
                                      let
                                      in
                                        if !(find_visited w) then
                                          min'(low,!(find_dfnumber w))
                                        else
                                          lowlink w
                                      end
                                        )
                                 (out_of_node n)))
              fun loop () =
                (* pop new scc from stack *)
                let
                  val x = pop stack
                  (*val _ = trace "pop"*)
                in (find_dfnumber x := 0;
                    scc_list := (x:: hd (!scc_list)) :: tl (!scc_list);
                    if eq_nodes(x,n) then ()
                    else loop ())
                end
            in
              (*trace ("lowlink -- body, n : " ^ (pp(layout_node layout_info n))
                     ^ " low : " ^ Int.toString low);*)
              if low = !(find_dfnumber n) then (* a new scc starts *)
                (scc_list := [] :: !scc_list;
                 loop())
              else
                ();
              low
            end
          end
      in
        reset_df_num g; (* sets the depth-first number of every node in the list g to 0 *)
           (* no nodes on the stack initially*)
        List.app (fn n =>
                    let
                    in
                      if !(find_visited n) then
                        ()
                      else
                        (lowlink(n); ())
                    end)
                   g;
        unvisit g;
        rev (!scc_list)
      end

    end (* local *)

    fun bottom_up_eval (f : 'info * 'info list -> unit) (g : 'info graph) : unit =
      (*
       * bottom_up_eval f g : evaluates the graph g bottom up using function f.
       * f is supposed to be applied to the info of a node n and the info of all
       * the nodes in its out-set after bottom_up_eval of these nodes.
       * The graph is suppossed to acyclic.
       *)
      let
        fun search (n: 'info node) : unit =
          let
            val r = find_visited n
          in
            if !r then
              ()
            else
              let
                val ns = out_of_node n
              in
                r := true;
                f (find_info n, map (fn n' => (search n'; find_info n')) ns)
              end
          end
      in
        List.app search g;(* Each node may potentially begin a new tree, so
                             * we have to evaluate for each node. Note however,
                             * that the graph in total is only traversed once,
                             * (ensured by the use of the mark visited)
                             *)
        unvisit g
      end

    fun remove_cycles (g: '_info graph) : unit =
      (* remove_cycles g: g is supposed to be a graph, with nodes listed
         in the order given by scc (i.e., bottom up - no non-trivial cycles
         and all nodes listed in g canonical).
         remove_cycles now removes all trivial cycles from g and also
         makes sure that for every pair (n1, n2) of nodes there is at most
         one edge from n1 to n2.
      *)
      let
        fun do_children(xs: '_info graph) =
            case xs of
              [] => []
            | (n::xs') =>
              let val r = find_visited n
              in
                  if !r then do_children xs'
                  else (r:= true;
                        n :: do_children xs')
              end
        fun do_node (x: '_info node) = (* x is canonical *)
          let
              val children = out_of_node x
          in
              List.app (fn n => find_visited n := false) children;
              find_visited x := true;
              set_out_of_node x (do_children children)
          end
      in
            List.app do_node g;
            unvisit g
      end;


    (* quotient: simplifies graph; see explanation in signature *)

    fun quotient (layout_info: '_info -> StringTree) (info_combine: '_info * '_info -> '_info)
                 (g: '_info graph) : '_info graph =
         let val g' = map (union_graph info_combine) (scc layout_info g)
             (* all nodes in g' canonical *)
         in remove_cycles g';
            g' (* all nodes in g' still canonical *)
         end

    fun graft (ns: 'info node list, n2: 'info node):
                'info node * 'info node list =
      (* add those nodes that can be reached from ns as
         children of n2, provided they are not already reachable from n2
         and are not transparent.
      *)
      let fun accum([], added_acc, append_acc) = (added_acc,append_acc)
            | accum(n::ns, added_acc, append_acc) =
               if !(find_visited n) then accum(ns,added_acc,append_acc)
               else ((*visit_all n;  to ensure that children are not added again *)
                     accum(ns, n::added_acc, n:: append_acc))
          val (added_children_of_n2, total_children_of_n2) =
               accum(ns, [],(visit_all n2; out_of_node n2))
      in
        set_out_of_node n2 total_children_of_n2;
        unvisit_all n2;
        (n2, added_children_of_n2)
      end



    fun multi_graft bound_to_free
                    (l: ('info node * 'info node) list):
                      ('info node * 'info node list)list =
       (* for explanation and assumptions: see signature *)
       let
          fun graft_one(n,m) = (* n is source node of the substitution,
                                  m is the target node*)
              let
                  fun accum([], acc) = acc
        	    | accum(n::ns, acc as (added_acc, append_acc)) =
	               let val child = n
        	       in
                	 case bound_to_free child of
	                   SOME n' =>
        	              if !(find_visited n') then accum(ns,(added_acc,append_acc))
                	      else accum(ns, (n'::added_acc, n':: append_acc))
	                 | NONE => (* UNION node, proceed to children *)
        	              accum(ns,
                	            accum(out_of_node child, acc))
	               end

	          val (added_children_of_m, total_children_of_m) =
        	       accum(out_of_node n, ([],(visit_all m; out_of_node m)))
              in
                  set_out_of_node m total_children_of_m;
                  unvisit_all m;
                  (m, added_children_of_m)
              end
       in map graft_one l
       end;

    (* ---------------------------------------------------------------------- *)
    (*   Pretty Printing of a Graph                                           *)
    (* ---------------------------------------------------------------------- *)

    fun layout_graph (layout_info: 'info -> StringTree)
                     (g: 'info graph) : StringTree =
         PP.NODE{start = "[", finish = "]", indent = 1, childsep = PP.RIGHT ",",
                 children = map (layout_node_with_outset layout_info) g}

    (* layout_nodes_deep layoutinfo g:
       g need not be a subgraph, i.e., it need not be closed
       under successors *)

    fun layout_nodes_deep (layout_info: 'info -> StringTree)
                     (g: 'info graph) : StringTree =
      let
	val debug = false
	fun maybe_under layout n =
	    if debug then [PP.NODE{start="(", finish = ")", indent = 1,
				   childsep = PP.RIGHT ",",
				   children = map layout (out_of_node n)}]
	    else nil
        fun layout(n: 'info node): StringTree =
          let
          in if !(find_visited n) then (* detected sharing; print node with "@" prefixed *)
                PP.NODE{start = "@", finish = "", indent = 1, childsep = PP.NOSEP,
                        children = layout_node layout_info n :: maybe_under layout n}
             else
               ((find_visited n):= true;
                case (out_of_node n) of
                  [] => layout_node layout_info n
                | _ =>
                    PP.NODE{start = "", finish = "", indent = 0, childsep = PP.NOSEP,
                        children = [layout_node layout_info n,
                                    PP.NODE{start="(", finish = ")", indent = 1,
                                            childsep = PP.RIGHT ",",
                                            children = map layout (out_of_node n)}]}
               )
          end
      in
        (case g of
          [one] => layout one
         | _ => PP.NODE{start = "[", finish = "]", indent = 1, childsep = PP.RIGHT ",",
                 children = map layout g})
         footnote List.app unvisit_all g
      end

    fun layout_nodes l = layout_graph l;
      (* not as redundant as it looks: to the user, graphs and node lists
         are not the same *)

  end;

(*

functor TestDiGraph(structure DiGraph : DIGRAPH
		    structure Flags   : FLAGS
		    structure PP      : PRETTYPRINT
		    sharing type DiGraph.StringTree = PP.StringTree
		      ) (* : sig end *) =
  struct

    fun etest(found, expected) =
        if found = expected then "OK"
        else "NOT OK"

fun say s = TextIO.output(TextIO.stdOut, s^"\n")
fun etest(label,found,expected) =
 say(label ^ (if expected = found then " OK" else " ****** NOT OK *******" ^
"\n found    :    " ^ found ^
"\n expected :    " ^ expected));

    open DiGraph

    fun pp(t):string      = PP.flatten(PP.format(!Flags.colwidth, t))
    fun log s             = TextIO.output(!Flags.log,s ^ "\n")



    (* Test Example from Aho, Hopcroft, Ullman, p. 187. *)
    val graph : int ref graph ref = ref (mk_graph ())
    fun mk_node_int(i) =
      let val n =  mk_node i
      in graph := add_node_to_graph(n,!graph);
         n
      end

    val v8 = mk_node_int (ref 8)
    val v7 = mk_node_int (ref 7)
    val v6 = mk_node_int (ref 6)
    val v5 = mk_node_int (ref 5)
    val v4 = mk_node_int (ref 4)
    val v3 = mk_node_int (ref 3)
    val v2 = mk_node_int (ref 2)
    val v1 = mk_node_int (ref 1)
    val _  = List.app mk_edge [(v1,v5),
                                 (v1,v4),
                                 (v1,v2),
                                 (v2,v4),
                                 (v2,v3),
                                 (v3,v1),
                                 (v4,v3),
                                 (v5,v4),
                                 (v6,v8),
                                 (v6,v7),
                                 (v7,v5),
                                 (v8,v7),
                                 (v8,v6),
                                 (v8,v4)]

    val graph = !graph

    val layout_info = PP.LEAF o Int.toString o (op !)

    fun test_quotient() =
      (log "graph in depth first search order:";
       log ("\t" ^ pp(layout_graph layout_info graph));

       log "graph in depth first search order, after quotient:";
       log ("\t" ^ pp(layout_graph layout_info (quotient layout_info (fn (i1,i2) => i2) graph)))
      )


    fun do_test() =
      (log "graph in depth first search order:";
       log ("\t" ^ pp(layout_graph layout_info graph));

       let
         val sccs:int ref graph list  = scc layout_info graph
       in
         log "strongly connected components, each in depth first search order:";
         List.app (fn g => log ("\t" ^ pp(layout_graph layout_info g)))
         sccs;
         let
           val compressed_sccs: int ref node list  = (map (union_graph (fn (i1,i2) => i2)) sccs)
           val compressed_sccs_as_graph = subgraph compressed_sccs
           val max_list = List.foldL' Int.max

         in
           log "compressed graph (tree of sccs) in depth first search order:";
           log ("\t" ^ pp(layout_graph layout_info compressed_sccs_as_graph));

           remove_cycles compressed_sccs_as_graph;

           log "compressed graph (tree of sccs) in depth first search order, after removal of cycles and repeated edges:";
           log ("\t" ^ pp(layout_graph layout_info compressed_sccs_as_graph));

           log "representative for each compressed scc";
           List.app (fn n => log ("\t" ^ pp(layout_node layout_info n)))
           compressed_sccs;

           (* The resulting graph should have
            *  nodes : 1, 6, and 7
            *  edges : [7,1], [6,1], [6,7]
            * Thus the following bottom up evaluation should change the info
            * of 6 to 7.
            *)

           bottom_up_eval (fn (i, is) => i := max_list (map (op !) (i::is))) graph;
           log "bottom up evaluated graph in depth first search order";
           log ("\t" ^ pp(layout_graph layout_info graph))

         end
       end)

     fun test_graft() =
       let
           fun transparent (ref s) = ord s >= ord "A" andalso ord s <= ord "Z"

           fun build_graph() : string ref graph * string ref node * string ref node * string ref node * string ref node * string ref node * string ref node * string ref node * string ref node=
               let val graph:string ref graph ref = ref(mk_graph())
                   fun mk_node_string(i)  =
                     let val n =  mk_node i
                     in graph := add_node_to_graph(n,!graph);
                        n
                     end
                   val [a,b,c,d,e,f,g,h] = map (mk_node_string o ref)(explode "abcdefgh")
                   val _ = List.app mk_edge [(a,b),(a,c), (c,e),(c,g), (e,g), (d,e),(d,f),(e,h)]
               in  (! graph, a,b,c,d,e,f,g,h)
               end
           fun layout_info (ref s) = PP.LEAF s
       in
           etest("original graph: ",
                      pp(layout_graph layout_info (#1(build_graph()))),
                      "[h,g,f,e(h,g),d(f,e),c(g,e),b,a(c,b)]");
           let val (graph, a,b,c,d,e,f,g,h) = build_graph()
           in graft transparent(d, [a]);
              etest("graft([a], subgraph [d]) =>" ,
                    pp(layout_graph layout_info graph),
                    "[h,g,f,e(h,g),d(a,f,e),c(g,e),b,a(c,b)]")
           end;
           let val (graph, a,b,c,d,e,f,g,h) = build_graph()
           in graft transparent(d,[a,c]);
                       etest("graft([a,c], subgraph [d]) => ",
                             pp(layout_graph layout_info graph),
                             "[h,g,f,e(h,g),d(a,f,e),c(g,e),b,a(c,b)]")
           end;
           let val (graph, a,b,c,d,e,f,g,h) = build_graph()
           in graft transparent(d,[a,b,c]);
             etest("graft([a,b,c], subgraph [d]) => ",
                   pp(layout_graph layout_info graph),
                   "[h,g,f,e(h,g),d(a,f,e),c(g,e),b,a(c,b)]")
           end;
           let val (graph, a,b,c,d,e,f,g,h) = build_graph()
           in graft transparent (d, [b,c,g]);
              etest("graft([b,c,g], subgraph [d]) => ",
                 pp(layout_graph layout_info graph),
                 "[h,g,f,e(h,g),d(c,b,f,e),c(g,e),b,a(c,b)]")
           end;
           let val (graph, a,b,c,d,e,f,g,h) = build_graph()
           in graft transparent(d,[g]);
                       etest("graft([g], subgraph [d]) => ",
                             pp(layout_graph layout_info graph),
                             "[h,g,f,e(h,g),d(f,e),c(g,e),b,a(c,b)]")
           end
       end
     fun test_multi_graft() =
       let
           fun transparent (ref s) = ord s >= ord "A" andalso ord s <= ord "Z"
           fun layout_info (ref s) = PP.LEAF s
           fun show_graph(text, g) =
                log(text ^ pp(layout_graph layout_info g))
           fun show_graph'(text, g) = log(text ^ g)
           fun etest expected g =
              let val found =pp(layout_graph layout_info g)
              in
               if expected = found then " OK" else
                 (" NOT OK\nfound : " ^ found)
              end
           fun test1() : unit =
               let val graph:string ref graph ref = ref(mk_graph())
                   fun mk_node_string(i)  =
                     let val n =  mk_node i
                     in graph := add_node_to_graph(n,!graph);
                        n
                     end
                   val [a,b,c,d] = map (mk_node_string o ref)(explode "abcd")
                   val _ = List.app mk_edge [(a,b),(a,c)]
                   val graph = ! graph
                   fun bound_to_free x =
                       if eq_nodes(x, a) then d else x
               in show_graph("test1: graph = ", graph);
                  multi_graft bound_to_free transparent[(a,d)];
                  show_graph'("test1: multi_graft[(a,d)] => ", etest "[d(b,c),c,b,a(c,b)]" graph)
               end
           fun test2() : unit =
               let val graph:string ref graph ref = ref(mk_graph())
                   fun mk_node_string(i)  =
                     let val n =  mk_node i
                     in graph := add_node_to_graph(n,!graph);
                        n
                     end
                   val [a,b,c,d,e,f] = map (mk_node_string o ref)(explode "abcdef")
                   val _ = List.app mk_edge [(a,f),(a,b),(c,d),(d,e)]
                   val graph = ! graph
                   fun bound_to_free x =
                       if eq_nodes(x, a) then d else
                       if eq_nodes(x,b) then e else x
               in show_graph("test2: graph = ", graph);
                  multi_graft bound_to_free transparent[(b,e),(a,c)];
                  show_graph'("test2: multi_graft[(b,e),(a,c)] => ", etest "[f,e,d(e),c(f,d),b,a(b,f)]" graph)
               end
           fun test3() : unit =
               let val graph:string ref graph ref = ref(mk_graph())
                   fun mk_node_string(i)  =
                     let val n =  mk_node i
                     in graph := add_node_to_graph(n,!graph);
                        n
                     end
                   val [a,B,c,d,e,f] = map (mk_node_string o ref)(explode "aBcdef")
                   val _ = List.app mk_edge [(a,B),(B,f),(B,c),(d,e)]
                   val graph = ! graph
                   fun bound_to_free x =
                       if eq_nodes(x, a) then d else
                       if eq_nodes(x,c) then e else x
               in show_graph("test3: graph = ", graph);
                  multi_graft bound_to_free transparent[(c,e),(a,d)];
                  show_graph'("test3: multi_graft[(c,e),(a,d)] => ", etest "[f,e,d(f,e),c,B(c,f),a(B)]" graph)
               end
           fun test4() : unit =
               let val graph:string ref graph ref = ref(mk_graph())
                   fun mk_node_string(i)  =
                     let val n =  mk_node i
                     in graph := add_node_to_graph(n,!graph);
                        n
                     end
                   val [a,B,c,d,e,f,h] = map (mk_node_string o ref)(explode "aBcdefh")
                   val _ = List.app mk_edge [(a,B),(B,f),(B,c),(d,e),(e,h),(h,d)]
                   val graph = ! graph
                   fun bound_to_free x =
                       if eq_nodes(x,a) then d else
                       if eq_nodes(x,c) then e else x
               in show_graph("test4: graph = ", graph);
                  multi_graft bound_to_free transparent[(c,e),(a,d)];
                  show_graph'("test4: multi_graft[(c,e),(a,d)] => ", etest
                    "[h(d),f,e(h),d(f,e),c,B(c,f),a(B)]"graph)
               end
       in
           test1(); test2(); test3(); test4()
       end
  end;

(*
 * Build code for testing
 *   'make DoDiGraphTest' builds the test structure
 *   and 'Test.do_test()' then performs the test
 *)

functor DiGraphTest' () =
struct

(*$DiGraphTest:
      DiGraph Flags BasicIO Crash Report PrettyPrint Stack UnionFindPoly Test
*)
structure BasicIO = BasicIO();
structure Crash = Crash(structure BasicIO = BasicIO);
structure Flags = Flags(structure Crash = Crash);
structure Report = Report(structure BasicIO = BasicIO);
structure PP = PrettyPrint(structure Report = Report
                           structure Crash = Crash
                           structure Flags = Flags);
structure UF = UF_with_path_halving_and_union_by_rank();
structure Stack = Stack();
structure DiGraph = DiGraph(structure UF = UF
                            structure Stack = Stack
                            structure PP = PP
                            structure Flags = Flags
                            structure Crash = Crash)
structure Test = DiGraphTest(structure DiGraph = DiGraph
			     structure Flags = Flags
			     structure PP = PP);

val _ = Test.do_test();
val _ = Test.test_graft();
val _ = Test.test_multi_graft();

end (* DiGraphTest' *)

*)
