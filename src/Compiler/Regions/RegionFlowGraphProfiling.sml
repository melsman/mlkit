
structure RegionFlowGraphProfiling : REGION_FLOW_GRAPH_PROFILING =
  struct
    val region_paths : (int * int) list ref = ref []
    structure PP = PrettyPrint
    type place = Effect.place
    type 'a at = 'a AtInf.at
    type phsize = PhysSizeInf.phsize
    type pp = PhysSizeInf.pp
    type StringTree = PP.StringTree

    fun die errmsg   = Crash.impossible ("RegionFlowGraphProfiling." ^ errmsg)

    val line = Report.line
    val // = Report.//
    infix //
    fun warn report = Flags.warn (line "from module RegionFlowGraphProfiling:"
				  // report)

    fun get_rho_key place =
      if Effect.is_rho place then
	Effect.key_of_eps_or_rho place
      else
	die "get_rho_key"

    fun show_phsize (PhysSizeInf.INF) = "inf"
      | show_phsize (PhysSizeInf.WORDS n) = Int.toString n

    fun show_atkind (AtInf.ATTOP _) = "attop"
      | show_atkind (AtInf.ATBOT _) = "atbot"
      | show_atkind (AtInf.SAT _) = "sat"

    fun get_info_actual (AtInf.ATTOP i) = i
      | get_info_actual (AtInf.ATBOT i) = i
      | get_info_actual (AtInf.SAT i) = i

    (* Ordering for storage modes. ATBOT < ATTOP and SAT < ATTOP. *)
    fun maxAtKind ak1 (SOME ak2) =
      (case (ak1, ak2) of
	 (AtInf.ATBOT _, AtInf.ATBOT _) => ak1
       | (AtInf.ATBOT _, AtInf.ATTOP _) => ak2
       | (AtInf.ATTOP _, AtInf.ATBOT _) => ak1
       | (AtInf.SAT _,   AtInf.SAT _)   => ak1
       | (AtInf.SAT _,   AtInf.ATTOP _) => ak2
       | (AtInf.ATTOP _, AtInf.SAT _)   => ak1
       | (AtInf.ATTOP _, AtInf.ATTOP _) => ak1
       | (AtInf.ATBOT _, AtInf.SAT _)   => ak2
       | (AtInf.SAT _,   AtInf.ATBOT _) => ak1)
      | maxAtKind ak1 NONE = ak1

    (*--------------------------------------------------------------------------------------*
     * Generation of Region Flow Graphs                                                     *
     *  When building the region flow graph, primitives like explode do not have any actual *
     *  region variables because all primitives are inlined to f.ex. prim(explode,...)      *
     *  by the lambda optimizer.                                                            *
     *--------------------------------------------------------------------------------------*)

    structure DiGraphScc = DiGraphScc(struct
					  type nodeId = int
					  type info = int * string * string
					  type edgeInfo = (place*pp) AtInf.at (* We put the storage mode on the edge. *)
					  fun lt (a:nodeId, b) = (a<b)
					  fun getId ((id,s,size):info) = id
					  val pu = Pickle.int
                                          structure Map = IntFinMap
				      end)
    local
      val region_flow_graph : DiGraphScc.graph ref = ref (DiGraphScc.mkGraph())
    in
      fun reset_graph () = region_flow_graph := DiGraphScc.mkGraph()
      fun get_graph () = !region_flow_graph
    end (*local*)

    (* Add node (rho:int, string:string, rho_size:string) to graph. *)
    fun add_nodes ([], str) = ()
      | add_nodes ((p:place, phs:phsize)::rest,str) =
      (DiGraphScc.addNodeWithUpdate (DiGraphScc.mkNode(get_rho_key p, str, show_phsize phs)) (get_graph());
       add_nodes (rest,str))

    (* Add edge (p_formal -----> p_actual) with storage mode *)
    fun add_edges  (([],str),[]) = ()
      | add_edges (((p_formal:place, phs_formal:phsize)::rest_formals,str),actual::rest_actuals) =
      let
	val rhoNode1 =
	  case DiGraphScc.findNodeOpt (get_rho_key p_formal) (get_graph()) of
	    SOME n1 => (DiGraphScc.setInfoNode n1 (get_rho_key p_formal, str, show_phsize phs_formal);n1)
	  | NONE =>
	      let
		val n1 = DiGraphScc.mkNode (get_rho_key p_formal, str, show_phsize phs_formal)
	      in
		DiGraphScc.addNode n1 (get_graph());
		n1
	      end
	val (p_actual:place,_) = get_info_actual actual
	val rhoNode2 =
	  case DiGraphScc.findNodeOpt (get_rho_key p_actual) (get_graph()) of
	    SOME n2 => n2
	  | NONE =>
	      let
		val n2 = DiGraphScc.mkNode (get_rho_key p_actual, "unknown", "unknown")
	      in
		DiGraphScc.addNode n2 (get_graph());
		n2
	      end

	val oldAtKind = DiGraphScc.findEdgeOpt rhoNode1 rhoNode2
      in
	DiGraphScc.addEdgeWithUpdate rhoNode1 rhoNode2 (maxAtKind actual oldAtKind);
	add_edges ((rest_formals,str),rest_actuals)
      end
      | add_edges _ = die "add_edges, lists not of equal size."

    fun layout_graph () =
      let
	val g = get_graph()
	val sccGraph = DiGraphScc.genSccGraph g
	val nodeIdList = !region_paths

	fun findPath id1 id2 =
	  let
	    val node1 = DiGraphScc.findNodeOpt id1 g
	    val node2 = DiGraphScc.findNodeOpt id2 g
	  in
	    case (node1, node2) of
	      (NONE, _) =>
		(warn (line ("Can't generate path between " ^ Int.toString id1
			     ^ " and " ^ Int.toString id2 ^ ".")
		       // line "The first node does not exist.");
		[])
	    | (_, NONE) =>
		(warn (line ("Can't generate path between " ^ Int.toString id1
			     ^ " and " ^ Int.toString id2 ^ ".")
		       // line "The second node does not exist.");
		[])
	    | (SOME n1, SOME n2) => DiGraphScc.pathsBetweenTwoNodes n1 n2 sccGraph
	  end

	val pathsList =
	  map
	  (fn (id1, id2) => findPath id1 id2)
	  nodeIdList

      in
	PP.NODE{start="Begin layout of region flow graph and SCC-graph.",
		finish="End layout of region flow graph and SCC-graph.",
		indent=4,
		children=[DiGraphScc.layoutGraph (fn (id,str,size) => str^"[r"^(Int.toString id)^":"^size^"]")
			  (fn edgeInfo => " "^(show_atkind edgeInfo))
			  (fn id => "r"^(Int.toString id)) (DiGraphScc.rangeGraph g),
			  DiGraphScc.layoutScc (fn id => "r"^(Int.toString id)) sccGraph] @
		(map (DiGraphScc.layoutPaths (fn id => "r"^(Int.toString id))) pathsList),
		childsep=PP.NOSEP}
      end

  end
