
functor RegionFlowGraphProfiling(structure Effect    : EFFECT
				 structure AtInf     : AT_INF
				 structure PhySizeInf: PHYS_SIZE_INF
				 structure PP        : PRETTYPRINT
				 structure Flags     : FLAGS
				 structure Crash     : CRASH
				 structure Report    : REPORT
				 sharing type Report.Report = Flags.Report
				   ) : REGION_FLOW_GRAPH_PROFILING =
  struct

    type place = Effect.place
    type 'a at = 'a AtInf.at
    type phsize = PhySizeInf.phsize
    type pp = PhySizeInf.pp
    type StringTree = PP.StringTree

    fun die errmsg   = Crash.impossible ("RegionFlowGraphProfiling." ^ errmsg)

    fun member a [] = false
      | member a (x::xs) = a=x orelse member a xs

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

    fun show_phsize (PhySizeInf.INF) = "inf"
      | show_phsize (PhySizeInf.WORDS n) = Int.toString n

    fun show_atkind (AtInf.ATTOP _) = "attop" 
      | show_atkind (AtInf.ATBOT _) = "atbot"
      | show_atkind (AtInf.SAT _) = "sat"
      | show_atkind (AtInf.IGNORE) = die "show_atkind"

    fun get_info_actual (AtInf.ATTOP i) = i
      | get_info_actual (AtInf.ATBOT i) = i
      | get_info_actual (AtInf.SAT i) = i
      | get_info_actual _ = die "get_info_actual"

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
       | (AtInf.SAT _,   AtInf.ATBOT _) => ak1
       | _  => die ("maxAtKind("^(show_atkind ak1)^","^(show_atkind ak2)^")"))
      | maxAtKind ak1 NONE = ak1

    (*--------------------------------------------------------------------------------------*
     * Generation of Region Flow Graphs                                                     *
     *  When building the region flow graph, primitives like explode do not have any actual *
     *  region variables because all primitives are inlined to f.ex. prim(explode,...)      *
     *  by the lambda optimizer.                                                            *
     *--------------------------------------------------------------------------------------*)

    structure DiGraphScc = DiGraphScc(structure InfoDiGraph =
					struct
					  type nodeId = int
					  type info = int * string * string
					  type edgeInfo = (place*pp) AtInf.at (* We put the storage mode on the edge. *)
					  fun lt (a:nodeId) b = (a<b)
					  fun getId ((id,s,size):info) = id
					  val pu = Pickle.int
					end
				      structure PP = PP
				      structure Flags = Flags
				      structure Crash = Crash
				      structure Report = Report)



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
	val nodeIdList = !Flags.region_paths
	  
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

    fun export_graph stream = 
      let
	val g = get_graph()
	val sccGraph = DiGraphScc.genSccGraph g
	val nodeIdList = !Flags.region_paths
	  
	(* Returns a list with each element being a list of nodes (a path). *)
	(* [ [n1, ..., nN],...,[n1, ..., nM] ] *)
	fun findPath id1 id2 : DiGraphScc.node list list=
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
	    | (SOME n1, SOME n2) => 
		let
		  val sccNodess = DiGraphScc.pathsBetweenTwoNodes n1 n2 sccGraph
		in
		  List.map
  		  (foldr (fn (sccNode, acc) => (DiGraphScc.convertSccNodeToNodes sccNode) @ acc) [])
		  sccNodess
		end
	  end

	(* Returns a list of paths numbered from two each with a path name.               *)
	(* [ [pathNo1, pathName1, nodesInPath], ..., [pathNoN, pathNameN, nodesInPathN] ] *)
	val pathsList : (int * string * DiGraphScc.node list) list = 
	  let
	    fun paths no [] = []
	      | paths no ((id1, id2)::rest) = 
	      let
		val (no',p) = 
		  foldr
		   (fn (nodes, (no, acc)) => 
		     (no+1, (no, "Path" ^ Int.toString no ^ "(r" ^ Int.toString id1 ^ ",r" ^ Int.toString id2 ^ ")", nodes) :: acc))
		  (no, []) 
		  (findPath id1 id2)
	      in
		p @ (paths no' rest)
	      end
	  in
	    paths 2 nodeIdList
	  end
	
	(* Returns a list of classes numbered from two each with a class name.                  *)
	(* Each class contains a list of nodepairs representing an edge in the the path         *)
	(* that the class represents.                                                           *)
	(* [ [classNo1, className1, edgesInClass], ..., [classNoN, classNameN, edgesInClassN] ] *)
	val classList : (int * string * (DiGraphScc.node * DiGraphScc.node) list) list =
	  let
	    fun genEdgeList nodes =
	      foldr
	      (fn (node, acc) =>
 	        (foldr
		 (fn (node', acc) =>
		   (if member node' nodes then
		      (node, node') :: acc
		    else
		      acc))
		 acc
		 (DiGraphScc.succNodes node)))
	      []
	      nodes
	  in
	    List.map 
	    (fn (classNo, className, nodes) => (classNo, className, genEdgeList nodes))
	    pathsList
	  end

	fun exportClassNames (classNo, className, nodePairs) = 
	  TextIO.output(stream, "classname " ^ (Int.toString classNo) ^ ":\"" ^ className ^ "\"\n")

	val beginGraph = "graph: {\n"
	val attrGraph = "title: \"RegionFlowGraph and SCCgraph\"\n" ^
	  "layoutalgorithm: dfs\n" ^
	  "splines: yes\n" ^
	  "finetuning: no\n" ^
	  "orientation: left_to_right\n" ^
	  "ignore_singles: no\n" ^
	  "display_edge_labels: yes\n" ^
	  "classname 1:\"Graph\"\n"
	val endGraph = "}\n"
	  
      in
	(TextIO.output(stream, beginGraph ^ attrGraph);
	 map exportClassNames classList;
	 DiGraphScc.exportGraphVCG 
         "Region flow graph"
	 (fn (id,str,size) => str^"[r"^(Int.toString id)^":"^size^"]") 
	 (fn edgeInfo => " "^(show_atkind edgeInfo)) (*06/03/1996, Niels*)
	 (fn id => "r"^(Int.toString id)) 
	 classList
	 g stream;
	 DiGraphScc.exportSccVCG "SCC graph" (fn id => "r"^(Int.toString id)) sccGraph stream;
	 TextIO.output(stream, endGraph))
      end
  end (*struct*)
