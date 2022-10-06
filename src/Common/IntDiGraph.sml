
signature INFO_DIGRAPH =
sig
  (* To make a DiGaph we have to specify the info field of the nodes, and *
   * identify each node with an identifier on which there is a binary     *
   * ordering relation. The reason is that the graph is implemented using *
   * a finite map.                                                        *
   * Function getId takes the id field out of the info fiels.             *)

  type nodeId
  type info
  type edgeInfo
  val lt: nodeId * nodeId -> bool
  val getId: info -> nodeId

  val pu : nodeId Pickle.pu
end


signature DIGRAPH2 =
  sig

    (* DiGraph, is a directed graph with an id on each node, an info field, and *
     * some attributes for internal use.                                        *
     * The info field contain the id, and one have to supply a function to      *
     * extract the id from the info field.                                      *
     * The graph is constructed with a finite map from id's to the nodes,       *
     * containing the info field, attributes and the set of out nodes           *
     * represented as ref to nodes.                                             *)

    exception DiGraphExn of string

    type nodeId
    type info
    type edgeInfo
    type node
    type graph

    type StringTree

    (*---------------------------------------------------------*
     *                     Graph oprations                     *
     * Root nodes are those nodes with zero in-edges.          *
     * Leaf nodes are those nodes with zero out-edges, so a    *
     * node can be a root and leaf node at the same time.      *
     *---------------------------------------------------------*)

    (* Returns an empty graph. *)
    val mkGraph : unit -> graph

    (* Generates a node ready to put in a graph with addNode *)
    val mkNode   : info -> node

    (* Return the domain, that is all id's on the nodes in the graph. *)
    val domGraph : graph -> nodeId list

    (* Return range, that is all nodes in the graph. *)
    val rangeGraph : graph -> node list

    (* Insert a node in the graph. The node becomes a root *)
    (* until an in edge is created.                                *)
    (* If there already is a node with same id, then DiGraphExn    *)
    (* is raised.                                                  *)
    val addNode : node -> graph -> unit

    (* Same as addNode, except it is ok that the id of the node to *)
    (* be inserted already exists. In that case the info field is  *)
    (* copied to the excisting node.                               *)
    val addNodeWithUpdate : node -> graph -> unit

    (* Return the node given by nodeId and raise DiGraphExn if the *)
    (* node does not exists.                                       *)
    (* findNodeOpt does not raise DiGraphExn but return NONE.      *)
    val findNode    : nodeId -> graph -> node
    val findNodeOpt    : nodeId -> graph -> node option

    (* Given a node a ref to the info field is returned. *)
    val getInfoNode : node -> info ref

    (* Given a node and a new info field the node is updated with *)
    (* the new info field.                                        *)
    val setInfoNode : node -> info -> unit

    (* Given two nodes, the edgeInfo is returned if they exists. *)
    val findEdge : node -> node -> edgeInfo
    val findEdgeOpt : node -> node -> edgeInfo option

    (* Given two nodes n1 and n2 an edge n1--edgeInfo-->n2 is inserted. *)
    val addEdge : node -> node ->edgeInfo -> unit
    val addEdgeWithUpdate : node -> node -> edgeInfo -> unit

    (* Given a graph all root nodes are returned. *)
    val rootNodes : graph -> node list

    (* Given a graph all leaf nodes are returned. *)
    val leafNodes : graph -> node list

    (* Given a node a list with the successors are returned. *)
    val succNodes : node  -> node list

    (* Given a node a list with the predessors are returned. *)
    val predNodes : node -> graph -> node list

    (* Given a node all nodes reachable from the node is returned.   *)
    (* The node itself is also in the list so you can never get an   *)
    (* empty list.                                                   *)
    val reachable : node -> node list

    (* fold is done in nodeId order such that the accumulator has to be *)
    (* reversed to get the elements in normal order again.              *)
    val fold : (node * 'b -> 'b) -> 'b -> graph -> 'b

    val layoutNode : (info -> string) -> node -> StringTree
    val layoutGraph : (info -> string) -> (edgeInfo -> string) -> (nodeId -> string) -> node list -> StringTree
    val exportGraphVCG : string ->
                         (info -> string) ->
			 (edgeInfo -> string) ->
			 (nodeId -> string) ->
                         (int * string * (node * node) list) list ->
			 graph -> TextIO.outstream -> unit
  end

signature DIGRAPH_ALL =
  sig
    include DIGRAPH2

    (* Extra functions to perform attribute manipulations. *)

    val getNodeId : node -> nodeId

    (*-------------------------------------------------------------------*
     *                   Operations on attributes.                       *
     *-------------------------------------------------------------------*)

    type attributes

    (* Return a ref to the list of out nodes. *)
    val getOutSet : node -> (node * edgeInfo) list ref

    (* Given a list of out edges, a list with nodes is returned (without edge infos). *)
    val getNodes : (node * edgeInfo) list ref -> node list

    (* Returns a ref to the attributes. *)
    val getAttributes : node -> attributes ref

    (* Update node n with new attributes attr. *)
    val setAttributes : node -> attributes -> unit

    (* Ref to visited field. *)
    val getVisited : node -> bool ref

    (* Updates node n with new visited field. *)
    val setVisited : node -> bool -> unit

    (* Returns a ref to the number of in-edges. *)
    val getInEdges : node -> int ref

    (* Update number of in-edges for node n. *)
    val setInEdges : node -> int -> unit

    (* Return number of out-edges for node n. *)
    val getOutEdges : node -> int ref

    (* Update number of out-edges for node n. *)
    val setOutEdges : node -> int -> unit

    (* Return Dfs number for node n. *)
    val getDfsNo : node -> int ref

    (* Update Dfs number for node n. *)
    val setDfsNo : node -> int -> unit

    (* Return Dfs number for node n. *)
    val getSccNo : node -> int ref

    (* Update Dfs number for node n. *)
    val setSccNo : node -> int -> unit

  end

signature DIGRAPH_SCC =
  sig
    include DIGRAPH2

    type sccGraph
    type sccNode

    (* StronglyConnectedComponents is a list of strongly connected components *)
    (* which again is a list of nodes. Each node is in exactly one component. *)
    val scc : graph -> node list list
    val genSccGraph: graph -> sccGraph
    val pathsBetweenTwoNodes : node -> node -> sccGraph -> sccNode list list
    val convertSccNodeToNodes : sccNode -> node list
    val layoutPaths: (nodeId -> string) -> sccNode list list -> StringTree
    val layoutScc : (nodeId -> string) -> sccGraph -> StringTree
    val exportSccVCG : string -> (nodeId -> string) -> sccGraph -> TextIO.outstream -> unit
  end


functor DiGraphAll(InfoDiGraph : INFO_DIGRAPH) : DIGRAPH_ALL =
  struct
    structure PP = PrettyPrint

    structure IdFinMap  =
      OrderFinMap(struct
		      type t = InfoDiGraph.nodeId
		      val lt = InfoDiGraph.lt
		  end)


    (*-------------------------------------------------------------------------------------*
     * Graph representation, where each node contain some attributes only used internally  *
     * by the graph algorithms. The function freshAttributes is called each time a new     *
     * node is constructed, so when adding new attributes you only have to change the      *
     * attributes type, and the function freshAttributes.                                  *
     *-------------------------------------------------------------------------------------*)

    type attributes = {Visited: bool ref,
		       InEdges: int ref,
		       OutEdges: int ref,
		       DfsNo: int ref,
		       SccNo: int ref}

    type 'a idFinMap = 'a IdFinMap.map

    type nodeId = InfoDiGraph.nodeId
    type info = InfoDiGraph.info
    type edgeInfo = InfoDiGraph.edgeInfo

    datatype graphnode =
      GRAPHNODE of {info: info ref,
		    out: (graphnode ref * edgeInfo) list ref,
		    attributes: attributes ref}
    withtype node = graphnode ref
    and      graph = graphnode ref idFinMap ref

    exception DiGraphExn of string

    (*-------------------------------------------------------------------*
     *                   Auxiallery operations.                       *
     *-------------------------------------------------------------------*)

    fun pp(t):string      = PP.flatten(PP.format(!Flags.colwidth, t))
    fun log s             = TextIO.output(!Flags.log,s ^ "\n")
    fun die errmsg        = Crash.impossible ("IntDiGraph." ^ errmsg)

    (*-------------------------------------------------------------------*
     *                   Operations on attributes.                       *
     *-------------------------------------------------------------------*)

    fun freshAttributes () : attributes =
      {Visited = ref false, (* Have the node beed visited. *)
       InEdges = ref 0,     (* Number of in-edges to the node. *)
       OutEdges = ref 0,    (* Number of out-edges to the node. *)
       DfsNo = ref 0,       (* DepthFirstSearch numer. *)
       SccNo = ref 0}       (* Id on scc holding this node. *)


    (* Return a ref to the list of out nodes. *)
    fun getOutSet (n: node) =
      case (!n) of
	GRAPHNODE{out, ...} => out

    (* Given a list of out edges, a list with nodes is returned (without edge infos). *)
    fun getNodes (out: (graphnode ref * edgeInfo) list ref) =
      List.map (fn (n, i) => n) (!out)

    (* Returns a ref to the attributes. *)
    fun getAttributes (n: node) =
      case (!n) of
	GRAPHNODE {attributes, ...} => attributes

    (* Update node n with new attributes attr. *)
    fun setAttributes (n: node) (attr: attributes) =
      getAttributes n := attr

    (* Ref to visited field. *)
    fun getVisited (n : node) =
      case !(getAttributes n) of
	{Visited, ...} => Visited

    (* Updates node n with new visited field. *)
    fun setVisited (n : node) (visited : bool) =
      case !(getAttributes n) of
	{Visited, ...} => Visited := visited

    (* Returns a ref to the number of in-edges. *)
    fun getInEdges (n : node) =
      case !(getAttributes n) of
	{InEdges, ...} => InEdges

    (* Update number of in-edges for node n. *)
    fun setInEdges (n : node) (i : int) =
      case !(getAttributes n) of
	{InEdges, ...} => InEdges := i

    (* Return number of out-edges for node n. *)
    fun getOutEdges (n : node) =
      case !(getAttributes n) of
	{OutEdges, ...} => OutEdges

    (* Update number of out-edges for node n. *)
    fun setOutEdges (n : node) (u : int) =
      case !(getAttributes n) of
	{OutEdges, ...} => OutEdges := u

    (* Return Dfs number for node n. *)
    fun getDfsNo (n : node) =
      case !(getAttributes n) of
	{DfsNo, ...} => DfsNo

    (* Update Dfs number for node n. *)
    fun setDfsNo (n : node) (u : int) =
      case !(getAttributes n) of
	{DfsNo, ...} => DfsNo := u

    (* Return Scc number for node n. *)
    fun getSccNo (n : node) =
      case !(getAttributes n) of
	{SccNo, ...} => SccNo

    (* Update Scc number for node n. *)
    fun setSccNo (n : node) (u : int) =
      case !(getAttributes n) of
	{SccNo, ...} => SccNo := u

    (*-------------------------------------------------------------------*
     *                   Graph and node operations                       *
     *-------------------------------------------------------------------*)

    (* Return an empty graph. *)
    fun mkGraph () = ref IdFinMap.empty

    (* Make new node ready to be inserted in the graph. *)
    fun mkNode (i : info) : node =
      ref (GRAPHNODE {info = ref i,
		      out =  ref [],
		      attributes = ref (freshAttributes())})

    (* Return a ref to info of node n. *)
    fun getInfoNode (n : node) =
      case (!n) of
	GRAPHNODE {info, ...} => info

    (* Update info field of node n. *)
    fun setInfoNode (n : node) (i : info) =
      case (!n) of
	GRAPHNODE {info, ...} => info := i

    fun getNodeId (n : node) = InfoDiGraph.getId (!(getInfoNode n))

    fun findNode (id : nodeId) (g : graph) : node =
      case IdFinMap.lookup (!g) id of
	SOME n => n
      | NONE => raise DiGraphExn "DiGraph error in findNode: Node doesn't exist."

    fun findNodeOpt (id : nodeId) (g : graph) : node option =
      IdFinMap.lookup (!g) id

    (* Add node n to graph g; that is just insert n in map, but *)
    (* check first, that n doesn't exists.                      *)
    fun addNode (n : node) (g : graph) =
      case IdFinMap.lookup (!g) (getNodeId n) of
	NONE => g := IdFinMap.add (getNodeId n, n, (!g))
      | SOME _ => raise DiGraphExn "DiGraph error in addNode: Node allready exist."

    (* If node exists info field is copied to existing ndoe.     *)
    (* Edges are not copied, so be carefull with this functions. *)
    fun addNodeWithUpdate (n : node) (g : graph) =
      addNode n g
      handle DiGraphExn _ =>
	setInfoNode (findNode (getNodeId n) g) (!(getInfoNode n))

    (* Given two nodes, the edgeInfo is returned if they exists. *)
    fun findEdge (n1 : node) (n2 : node) : edgeInfo =
      case (!n1) of
	  GRAPHNODE {out, ...} =>
          case List.find (fn (n,i) => n = n2) (!out) of
              SOME (_,res) => res
            | NONE => raise DiGraphExn "Digraph error in findEdge: Edge does not exist."

    fun findEdgeOpt (n1 : node) (n2 : node) : edgeInfo option =
      case (!n1) of
	  GRAPHNODE {out, ...} =>
          case List.find (fn (n,i) => n = n2) (!out) of
              SOME (_,res) => SOME res
            | NONE => NONE

    fun addEdge (n1 : node) (n2 : node) (info : edgeInfo) =
      case (!n1) of
	GRAPHNODE {out, ...} =>
	  if (ListUtils.member n2 (getNodes(out))) = false then
	      (out := (n2,info) :: (!out);
	       setOutEdges n1 ((!(getOutEdges n1))+1);
	       setInEdges n2 ((!(getInEdges n2))+1))
	    else
	      ()

    fun addEdgeWithUpdate (n1 : node) (n2 : node) (info : edgeInfo) =
      case (!n1) of
	GRAPHNODE {out, ...} =>
	  if (ListUtils.member n2 (getNodes(out))) = false then
	      (out := (n2,info) :: (!out);
	       setOutEdges n1 ((!(getOutEdges n1))+1);
	       setInEdges n2 ((!(getInEdges n2))+1))
	    else
	      (out := (List.map (fn (n, i) =>
				 if n = n2 then
				   (n, info)
				 else
				   (n, i)) (!out)))

    fun rootNodes (g : graph) =
      let
	fun root (n1 : node, acc : node list) =
	  if (!(getInEdges n1)) = 0 then
	    n1 :: acc
	  else
	    acc
      in
	IdFinMap.fold root [] (!g)
      end

    fun leafNodes (g : graph) =
      let
	fun leaf (n1 : node, acc : node list) =
	  if (!(getOutEdges n1)) = 0 then
	    n1 :: acc
	  else
	    acc
      in
	IdFinMap.fold leaf [] (!g)
      end

    fun succNodes (n : node) =
      case (!n) of
	GRAPHNODE{out, ...} => getNodes(out)


    fun predNodes (n : node) (g : graph) =
      let
	fun pred (n1 : node, acc : node list) =
	  case (!n1) of
	    GRAPHNODE {out, ...} =>
	      if ListUtils.member n (getNodes out) then
		n1 :: acc
	      else
		acc
      in
	IdFinMap.fold pred [] (!g)
      end

    fun fold (f: node * 'b -> 'b) (acc : 'b) (g : graph) : 'b =
      IdFinMap.fold f acc (!g)

    fun reachable (n : node) =
      let
	fun reachable' (n as ref (GRAPHNODE {out, ...}), acc) =
	  if !(getVisited n) = false then
	    (setVisited n true;
	     List.foldr (fn (node,acc) =>
			  reachable' (node, acc)) (n::acc) (getNodes out))
	  else
	    acc

	fun SetVisitedFalse (node as ref (GRAPHNODE{out, ...}) : node) =
	  if !(getVisited node) = true then
	    (setVisited node false;
	     List.app SetVisitedFalse (getNodes out))
	  else
	    ()
      in
	reachable' (n, []) before (SetVisitedFalse n)
      end

    fun domGraph (g : graph) : nodeId list =
      IdFinMap.dom (!g)

    fun rangeGraph (g : graph) : node list =
      IdFinMap.range (!g)

    (*-------------------------------------------------------------------*
     *                  Pretty Printing                                  *
     *-------------------------------------------------------------------*)

    type StringTree = PP.StringTree

    fun layoutNode (layoutInfo : info -> string) (n : node) : StringTree =
	PP.NODE {start=layoutInfo (!(getInfoNode n)),
		 finish="",
		 indent=0,
		 children=[],
		 childsep=PP.NOSEP}

    fun layoutGraph (layoutInfo : info -> string)
                    (layoutEdgeInfo : edgeInfo -> string)
                    (layoutId : nodeId -> string)
		    (rootNodes : node list) : StringTree =
      let

	val sizeIndent = 0

	fun makeChildren level node =
	  List.map (fn (n',edgeInfo') =>
		    if !(getVisited n') = false then
		      (setVisited n' true;
		       let
			 val startStr = "   --" ^ (layoutId (getNodeId node)) ^ (layoutEdgeInfo(edgeInfo')) ^ "-->   " ^ (layoutInfo (!(getInfoNode n')))
		       in
			 PP.NODE{start = startStr,
				 finish = ";",
				 indent = (*level+*)(String.size startStr),
				 children = makeChildren (level+(String.size startStr)) n',
				 childsep = PP.NOSEP}
		       end)
		    else
		      let
			val startStr = "   --" ^ (layoutId (getNodeId node)) ^ (layoutEdgeInfo(edgeInfo')) ^ "-->   [*" ^ (layoutId (getNodeId n')) ^ "*]"
		      in
			PP.NODE{start = startStr,
			      finish = ";",
			      indent = (*level+*)(String.size startStr),
			      children = [PP.LEAF " "],
			      childsep = PP.NOSEP}
		      end)
	  (!(getOutSet node))

	fun layout (node, acc) =
	  if !(getVisited node) = false then
	    (setVisited node true;
	     (PP.NODE{start = layoutInfo (!(getInfoNode node)),
		      finish = "",
		      indent = sizeIndent,
		      children = makeChildren 0 node,
		      childsep = PP.NOSEP})::acc)
	  else
	    acc

      in
	PP.NODE{start="[Starting layout of graph...",
		finish = "...Finishing layout of graph]",
		indent = 0,
		children = List.rev (List.foldl layout [] rootNodes),
		childsep = PP.NOSEP}
      end

    fun exportGraphVCG title
                       (layoutInfo : info -> string)
		       (layoutEdgeInfo : edgeInfo -> string)
		       (layoutId : nodeId -> string)
		       (classes : (int * string * (node * node) list) list)
                       (g : graph)
		       (out : TextIO.outstream) =
      let
	val newLine = "\n"

	fun exportNode node =
	  let
	    val beginNode = "node: {"
	    val titleNode = "title: \"" ^ (layoutId (getNodeId node)) ^ "\" "
	    val labelNode = "label: \"" ^ (layoutInfo (!(getInfoNode node))) ^ "\" "
	    val endNode = "}" ^ newLine
	  in
	    TextIO.output(out, beginNode ^ titleNode ^ labelNode ^ endNode)
	  end

	fun exportEdge node =
	  let
	    val beginEdge = "edge: {"
	    val sourcename = "sourcename: \"" ^ (layoutId (getNodeId node)) ^ "\" "
	    val targetname = "targetname: \""
            val label = "label: \" "
            val class = "class: "
	    val endEdge = "}" ^ newLine

	    fun addEdge (node',edgeInfo') =
	      TextIO.output(out, beginEdge ^ sourcename ^ (targetname ^ (layoutId (getNodeId node')) ^ "\" ") ^
		     class ^ "1 " ^ label ^ layoutEdgeInfo(edgeInfo') ^ "\"" ^ endEdge)
	  in
            map addEdge (!(getOutSet node))
	  end

       fun exportClass (classNo, className, nodePairs) =
         let
  	   val beginEdge = "edge: {"
	   val sourcename = "sourcename: \""
           val targetname = "targetname: \""
           fun label (node1, node2) =
	     let
	       val edgeInfo = findEdge node1 node2
	     in
		"label: \"" ^ className ^ "(" ^ layoutEdgeInfo(edgeInfo) ^ ")\""
	     end
           val class = "class: " ^ (Int.toString classNo) ^ " "
	   val endEdge = "}" ^ newLine
	 in
	   List.app
	    (fn (node1, node2) =>
	     TextIO.output(out, beginEdge ^ (sourcename ^ (layoutId (getNodeId node1)) ^ "\" ") ^
		      (targetname ^ (layoutId (getNodeId node2)) ^ "\" ") ^
		      class ^ (label(node1, node2)) ^ endEdge))
	    nodePairs
	 end

       fun exportClassNames (classNo, className, nodePairs) =
	 TextIO.output(out, "classname " ^ (Int.toString classNo) ^ ":\"" ^ className ^ "\"" ^ newLine)

	val beginGraph = "graph: {" ^ newLine
	val attrGraph = "title: \"" ^ title ^ "\"" ^ newLine ^
			"splines: yes" ^ newLine ^
			"finetuning: no" ^ newLine ^
			"folding: 1" ^ newLine ^
                        "orientation: left_to_right" ^ newLine ^
			"ignore_singles: yes" ^ newLine
	val endGraph = "}" ^ newLine

	val range_g = rangeGraph g
      in
	(TextIO.output(out, beginGraph);
	 TextIO.output(out, attrGraph);
	 map exportClassNames classes;
	 map exportNode range_g;
	 map exportEdge range_g;
	 map exportClass classes;
	 TextIO.output(out, endGraph))
      end
  end

functor DiGraph2(InfoDiGraph : INFO_DIGRAPH) : DIGRAPH2 =
  struct
    structure DiGraph = DiGraphAll(InfoDiGraph)
    open DiGraph
  end


functor DiGraphScc(InfoDiGraph : INFO_DIGRAPH) : DIGRAPH_SCC =
  struct
    structure PP = PrettyPrint

    structure DiGraph = DiGraphAll(InfoDiGraph)
    open DiGraph

    datatype SCCedgeInfo = NO_EDGE_INFO

    (* We make a new graph holding the strongly connected components. *)
    (* Used when pretty printing the scc-graph.                       *)
    structure SccDiGraph = DiGraphAll(struct
					  type nodeId = int
					  type info = int * node list
					  type edgeInfo = SCCedgeInfo
					  fun lt (a:nodeId, b) = (a<b)
					  fun getId ((i,ns):info) = i
					  val pu = Pickle.int
				      end)

    type sccGraph = SccDiGraph.graph
    type sccNode = SccDiGraph.node

    (* StronglyConnectedComponent return a list of the strongly connected components, *
     * which is a list of nodes. Each node is in exactly one component.               *)
    fun scc (g : graph) : node list list =
      let
	val scc = ref []
	fun resetSCC() = scc := []
	fun newSCC() = scc := []::(!scc)

	val nodeStack =  ref []
	fun resetNodeStack () = nodeStack := []
	fun pushNode n =
	  nodeStack := n :: (!nodeStack)
	fun popNode() =
	  case (!nodeStack) of
	    n::rest => (nodeStack := rest;n)
	  | [] => (raise DiGraphExn "DiGraph error in scc -- popNode.\n")

	val dfsNo = ref 0
	fun resetDfsNo () = dfsNo := 0
	fun nextDfsNo () =
	  (dfsNo := !dfsNo + 1;
	   !dfsNo)

	fun makeSCC n =
	  let
	    val n' = popNode()
	  in
	    (scc := (n'::hd(!scc))::(tl(!scc));
	     setDfsNo n' 0; (* This node is now in a connected component. *)
	     if n <> n' then
	       makeSCC n (* There are still nodes on the stack beloning to this component;  *)
	     else        (* the root node is always under all other nodes in the component. *)
	       ())
	  end

	fun processNode n =
	  let
	    val low = nextDfsNo()
	    val _ = setDfsNo n low
	    val _ = setVisited n true;
	    val _ = pushNode n;
	    val low = (List.foldl
		       (fn (n',l) =>
				  if !(getVisited n') then       (* n' processed before. *)
				    (if !(getDfsNo n') <> 0 then (* is n' in this scc.   *)
				       Int.min (!(getDfsNo n'), l)
				     else
				       l)
				  else
				    Int.min (processNode n', l)) low (getNodes((getOutSet n))))
	  in
	    (if !(getDfsNo n) = low then (* n is root in a scc *)
	       (newSCC();
		makeSCC n)
	     else
	       ());
            low
	  end

      in
	(resetDfsNo();
	 resetNodeStack();
	 resetSCC();
	 List.app (fn n => (setVisited n false;
			      setDfsNo n 0)) (rangeGraph g);
	 List.app (fn n =>
		     if !(getVisited n) = false then
		       (processNode n;())
		     else
		       ()) (rangeGraph g);

	 (List.rev (!scc)) before
		     (List.app (fn n => (setVisited n false;
					   setDfsNo n 0)) (rangeGraph g)))
      end

    (* Generate a graph with strongly connected components. *)
    (* We may not make any cycles. *)
    fun genSccGraph g =
      let
	val sccs = scc g
	val sccGraph = SccDiGraph.mkGraph()

	fun mkEdge node1 node2 =
	  let
	    val sccNode1 = SccDiGraph.findNode (!(getSccNo node1)) sccGraph
	    val sccNode2 = SccDiGraph.findNode (!(getSccNo node2)) sccGraph
	  in
	    if sccNode1 <> sccNode2 then
	      SccDiGraph.addEdge sccNode1 sccNode2 NO_EDGE_INFO
	    else
	      ()
	  end
      in
	(List.foldl (fn (nodes,sccNo) =>
				  (SccDiGraph.addNode (SccDiGraph.mkNode (sccNo, nodes)) sccGraph;
				   List.app (fn node => setSccNo node sccNo) nodes;
				   sccNo+1)
				  ) 1 sccs;
	 List.app (fn nodes =>
		     List.app (fn node1 =>
				 List.app (fn node2 => mkEdge node1 node2) (getNodes((getOutSet node1)))) nodes) sccs);
	sccGraph
      end

    fun layoutSccNo sccNo = "sccNo " ^ (Int.toString sccNo)
    fun layoutComponent layoutId (sccNo, scc) =
      "[" ^ (layoutSccNo sccNo) ^ ": " ^
      (List.foldr (fn (node,str) => (layoutId (getNodeId node)) ^ "," ^ str)
                  "]" scc)

    fun layoutEdge NO_EDGE_INFO = ""

    fun layoutScc layoutId sccG =
	SccDiGraph.layoutGraph (layoutComponent layoutId) layoutEdge layoutSccNo (SccDiGraph.rootNodes sccG)

    fun exportSccVCG title layoutId sccG stream =
      SccDiGraph.exportGraphVCG title (layoutComponent layoutId) layoutEdge layoutSccNo [] sccG stream

    fun pathsBetweenTwoNodes node1 node2 sccG =
      let
	val sccNode1 = SccDiGraph.findNode (!(getSccNo node1)) sccG
	val sccNode2 = SccDiGraph.findNode (!(getSccNo node2)) sccG

	fun findPath curSccNode sccNode2 path paths =
	  if curSccNode = sccNode2 then
	    (List.rev (curSccNode::path))::paths
	  else
	    List.foldl (fn (sccNode,paths) =>
			 findPath sccNode sccNode2 (curSccNode::path) paths)
	    paths (SccDiGraph.getNodes(SccDiGraph.getOutSet curSccNode))
      in
	findPath sccNode1 sccNode2 [] []
      end

    fun convertSccNodeToNodes sccNode =
      case (!(SccDiGraph.getInfoNode sccNode)) of
	(sccNo, nodes) => nodes

    fun layoutPaths layoutId paths =
      let
	fun layoutPath path =
	  PP.NODE{start="[Start path: ",
		  finish="]",
		  indent = 4,
		  children = List.map (fn sccNode =>
				       PP.LEAF (layoutComponent layoutId (!(SccDiGraph.getInfoNode sccNode)))) path,
		  childsep = PP.RIGHT "--->"}
      in
      PP.NODE{start="[Starting layout of paths...",
	      finish="...Finishing layout of paths]",
	      indent = 4,
	      children = List.map layoutPath paths,
	      childsep = PP.RIGHT ","}
      end
  end

(*
functor Test(structure Flags      : FLAGS
	     structure PP         : PRETTYPRINT
	     structure Crash      : CRASH
	     structure Report     : REPORT)  =
  struct

    structure IntDiGraph = DiGraphScc(structure InfoDiGraph =
					struct
					  type nodeId = int
					  type info = int * string
					  type edgeInfo = string
					  fun lt (a:nodeId, b) = (a<b)
					  fun getId ((i,s):info) = i
					end
				      structure PP = PP
				      structure Flags = Flags
				      structure Crash = Crash
				      structure Report = Report)

    open IntDiGraph

    fun pp(t):string      = PP.flatten(PP.format(!Flags.colwidth, t))
    fun log s             = TextIO.output(!Flags.log,s ^ "\n")


    fun getInfo i = !(getInfoNode i)

    val layoutInfo = PP.LEAF
    fun layoutInfoString i =
      case getInfo i of
	(i,s) => (Int.toString i) ^ ": " ^ s

    fun do_test1() =
      let
	val g = mkGraph()
	val _ = addNode (mkNode ((1, "Niels"):info)) g
	val _ = addNode (mkNode (2, "Hans")) g
	val _ = addNodeWithUpdate (mkNode (2, "Hans Hansen")) g
	val n1 = findNode 1 g
	val n2 = findNode 2 g
      in
	log (layoutInfoString(n1));
	log (layoutInfoString(n2));
	setInfoNode n2 (2, "Hans Update Hansen"); (*fejl mulighed, aendre id. *)
	log (layoutInfoString(findNode 2 g));

	case findNodeOpt 42 g of
	  NONE => log "findNodeOpt works"
	| SOME v => log ("findNodeOpt doesn't work with info " ^ (layoutInfoString v));

	case findNodeOpt 2 g of
	  NONE => log "findNodeOpt doesn't work"
	| SOME v => log ("findNodeOpt works, with info " ^ (layoutInfoString v));

	addNode (mkNode (1, "niels")) g
	handle DiGraphExn _  => log "addNode works by first looking for id.";

	()
      end

    fun do_test2() =
      let
	val g = mkGraph()
	val _ = List.app (fn x => addNode (mkNode x) g)
	  [(1, "node1"),
	   (2, "node2"),
	   (3, "node3"),
	   (4, "node4"),
	   (5, "node5"),
	   (6, "node6"),
	   (7, "node7"),
	   (8, "node8"),
	   (9, "node9")]

	val _ = List.app (fn (id1, id2, edgeInfo) => addEdge (findNode id1 g) (findNode id2 g) edgeInfo)
	  [(1,5,"edge1"),
	   (1,4,"edge2"),
	   (1,2,"edge3"),
	   (2,4,"edge4"),
	   (2,3,"edge5"),
	   (3,1,"edge6"),
	   (4,3,"edge7"),
	   (5,4,"edge8"),
	   (6,8,"edge9"),
	   (6,7,"edge10"),
	   (7,5,"edge11"),
	   (8,7,"edge12"),
	   (8,6,"edge13"),
	   (8,4,"edge14")]

      in

	log "Writing roots [9] in graph g:";
	List.map (fn i => log ("Root node: " ^ (layoutInfoString i))) (rootNodes g);
	log "Writing leafs [9] in graph g:";
	List.map (fn i => log ("Leaf node: " ^ (layoutInfoString i))) (leafNodes g);
	log "Writing all successor nodes to node 1 [4, 5, 2]";
	List.map (fn i => log ("Succ node: " ^ (layoutInfoString i))) (succNodes (findNode 1 g));
	log "Writing all predessor nodes to node 1 [3]";
	List.map (fn i => log ("Pred node: " ^ (layoutInfoString i))) (predNodes (findNode 1 g) g);
	log "Writing all predessor nodes to node 6 [8]";
	List.map (fn i => log ("Pred node: " ^ (layoutInfoString i))) (predNodes (findNode 6 g) g);
	log "Writing all predessor nodes to node 4 [8, 5, 2, 1]";
	List.map (fn i => log ("Pred node: " ^ (layoutInfoString i))) (predNodes (findNode 4 g) g);

	log "Trying fold, where all info fields should be written now.";
	List.map (fn node => log ("Info: " ^ (layoutInfoString node)))
          (fold (fn (node, acc) => (node :: acc)) [] g);

	log "Trying reachable, where all nodes on reachable (1) should be written now [4, 5, 2, 3, 1].";
	List.map (fn (n) => log ("Reachable of node " ^ (layoutInfoString n)))
	  (reachable (findNode 1 g));

	log "Trying reachable, where all nodes on reachable (6) should be written now [1, ..., 8].";
	List.map (fn (id) => log ("Reachable of node " ^ (layoutInfoString id)))
	  (reachable (findNode 6 g));

	log "Trying reachable, where all nodes on reachable (9) should be written now [9].";
	List.map (fn (id) => log ("Reachable of node " ^ (layoutInfoString id)))
	  (reachable (findNode 9 g));

	log "Trying layoutGraph.";
	log (pp(layoutGraph (fn (i,s) => "[r"^(Int.toString i)^"="^s^"]") (fn i => i) (fn i => "r"^(Int.toString i)) (rangeGraph g) ));

	log "Trying scc.";
	log (pp(layoutScc (fn i => "r"^(Int.toString i)) (genSccGraph g)));

	log "Trying pathsBetweenTwoNodes: 6 and 1.";
	log (pp(layoutPaths (fn i => "r"^(Int.toString i)) (pathsBetweenTwoNodes (findNode 6 g) (findNode 1 g) (genSccGraph g))));

	log "Trying pathsBetweenTwoNodes: 8 and 5.";
	log (pp(layoutPaths (fn i => "r"^(Int.toString i)) (pathsBetweenTwoNodes (findNode 8 g) (findNode 5 g) (genSccGraph g))));

	log "Trying pathsBetweenTwoNodes: 9 and 1.";
	log (pp(layoutPaths (fn i => "r"^(Int.toString i)) (pathsBetweenTwoNodes (findNode 9 g) (findNode 1 g) (genSccGraph g))));

	log "Trying pathsBetweenTwoNodes: 9 and 9.";
	log (pp(layoutPaths (fn i => "r"^(Int.toString i)) (pathsBetweenTwoNodes (findNode 9 g) (findNode 9 g) (genSccGraph g))));


	log "Finished test2"
      end

  end

(*$DoTest:
      BasicIO Crash Flags Report PrettyPrint Test
*)
structure BasicIO = BasicIO();
structure Crash = Crash(structure BasicIO = BasicIO);
structure Flags = Flags(structure Crash = Crash);
structure Report = Report(structure BasicIO = BasicIO);
structure PP = PrettyPrint(structure Report = Report
			   structure Crash = Crash
			   structure Flags = Flags);
structure Test = Test(structure Flags = Flags
		      structure PP = PP
		      structure Crash = Crash
		      structure Report = Report)


*)
