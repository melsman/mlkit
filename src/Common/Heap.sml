(* File "Hepa.sml"                                        *)
(* These algorithms are based on the paper:               *)
(*    Functional Data Structures                          *)
(*          Chris Okasaki                                 *)
(* School of Computer Science, Carnegie Mellon University *)

(* 16/09/1996, Niels Hallenberg.                          *)

signature HEAP_INFO =
  sig
    type elem                              (* Type of ordered elements. *)
    val leq : elem * elem -> bool          (* Total ordering relation.  *)
    val layout : elem -> string            (* Layout element.           *)
  end

signature HEAP =
  sig
    structure HeapInfo : HEAP_INFO

    exception Heap of string
    type heap

    val empty : heap
    val unit : HeapInfo.elem -> heap
    val is_empty : heap -> bool
    val merge : heap * heap -> heap
    val find_min : heap -> HeapInfo.elem
    val delete_min : heap -> HeapInfo.elem * heap
    val insert : HeapInfo.elem -> heap -> heap
    val from_list : HeapInfo.elem list -> heap
    val to_list : heap -> HeapInfo.elem list
    val to_list_sorted : heap -> HeapInfo.elem list
    val sort : HeapInfo.elem list -> HeapInfo.elem list
    val layout : heap -> string
    val export_heap_vcg : string -> heap -> TextIO.outstream -> unit
    val fix_heap : heap -> heap
    val find_max : heap -> HeapInfo.elem
    val delete_max : heap -> heap * HeapInfo.elem
  end

functor Heap(structure HeapInfo: HEAP_INFO) : HEAP =
  struct

    structure HeapInfo = HeapInfo
    exception Heap of string

    datatype heap = Empty
                  | Node of int * HeapInfo.elem * heap * heap

    val empty = Empty
    fun unit x = Node(1,x,Empty,Empty)

    fun div2 n = n div 2

    fun is_empty Empty = true
      | is_empty _ = false

    fun node (x,a,Empty) = Node(1,x,a,Empty)
      | node (x,Empty,b) = Node(1,x,b,Empty)
      | node (x,a as Node(ra,_,_,_), b as Node(rb,_,_,_)) =
          if ra <= rb then
	    Node(ra+1,x,b,a)
	  else
	    Node(rb+1,x,a,b)

    fun merge (a,Empty) = a
      | merge (Empty,b) = b
      | merge (a as Node(_,x,a1,a2), b as Node(_,y,b1,b2)) =
          if HeapInfo.leq(x,y) then
	    node(x,a1,merge(a2,b))
	  else
	    node(y,b1,merge(b2,a))

    fun find_min Empty = raise Heap "Empty"
      | find_min (Node(r,x,a,b)) = x

    fun delete_min Empty = raise Heap "Empty"
      | delete_min (Node(r,x,a,b)) = (x, merge (a,b))

    fun insert x Empty = Node(1,x,Empty,Empty)
      | insert x (b as Node(_,y,b1,b2)) =
          if HeapInfo.leq (x,y) then
	    Node(1,x,b,Empty)
	  else
	    node(y,b1,merge(b2,Node(1,x,Empty,Empty)))

    local
      fun from_list' (0, xs) = (Empty, xs)
	| from_list' (1, x::xs) = (Node(1,x,Empty,Empty),xs)
	| from_list' (n, xs) = 
	    let
	      val (h1,xs1) = from_list'(div2 n, xs)
	      val (h2,xs2) = from_list'(div2 (n+1), xs1)
	    in
	      (merge(h1,h2), xs2)
	    end
    in
      fun from_list xs = #1 (from_list' (length xs, xs))
    end

    local
      fun to_list' Empty xs = xs
	| to_list' (Node(_,v,l,r)) xs =
	    let
	      val xs' = to_list' l xs
	      val xs'' = v :: xs'
	      val xs''' = to_list' r xs''
	    in
	      xs'''
	    end
    in
      fun to_list h = to_list' h []
    end

    fun to_list_sorted Empty = []
      | to_list_sorted n = 
          let
	    val (v,n') = delete_min n
	  in
	    v :: (to_list_sorted n')
	  end

    fun sort xs = to_list_sorted (from_list xs)

    fun layout Empty = "E"
      | layout (Node(h,v,l,r)) = "N("^(Int.toString h)^","^(HeapInfo.layout v)^"," ^
                                 (layout l) ^ "," ^ (layout r) ^ ")"

    fun export_heap_vcg (title: string)
                        (heap : heap)
			(out  : TextIO.outstream) =
      let
	val node_counter = ref 0
	fun fresh_node () = (node_counter := !node_counter + 1;
			     !node_counter)
        val new_line = "\n"
              
        fun export_node v =
          let
	    val node_id = fresh_node()
            val begin_node = "node: {"
            val title_node = "title: \"" ^ (Int.toString node_id) ^ "\" "
            val label_node = 
	      (case v of
		 NONE => ("label: \" Empty \" ")
	       | SOME (h,v) => ("label: \"" ^ "W:" ^ (Int.toString h) ^ ",Val:" ^ (HeapInfo.layout v) ^ "\" "))
            val end_node = "}" ^ new_line
          in
            (TextIO.output(out, begin_node ^ title_node ^ label_node ^ end_node);
	     node_id)
          end

        fun export_edge n_id1 n_id2 edge_info =
          let
            val begin_edge = "edge: {"
            val sourcename = "sourcename: \"" ^ (Int.toString n_id1) ^ "\" "
            val targetname = "targetname: \"" ^ (Int.toString n_id2) ^ "\" "
            val label = "label: \"" ^ edge_info ^ "\" "
	    val class = "class: 1"
            val end_edge = "}" ^ new_line
	  in
	    TextIO.output(out, begin_edge ^ sourcename ^ targetname ^ label ^ class ^ end_edge)
          end

	fun export_heap' Empty = export_node NONE
	  | export_heap' (Node(h,v,l,r)) =
	     let
	       val node_v = export_node (SOME (h,v))
	       val node_l = export_heap' l
	       val node_r = export_heap' r
	     in
	       (export_edge node_v node_l "L";
		export_edge node_v node_r "R";
		node_v)
	     end
	     
	val begin_graph = "graph: {" ^ new_line
        val attr_graph = "title: \"" ^ title ^ "\"" ^ new_line ^
	                 "splines: yes" ^ new_line ^
			 "finetuning: no" ^ new_line ^
			 "orientation: left_to_right" ^ new_line ^
			 "ignore_singles: yes" ^ new_line
        val end_graph = "}" ^ new_line
      in
        (TextIO.output(out, begin_graph);
         TextIO.output(out, attr_graph);
	 export_heap' heap;
         TextIO.output(out, end_graph))
      end

    fun fix_heap h = from_list (to_list h)

    local
      fun find_max' (Empty, max_val) = max_val
	| find_max' (Node(_,x,l,r), max_val) =
	    let
	      val max_val' = find_max' (l, max_val)
	      val max_val'' = find_max' (r, max_val')
	    in
	      if HeapInfo.leq (x, max_val'') then
		max_val''
	      else
		x
	    end
    in
      fun find_max h = find_max' (h, (find_min h))
    end

    fun delete_max h =
      let
	val max_val = find_max h
	fun delete_max' Empty = (Empty, NONE, 0)
	  | delete_max' (n as Node(1,x,Empty,Empty)) =
	  (if HeapInfo.leq (x,max_val) andalso
	     HeapInfo.leq (max_val,x) then
	     (Empty, SOME x, 0)
	   else
	     (n,NONE,1))
	  | delete_max' (n as Node(w,x,l,r)) =
	     case delete_max' l
	       of (n', NONE, w') => 
		 (case delete_max' r
		    of (n'',NONE,w'') => (n,NONE,w)
		     | (n'',SOME v'',w'') => (Node(w''+1,x,n',n''),SOME v'', w''+1))
		| (n',SOME v',w') => 
		    (if w > w'+1 then
		       (Node(w'+1,x,r,n'),SOME v', w'+1)
		     else
		       (Node(w,x,n',r),SOME v',w))
      in	
	case (delete_max' h)
	  of (h', NONE, _) => raise Heap "Empty"
	   | (h', SOME v, _) => (h',v)
      end
  end

(* TEST... *)

functor TestHeap ()  =
struct

  structure HeapInfo =
    struct
      datatype hval = C of {count: int ref,
			    id : int,
			    vars : heap_val list ref}
      withtype heap_val = hval ref
      type elem = heap_val
      fun leq (ref (C {count = ref c1, ...}), ref (C{count = ref c2, ...})) = c1 <= c2
      fun layout (ref (C {count = ref c, id, vars = ref xs})) =
	"Id(" ^ (Int.toString id) ^ ") with count: " ^ (Int.toString c) ^ 
        " and vars: [" ^ (foldl (fn (ref (C {count = ref c1, id, ...}),acc) => acc ^ "(" ^ (Int.toString c1) ^ ":" ^ (Int.toString id) ^ ")" ) "" xs) ^ "]."
    end

  structure Heap : HEAP = Heap(structure HeapInfo = HeapInfo)

  fun print s = TextIO.output(TextIO.stdOut, s)
  fun error s = TextIO.output(TextIO.stdOut, "ERROR: " ^ s)

  fun gen_list 0 xs = xs
    | gen_list n xs = gen_list (n-1) ((ref (HeapInfo.C{count = ref n, id = n, vars = ref xs}))::(rev xs))
    
  fun do_test () =
    let
      val _ = print "Starting testing...\n"
      val _ = print "Testing end...\n"
      val temp_list = gen_list 10 []
      val h = foldl (fn (n, acc) => Heap.insert n acc) Heap.empty temp_list
      val res = Heap.layout h
      val out_stream = TextIO.openOut ("test1.vcg")
      val _ = Heap.export_heap_vcg "Heap" h out_stream
      val _ = TextIO.closeOut out_stream
      val _ = print ("Max element: " ^ (Heap.HeapInfo.layout (Heap.find_max h)) ^ "\n")
      val _ = print ("Min element: " ^ (Heap.HeapInfo.layout (Heap.find_min h)) ^ "\n")
      val _ = case (Heap.find_min h) of
	ref (HeapInfo.C({count, ...})) => count := !count+1
      val _ = case (Heap.find_max h) of
	ref (HeapInfo.C({count, ...})) => count := !count+1
      val (_,h) = Heap.delete_min h
      val _ = case (Heap.find_max h) of
	ref (HeapInfo.C({count, ...})) => count := !count+1
      val out_stream = TextIO.openOut ("test2.vcg")
      val _ = Heap.export_heap_vcg "Heap; min element incremented." h out_stream
      val _ = TextIO.closeOut out_stream

    in
      print res
    end
end

(*
structure DoTest = TestHeap()
*)
