(*$RegFlow: LVARS CON EXCON PRETTYPRINT CRASH FLAGS EFFECT RTYPE
MUL_EXP REG_FLOW*)

(* Region Flow Analysis: first pass of Storage Mode Analysis *)


functor RegFlow(    
    structure Lvars: LVARS
    structure Con : CON
    structure Excon: EXCON
    structure PrettyPrint: PRETTYPRINT
    structure Crash: CRASH
    structure Flags: FLAGS
    structure Eff: EFFECT
    structure RType: RTYPE
    structure MulExp: MUL_EXP
      sharing type PrettyPrint.StringTree=Eff.StringTree
      sharing type RType.effect = Eff.effect = Eff.place = MulExp.effect = MulExp.place
      sharing type Lvars.lvar = MulExp.lvar
      sharing type MulExp.il = RType.il
    ): REG_FLOW = 

struct
  structure PP = PrettyPrint
  type lvar = Lvars.lvar
  type place = Eff.place
  type effect = Eff.effect
  type mul = MulExp.mul
  type qmularefset = MulExp.qmularefset
  type excon = Excon.excon
  type StringTree = PrettyPrint.StringTree

  type ('a,'b,'c)LambdaPgm = ('a,'b,'c)MulExp.LambdaPgm
  type pgm = (place, place*mul, qmularefset ref)MulExp.LambdaPgm
  type exp = (place, place*mul, qmularefset ref)MulExp.LambdaExp
  type trip = (place, place*mul, qmularefset ref)MulExp.trip


  (* ---------------------------------------------------------------------- *)
  (*    General Abbreviations                                               *)
  (* ---------------------------------------------------------------------- *)

  fun log s             = output(!Flags.log,s ^ "\n")
  fun device(s)         = output(!Flags.log, s)            
  fun dump(t)           = PrettyPrint.outputTree(device, t, !Flags.colwidth)
  fun die errmsg        = Crash.impossible ("RegFlow." ^ errmsg)
  fun unimplemented x   = Crash.unimplemented ("RegFlow." ^ x)

  (* -----------------------------------------------------------------------*)
  (* Debugging flags; updated from Flags each time main function in module  *)
  (* (AtInference) is called                                                *)
  (* -----------------------------------------------------------------------*)

  val debug_which_at = ref false
  val all_attop      = ref false
  val enhancedSMA    = ref false  (* Sestoft & Bertelsen optimization *)

  (* ---------------------------------------------------------------------- *)
  (*    Utility functions                                                   *)
  (* ---------------------------------------------------------------------- *)

  fun footnote(x,y) = x
  infix footnote

  fun noSome x errmsg =
    case x of 
      None => die errmsg
    | Some y => y


  fun equal_places' p q = Eff.eq_effect(p,q)


  (* ---------------------------------------------------------------------- *)
  (*    Region-flow Graphs                                                  *)
  (* ---------------------------------------------------------------------- *)

  type nodeVal = effect

  fun eq_nodeVal(p1, p2) = Eff.eq_effect(p1,p2)

  fun key_of_node(nodeVal) = Eff.key_of_eps_or_rho nodeVal

  fun pp_nodeVal p = PP.flatten1(Eff.layout_effect p)


  exception Find
  fun find [] x = raise Find
    | find ((x',y)::rest) x = if eq_nodeVal(x,x') then y else find rest x

  type visited = bool

  datatype graph = NODE of nodeVal * visited ref * graph list ref

  fun reachable(n) =
      let
        fun reachable(NODE(p,v,ref L), acc) = 
                if !v then acc
                else 
                  (v := true;
                   reachable_edges(L, p:: acc))
        and reachable_edges ([],acc) = acc
          | reachable_edges (n::rest,acc) = reachable_edges(rest, reachable(n,acc))

        fun revisit(NODE(p,v,ref L)) = 
          if !v then (v:= false; List.apply revisit L)
          else ()

      in
        reachable (n, []) footnote revisit n
      end
 
  fun eq_graph (NODE(p1,v1,r1))(NODE(p2,v2,r2)) =
      r1=r2 (*andalso v1=v2 andalso eq_nodeVal(p1,p2)*)

  type level = int

  structure Array = NewJersey.Array
  val regmap_size = 1000

  abstype regmap = REGMAP of (nodeVal * graph)list Array.array
        (* hash table from keys (nodeVal mod regmap_size) to
           buckets of nodes with the same hash key *)

  with
    fun array_of(ref(REGMAP a)) = a
    val R = ref(REGMAP(Array.array(regmap_size, [])))

    fun lookup_assoc p [] = None
      | lookup_assoc p ((p', graph)::rest) =
           if eq_nodeVal(p,p') then Some graph 
           else lookup_assoc p rest

    fun lookup_R p : graph Option=
      (* lookup p first in the binary tree and then in the association list *)
      lookup_assoc p (Array.sub(array_of R, key_of_node p mod regmap_size)) 
      
    fun new_graph(p) = NODE(p, ref false, ref[])

    fun lookup_R_with_insert (p: nodeVal) = 
      let val i = key_of_node p mod regmap_size
          val l = Array.sub(array_of R, i)
      in
         (case lookup_assoc p l of
                     Some g => g
                   | None => (*insert (p, new_graph p) in association list *)
                             let val g = new_graph(p)
                             in Array.update(array_of R, i, (p,g)::l);
                                g
                             end)
      end  
    (* add_node_iter p:  add p to graph, if it has not been added already*)
    fun add_node_iter p = (lookup_R_with_insert p; ())

    fun add_edge_graph_iter(p: nodeVal, (g as NODE(p',_,_)): graph) =
      case lookup_R p of
        Some (NODE(_,_, r' as (ref subG))) => 
           r':= (if (List.exists (eq_graph g) subG) then subG
                else 
                   ((*log ("adding edge from " ^ (pp_nodeVal p) ^ " to " 
                         ^ (pp_nodeVal p') ^ "\n");*)
                    g::subG))
       | None =>
         Crash.impossible ("add_edge_graph_iter: can't find node " ^ pp_nodeVal p)

    (* add edge from node labelled by p, which must exist, to node labelled q (which
       may be created) *)
     
    fun add_edge_iter(p: nodeVal, q: nodeVal) =
      add_edge_graph_iter(p, lookup_R_with_insert q)

    (* connecting a region variable to a global region variable
       with the same runtime type *)
 
    fun connect_to_global rho : unit= 
       case Eff.get_place_ty rho of
         Some Eff.WORD_RT => add_edge_iter(rho,Eff.toplevel_region_withtype_word)
       | Some Eff.STRING_RT =>       add_edge_iter(rho,Eff.toplevel_region_withtype_string)
       | Some Eff.REAL_RT =>       add_edge_iter(rho,Eff.toplevel_region_withtype_real)
       | Some Eff.TOP_RT =>       add_edge_iter(rho,Eff.toplevel_region_withtype_top)
       | Some Eff.BOT_RT => (add_edge_iter(rho,Eff.toplevel_region_withtype_bot);
                             add_edge_iter(rho,Eff.toplevel_region_withtype_word);
                             add_edge_iter(rho,Eff.toplevel_region_withtype_string);
                             add_edge_iter(rho,Eff.toplevel_region_withtype_real);
                             add_edge_iter(rho,Eff.toplevel_region_withtype_top))
       | None => die "connect_to_global"


    fun init_regmap() = R:= REGMAP(Array.array(regmap_size, []))

    (* find the places that are reachable from the place p *)
    fun reachable_in_graph_with_insertion p =
        List.foldL (fn nodeVal => fn acc: place list =>
                     if Eff.is_rho nodeVal then nodeVal :: acc else acc)
                   []
                   (reachable(lookup_R_with_insert p))

    (* Find the places in the graph reachable from any place or 
     arrow effect in the list ps *)

    fun reachable_with_insertion ps =
	let fun reachable (node as NODE(p,v,ref L), acc) = 
	        if !v then acc
		else (v := true; reachable_edges(L, node :: acc))
	    and reachable_edges ([],        acc) = acc
	      | reachable_edges (n :: rest, acc) = 
		reachable_edges(rest, reachable(n, acc))
	    fun loop ([],    acc) = acc
	      | loop (p::pr, acc) =
	        loop (pr, reachable(lookup_R_with_insert p, acc))
	    val reachableNodes = loop (ps, [])
	in 
	    List.foldL (fn NODE(nodeVal, v, _) => fn acc : place list =>
			(v := false; 
                         if Eff.is_rho nodeVal then nodeVal :: acc else acc))
	                [] reachableNodes
	end

  end (*abstype*)

  (* ---------------------------------------------------------------------- *)
  (*    Creating a Region Flow Graph                                        *)
  (* ---------------------------------------------------------------------- *)

  fun insert(arreff): unit = (* assuming arreff = eps.phi, insert(arreff) makes 
                                an edge from eps to every region and effect variable
                                which occurs free in phi *)
      let 
         val children = Eff.represents arreff
                        handle ex => die ("insert " ^ pp_nodeVal arreff)
      in 
         (* make sure arreff is inserted *)
         add_node_iter(arreff);
         List.apply (fn child => 
                       if Eff.is_rho child orelse 
                          Eff.is_arrow_effect child
                       then add_edge_iter(arreff, child)
                       else ()) children
      end

  local
     open MulExp
  in 

    (* finding the region-polymorphic functions that are exported from a 
       program unit: *)

    exception FRAME_NOT_FOUND

    fun find(TR(e,_,_,_)) = find_exp e
    and find_exp e =
         let 
           fun find_sw(SWITCH(_,branches,otherwise)) =
           let
               val sub_phrases = case otherwise of None => map #2 branches
                                 | Some phr => phr :: map #2 branches
               fun loop [] = raise FRAME_NOT_FOUND
                 | loop (tr::trs) =
                        find tr handle FRAME_NOT_FOUND => loop trs
           in
               loop sub_phrases
           end
         in
          case e of 
            FIX{scope, ...} => find scope
          | LET{scope, ...} => find scope
          | EXCEPTION(_,_,_,_,scope) => find scope
          | SWITCH_I(sw) => find_sw sw
          | SWITCH_S(sw) => find_sw sw
          | SWITCH_C(sw) => find_sw sw
          | SWITCH_E(sw) => find_sw sw
          | FRAME{declared_lvars, ...} => map #lvar declared_lvars
          | _ => raise FRAME_NOT_FOUND
         end

    fun mk_graph0 trip = 
      let
         val exported = find trip handle FRAME_NOT_FOUND => die "frame not found"
         fun is_exported lvar = List.exists (fn lvar_frame => Lvars.eq(lvar, lvar_frame)) exported

         fun mk_graph_exp(e: exp): unit =
         case e of
            FIX {free, shared_clos, functions, scope} =>
              let 
                fun mk_graph_lvar {lvar,occ = instances, tyvars,rhos= formal_regvars,
                                   epss = formal_arreffs,Type,rhos_formals,
                                   bound_but_never_written_into,
                                   other,bind} =
                      let

                         val _ = List.apply insert formal_arreffs

                         (*val _ = log("lvar = " ^ Lvars.pr_lvar lvar ^ ":" ^ Int.string(List.size formal_regvars)) *)
                         
                         (* region-polymorphic functions which are exported must have their formal
                            region parameters connected to global regions with the same runtime type.
                            This is necessary for soundness of the analysis across program units.
                         *)

                         fun deal_with_one_instance(il) =
                             let val (taus, actual_rhos, actual_epss) = RType.un_il il
                             in
                                (* connect every formal region variable to corresponding actual region variable *)
                                List.apply add_edge_iter
                                (ListPair.zip(formal_regvars, actual_rhos)
                                 handle ListPair.Zip => die ("deal_with_one_instance (1)"
                                                   ^ Int.string(List.size formal_regvars)
                                                   ^ " " ^ 
                                                     Int.string(List.size actual_rhos)));
     
                                (* connect every formal effect variable to corresponding actual effect variable *)
                                List.apply add_edge_iter
                                (ListPair.zip(formal_arreffs,actual_epss)
                                 handle ListPair.Zip => die "deal_with_one_instance (2)");
     
                                List.apply insert actual_epss
     
                             end
     
                      in
                        List.apply add_node_iter formal_regvars;
                        if is_exported lvar then List.apply connect_to_global formal_regvars
                        else ();
                        List.apply add_node_iter formal_arreffs;
                        List.apply deal_with_one_instance instances;
                        mk_graph bind
                      end
              in
                  List.apply mk_graph_lvar functions;
                  mk_graph scope
              end
          (* from here on only trivial traversal*)
          | FN{body, ...} => mk_graph body
          | LET {bind, scope, ...} => 
                (mk_graph bind; mk_graph scope)
          | APP(_,_,t1,t2) => (mk_graph t1;mk_graph t2)           
          | EXCEPTION(_,_,_,_, tr) => mk_graph tr
          | RAISE(tr) => mk_graph tr
          | HANDLE(tr1,tr2) => (mk_graph tr1; mk_graph tr2)
          | SWITCH_I(switch) => mk_graph_i switch
          | SWITCH_S(switch) => mk_graph_s switch
          | SWITCH_C(switch) => mk_graph_c switch
          | SWITCH_E(switch) => mk_graph_e switch
          | CON1(_,tr) => mk_graph tr
          | DECON(_,tr) => mk_graph tr
          | EXCON(_,Some(_,tr)) => mk_graph tr
          | DEEXCON(_,tr) => mk_graph tr
          | RECORD(_, trs) => List.apply mk_graph trs
          | UB_RECORD(trs) => List.apply mk_graph trs
          | SELECT(_, tr) => mk_graph tr
          | DEREF tr => mk_graph tr
          | REF(_,tr) => mk_graph tr
          | ASSIGN(_,tr1,tr2) => (mk_graph tr1; mk_graph tr2)
          | EQUAL(_,tr1, tr2) => (mk_graph tr1; mk_graph tr2)
          | CCALL(_,trs) => List.apply mk_graph trs
          | RESET_REGIONS(_,tr) => mk_graph tr
          | LETREGION{body, ...} => mk_graph body
          | _ => ()
      
        and mk_graph_i(SWITCH(tr, list,e')) = 
            (mk_graph tr;
             List.apply (mk_graph o #2) list;
             mk_graph_opt e'
            )
        and mk_graph_s(SWITCH(tr, list,e')) = 
            (mk_graph tr;
             List.apply (mk_graph o #2) list;
             mk_graph_opt e'
            )
        and mk_graph_r(SWITCH(tr, list,e')) = 
            (mk_graph tr;
             List.apply (mk_graph o #2) list;
             mk_graph_opt e'
            )
        and mk_graph_c(SWITCH(tr, list,e')) = 
            (mk_graph tr;
             List.apply (mk_graph o #2) list;
             mk_graph_opt e'
            )
        and mk_graph_e(SWITCH(tr, list,e')) = 
            (mk_graph tr;
             List.apply (mk_graph o #2) list;
             mk_graph_opt e'
            )
        and mk_graph_opt None = ()
          | mk_graph_opt (Some e) = mk_graph e
     
        and mk_graph(TR(e, _, _, _)) = mk_graph_exp e

      in
          mk_graph trip
      end
  end (* local *)
   (* resulting functions: *)

  fun mk_graph(MulExp.PGM{expression,...})= 
      (init_regmap();
       mk_graph0 expression)

end; (*RegFlow*)
