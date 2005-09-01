
structure PhysSizeInf: PHYS_SIZE_INF =
  struct
    structure RegvarFinMap = EffVarEnv
    structure PP = PrettyPrint
    structure LvarMap = Lvars.Map

    open MulExp
    structure E = Effect
    structure RE = RegionExp
    structure R = RType
    type 'a at = 'a AtInf.at


   (* -----------------------------------------------------------------
    * Dynamic flags
    * ----------------------------------------------------------------- *)

    val print_all_program_points = Flags.lookup_flag_entry "print_all_program_points"

    (* ----------------------------------------------------------------------
     * General Abbreviations and some utilities                                              
     * ---------------------------------------------------------------------- *)
    fun log s = TextIO.output(!Flags.log,s ^ "\n")
    fun msg s = (TextIO.output(TextIO.stdOut, s ^ "\n"); TextIO.flushOut TextIO.stdOut)
    fun die s = Crash.impossible ("PhysSizeInf."^s)
    fun log_st st = PP.outputTree(fn s => TextIO.output(!Flags.log,s), st, 70)
    fun pr_st st = PP.outputTree(print, st, 70)
    fun noSome NONE s = die s
      | noSome (SOME x) s = x
    fun map_opt f (SOME x) = SOME (f x)
      | map_opt f NONE = NONE
    fun apply_opt f (SOME x) = f x
      | apply_opt f NONE = ()

    fun place_atplace (atp: place at) : place option =
      let open AtInf
      in case atp
	   of ATTOP p => SOME p
	    | ATBOT p => SOME p
	    | SAT p => SOME p
	    | IGNORE => NONE
      end  

    (* --------------------------------------------------- 
     * Inserting Free Variables in MulExp
     * Buckets are used to hold free variables.
     * --------------------------------------------------- *)

    local (* free vars *)
      type fvs = lvar list * place list * excon list
      fun layout_lvar lv = PP.LEAF (Lvars.pr_lvar lv)
      fun layout_excon excon = PP.LEAF (Excon.pr_excon excon)
      fun layout_fvs (s,(lvars, excons, places)) =
	PP.NODE{start=s ^ " {",finish=" }",indent=1,childsep=PP.RIGHT ", ",
		children=(map layout_lvar lvars) @ (map layout_excon excons) @ (map E.layout_effect places)}
      fun layout_fvs' (lv, ref (SOME (lvars, excons, places))) =
	PP.NODE{start=Lvars.pr_lvar lv ^ " -> [",finish="]",indent=1,childsep=PP.RIGHT ",",
		children=(map layout_lvar lvars) @ (map layout_excon excons) @ (map E.layout_effect places)}
	| layout_fvs' (lv,_) = PP.LEAF(Lvars.pr_lvar lv ^ " -> <<not available>>")
      val pr_fvs = pr_st o layout_fvs'
      val log_fvs' = log_st o layout_fvs'

      local (* buckets *)
	val lvar_bucket = ref ([]:lvar list)
	val excon_bucket = ref ([]:excon list)
	val place_bucket = ref ([]:place list)
	  
	fun gen_marker(bucket, get_ref) =
	  let fun is_marked var = !(get_ref var)
	      fun mark var = get_ref var := true
	      fun unmark var = get_ref var := false
	      fun add var = if is_marked var then ()
			    else (bucket := (var :: !bucket); mark var)
	  in (mark, unmark, add)
	  end

      in (* local buckets *)

	val (mark_place, unmark_place, add_place) = gen_marker(place_bucket, E.get_visited)

	val (mark_lvar, unmark_lvar, add_lvar) = gen_marker(lvar_bucket, Lvars.is_free)

	fun add_excon (excon:excon) : unit =
	  if List.exists (fn excon' => Excon.eq(excon,excon')) (!excon_bucket) then ()
	  else excon_bucket := excon :: (!excon_bucket)
	    
	fun add_atp atp = case place_atplace atp
			    of SOME p => add_place p
			     | NONE => () 
	fun mark_atp atp = case place_atplace atp
			     of SOME p => mark_place p
			      | NONE => () 
	fun unmark_atp atp = case place_atplace atp
			       of SOME p => unmark_place p
				| NONE => () 

	fun kill_excon (excon:excon) : unit =
	  let fun kill [] = []
		| kill (excon'::excons) = if Excon.eq(excon',excon) then excons
					  else excon'::kill excons
	  in excon_bucket := kill (!excon_bucket)
	  end
	
	fun reset_fvs () = (List.app unmark_lvar (!lvar_bucket);
			    List.app unmark_place (!place_bucket);
			    lvar_bucket := [];
			    excon_bucket := [];
			    place_bucket := [])
	fun get_fvs () =
	  let val fvs = (!lvar_bucket, !excon_bucket, !place_bucket)
	  in reset_fvs (); fvs
	  end

      end (*local buckets*)


      (* ------------------------------------------------------
       * Assume free-vars info on fix and fn subexpressions.
       * ------------------------------------------------------ *)
     
      fun fv (TR(e,_,_,_): (place at,place*mul,unit)trip) : unit =
	let fun fv_sw (SWITCH(tr,choices,opt)) = (fv tr; List.app (fv o #2) choices;
						  case opt of SOME tr => fv tr | NONE => ())
	in case e
	     of VAR{lvar,rhos_actuals=ref actuals,...} =>
	       (add_lvar lvar; List.app add_atp actuals)
	      | INTEGER(n,t,alloc) => add_atp alloc
	      | WORD(n,t,alloc) => add_atp alloc
	      | STRING(s,alloc) => add_atp alloc
	      | REAL(s,alloc) => add_atp alloc
	      | UB_RECORD trips => List.app fv trips
	      | FN{pat,body,free,alloc} =>
	       (case free
		  of ref (SOME (lvars,excons,places)) =>
		    (List.app add_lvar lvars;
		     List.app add_excon excons;
		     List.app add_place places;
		     add_atp alloc)
		   | _ => die "fv.FN.free vars not available.") 
	      | LETREGION{B,rhos=ref rhos,body} =>
		  (List.app (fn (place,mul) => mark_place place) rhos;
		   fv body;
		   List.app (fn (place,mul) => unmark_place place) rhos)
	      | LET{k_let,pat,bind,scope} =>
		  (fv bind;
		   List.app (mark_lvar o #1) pat;
		   fv scope;
		   List.app (unmark_lvar o #1) pat)
	      | FIX{free,shared_clos,functions,scope} =>
		  (case free
		     of ref (SOME (lvars,excons,places)) =>
		       (List.app add_lvar lvars;
			List.app add_excon excons;
			List.app add_place places;
			add_atp shared_clos;
			List.app (mark_lvar o #lvar) functions;
			fv scope;
			List.app (unmark_lvar o #lvar) functions)
		      | _ => die "fv.FIX.free vars not available.") 
	      | APP(_,_,tr1,tr2) => (fv tr1; fv tr2)
	      | EXCEPTION(excon,b,tp,alloc,scope) =>
		     (fv scope; add_atp alloc; 
		      kill_excon excon)
	      | RAISE tr => fv tr
	      | HANDLE(tr1,tr2) => (fv tr1; fv tr2)
	      | SWITCH_I {switch, precision} => fv_sw switch
	      | SWITCH_W {switch, precision} => fv_sw switch
	      | SWITCH_S sw => fv_sw sw
	      | SWITCH_C sw => fv_sw sw
	      | SWITCH_E sw => let val SWITCH(_,choices,_) = sw
			       in List.app (add_excon o #1) choices;
				 fv_sw sw
			       end
	      | CON0 {con, il, aux_regions, alloc} => (List.app add_atp aux_regions;
						       add_atp alloc)
	      | CON1 ({con, il, alloc}, tr) => (add_atp alloc; fv tr)
	      | DECON ({con, il}, tr) => fv tr
	      | EXCON (excon, opt) => (case opt
					 of SOME (alloc,tr) => (add_excon excon; add_atp alloc; fv tr)
					  | NONE => add_excon excon)
	      | DEEXCON (excon,tr) => (add_excon excon; fv tr)
	      | RECORD (alloc, trs) => (add_atp alloc; List.app fv trs)
	      | SELECT (i, tr) => fv tr
	      | DEREF tr => fv tr
	      | REF (alloc,tr) => (add_atp alloc; fv tr)
	      | ASSIGN (alloc,tr1,tr2) => (add_atp alloc; fv tr1; fv tr2)
	      | DROP tr => fv tr
	      | EQUAL ({mu_of_arg1, mu_of_arg2, alloc}, tr1,tr2) => (add_atp alloc; fv tr1; fv tr2)
	      | CCALL ({rhos_for_result, ...}, trs) => (List.app (add_atp o #1) rhos_for_result;
							List.app fv trs)
	      | EXPORT (_,tr) => fv tr
	      | RESET_REGIONS ({force, alloc,regions_for_resetting}, tr) => 
                      (add_atp alloc; 
                       List.app add_atp regions_for_resetting;
                       fv tr)
	      | FRAME{declared_lvars, declared_excons} =>
			       (List.app (add_lvar o #lvar) declared_lvars;
				List.app (add_excon o #1) declared_excons)
	end
      

      (* ----------------------------------------------------------
       * Insert free variables in fn and fix by calling fv, 
       * buttom-up.
       * ---------------------------------------------------------- *)
     
      fun getOpt (SOME l) = l
        | getOpt NONE =[]

      fun ifv (TR(e,_,_,_): (place at,place*mul,unit)trip) : unit =
	let fun ifv_sw (SWITCH(tr,choices,opt)) = (ifv tr; List.app (ifv o #2) choices;
						   case opt of SOME tr => ifv tr | NONE => ())
	in case e
	     of VAR _ => ()
	      | INTEGER _ => ()
	      | WORD _ => ()
	      | STRING _ => ()
	      | REAL _ => ()
	      | UB_RECORD trips => List.app ifv trips
	      | FN{pat,body,free,alloc} => (ifv body;
					    List.app (mark_lvar o #1) pat;
					    fv body;
					    free := (SOME (get_fvs()));
					    List.app (unmark_lvar o #1) pat)
	      | LETREGION{B,rhos,body} => ifv body
	      | LET{k_let,pat,bind,scope} => (ifv bind; ifv scope)
	      | FIX{free,shared_clos,functions,scope} =>
	       let fun ifv_under_bind (TR(FN{body,...},_,_,_)) = ifv body
		     | ifv_under_bind _ = die "FIX.bind not fn."

		   fun ifv_bind (TR(FN{free,pat,body,alloc(*same as shared_clos*)}, _, _, _)) =
		        (List.app (mark_lvar o #1) pat;
			 fv body;
			 free := (SOME (get_fvs()));
			 List.app (unmark_lvar o #1) pat)
		     | ifv_bind _ = die "FIX.bind not fn(2)."
		   fun fv_bind (TR(FN{free,pat,body,alloc(*same as shared_clos*)}, _, _, _)) =
		     (case free
			of ref (SOME (lvars,excons,places)) =>
			  (List.app add_lvar lvars;
			   List.app add_excon excons;
			   List.app add_place places)  (* Region containing shared closure, alloc, is not free in letrec bound FN *)
		      | _ => die "FIX.fv_bind free vars not available.")
		     | fv_bind tr = die "FIX.fv_bind not fn."
	       in ifv scope;
  	 	  List.app (ifv_under_bind o #bind) functions;
		  List.app (mark_lvar o #lvar) functions;
		  List.app ((List.app (mark_place o #1)) o ! o #rhos_formals) functions;
		  List.app ((List.app (mark_place o #1)) o getOpt o #bound_but_never_written_into) functions;
		  (*mark_atp shared_clos;    commented out, 23/4/97, mads *)
		  List.app (ifv_bind o #bind) functions;
		  List.app (fv_bind o #bind) functions;  (* use fv_bind instead of fv 14/06-2000, Niels *)
		  free := (SOME (get_fvs()));
		  (*unmark_atp shared_clos;     commented out, 23/4/97, mads *)
		  List.app ((List.app (unmark_place o #1)) o getOpt o #bound_but_never_written_into) functions;
		  List.app ((List.app (unmark_place o #1)) o ! o #rhos_formals) functions;
		  List.app (unmark_lvar o #lvar) functions
(*
		;case functions
		  of [{lvar,...}] => pr_fvs (lvar, free)
		   | _ => () 
*)
	       end
	      | APP(_,_,tr1,tr2) => (ifv tr1; ifv tr2)
	      | EXCEPTION(excon,b,tp,alloc,scope) => ifv scope
	      | RAISE tr => ifv tr
	      | HANDLE(tr1,tr2) => (ifv tr1; ifv tr2)
	      | SWITCH_I {switch,precision} => ifv_sw switch
	      | SWITCH_W {switch,precision} => ifv_sw switch
	      | SWITCH_S sw => ifv_sw sw
	      | SWITCH_C sw => ifv_sw sw
	      | SWITCH_E sw => ifv_sw sw
	      | CON0 {con, il, aux_regions, alloc} => ()
	      | CON1 ({con, il, alloc}, tr) => ifv tr
	      | DECON ({con, il}, tr) => ifv tr
	      | EXCON (excon, opt) => (case opt
					 of SOME (alloc,tr) => ifv tr
					  | NONE => ())
	      | DEEXCON (excon,tr) => ifv tr
	      | RECORD (alloc, trs) => List.app ifv trs
	      | SELECT (i, tr) => ifv tr
	      | DEREF tr => ifv tr
	      | REF (alloc,tr) => ifv tr
	      | ASSIGN (alloc,tr1,tr2) => (ifv tr1; ifv tr2)
	      | DROP (tr) => ifv tr
	      | EQUAL ({mu_of_arg1, mu_of_arg2, alloc}, tr1,tr2) => (ifv tr1; ifv tr2)
	      | CCALL (_, trs) => List.app ifv trs
	      | EXPORT (_, tr) => ifv tr
	      | RESET_REGIONS ({force, alloc,regions_for_resetting}, tr) => ifv tr
	      | FRAME{declared_lvars, declared_excons} => ()
	end
      
    in (*local*)
      val reset_fvs : unit -> unit = reset_fvs    
      fun insert_free_vars (tr, import_vars, export_vars) =
	(ifv tr;
	 import_vars := 
	 SOME let val (_, _, export_rhos) = export_vars
		  val _ = List.app mark_place export_rhos
		  val imp_vars = (fv(tr); get_fvs())
		  val _ = List.app unmark_place export_rhos
	      in imp_vars
	      end)
    end (*local*)


    (* -------------------------------------------------------------------
     * Physical Size Inference: Exchange multiplicities in MulExp with
     * physical sizes.
     * ------------------------------------------------------------------- *)

    datatype phsize = INF | WORDS of int
    fun phsize_max _ INF = INF
      | phsize_max INF _ = INF
      | phsize_max (WORDS i) (WORDS i') = WORDS (Int.max(i,i'))
    fun pr_phsize INF = "inf"
      | pr_phsize (WORDS i) = Int.toString i
    fun layout_phsize phsize = PP.LEAF (pr_phsize phsize)

    (* ----------------------------------------------------------------------
     * Physical sizes of storable (boxed) values.
     * ---------------------------------------------------------------------- *)

    fun size_of_real a = (WORDS o RegConst.size_of_real) a
    fun size_of_ref a = (WORDS o RegConst.size_of_ref) a
    fun size_of_record a = (WORDS o RegConst.size_of_record) a
    fun closure_size(l1,l2,l3) = (WORDS o RegConst.size_closure) (l1,l2,l3)
    fun fix_closure_size(l1,l2,l3) = (WORDS o RegConst.size_fix_closure)(l1,l2,l3)
    fun size_region_vector l = (WORDS o RegConst.size_region_vector) l
    fun size_exname() = (WORDS o RegConst.size_exname)()
    fun size_excon0() = (WORDS o RegConst.size_excon0)()
    fun size_excon1() = (WORDS o RegConst.size_excon1)()
    fun size_nullery_exn() = WORDS(RegConst.size_exname() + RegConst.size_excon0())
    fun size_con0() = (WORDS o RegConst.size_con0)()
    fun size_con1() = (WORDS o RegConst.size_con1)()

    (* ----------------------------------------------------------------------
     * Environment to be used for building graph
     * ---------------------------------------------------------------------- *)

    datatype range_env = FORMAL_REGVARS of place list
                       | FORMAL_SIZES of phsize list
                       | NOTFIXBOUND 

    type env = range_env LvarMap.map
    val empty = LvarMap.empty
    fun plus a = LvarMap.plus a
    fun add_env a = LvarMap.add a
    fun lookup_env env lv = LvarMap.lookup env lv
    val init = empty

    fun equal_res (FORMAL_SIZES phs1,FORMAL_SIZES phs2) = (phs1 = phs2)
      | equal_res (NOTFIXBOUND, NOTFIXBOUND) = true
      | equal_res (FORMAL_REGVARS _, _) = die "equal_res"
      | equal_res (_, FORMAL_REGVARS _) = die "equal_res"
      | equal_res _ = false

    fun enrich(env1,env2) =
      LvarMap.Fold(fn ((lv2,res2),b) => b andalso
		   case LvarMap.lookup env1 lv2
		     of SOME res1 => equal_res(res1,res2)
		      | NONE => false) true env2
    fun restrict(env,lvars) =
      foldl(fn (lv, acc) =>
	    case LvarMap.lookup env lv
	      of SOME res => add_env(lv,res,acc)
	       | NONE => die "restrict") empty lvars 
      
    type StringTree    = PP.StringTree
    fun layout_list (p : 'a -> StringTree,first,sep,last) (l: 'a list) : StringTree =
      PP.NODE{start=first,finish=last,indent=1,childsep=PP.RIGHT sep,
	      children=map p l}
    fun layout_range (FORMAL_REGVARS places) = layout_list (E.layout_effect, "[", ",", "]") places
      | layout_range (FORMAL_SIZES phsizes) = layout_list (layout_phsize, "[", ",", "]") phsizes
      | layout_range NOTFIXBOUND = PP.LEAF "NOTFIXBOUND"
    val layout_env = LvarMap.layoutMap {start="psi_env: {", finish="}",sep=",",eq=" -> "}
      (PP.LEAF o Lvars.pr_lvar) layout_range
    
    (*
     * We build a graph, with one node for each binding occurrence of a 
     * region variable with logical size 1.  The info of each node is the
     * minimum physical size of the region (updated by each '... at rho' where 
     * rho is a region variable with logical size 1).  There is an edge
     * between two nodes, if and only if the physical size of the 
     * region corresponding to the first node shall be greater than or 
     * equal to the physical size of the region corresponding to the second 
     * node. When the graph has been evaluated using function eval_psi_graph, 
     * each node contains the physical size of the region it corresponds to.
     *)

    type psi_info = phsize ref
    val psi_graph : psi_info DiGraph.graph ref = ref (DiGraph.mk_graph ())
    fun reset_graph() = psi_graph := DiGraph.mk_graph()
    fun mk_node i = 
      let val n = DiGraph.mk_node i 
      in psi_graph := DiGraph.add_node_to_graph(n,!psi_graph); n 
      end
    fun eval_psi_graph () : unit =
      let
	(* val _ = log "eval_psi_graph" *)
	fun max_list [] = die "eval_psi_graph"
	  | max_list (i::is) = List.foldl (fn (i,b)=>phsize_max i b) i is
	val layout_info = layout_phsize o !
	  
	val sccs  = DiGraph.scc layout_info (!psi_graph)
	val compressed_sccs = 
	  map (DiGraph.union_graph 
	       (fn (i1,i2) => (i2 := phsize_max (!i1) (!i2); i2))) 
	  sccs
      in
	DiGraph.bottom_up_eval 
	(fn (i, is) => i := max_list (map (op !) (i::is))) (!psi_graph)
      end 

    (* --------------------------------------------------------------------------------
     * psi_env: (intern) 
     *   An environment which maps (only) regvars with logical size 1 
     *   to nodes in the graph. 
     * -------------------------------------------------------------------------------- *)

    type psi_env = psi_info DiGraph.node RegvarFinMap.map
    val psi_env  : psi_env ref = ref RegvarFinMap.empty
    fun reset_psi_env() = psi_env := RegvarFinMap.empty
      
    fun psi_declare place = 
      psi_env := RegvarFinMap.add (place,mk_node (ref (WORDS 0)),!psi_env)
      
    fun psi_lookup place = RegvarFinMap.lookup (!psi_env) place
     
    fun psi_add_place_size (place,size') =
      case psi_lookup place 
	of SOME n => 
	  (case DiGraph.find_info n 
	     of (r as ref size) => r := phsize_max size size')
	 | NONE => ()
	     
    fun psi_add_edge (actual_regvar,formal_regvar) =
      case (psi_lookup actual_regvar, psi_lookup formal_regvar) 
	of (SOME n1, SOME n2) => (* both regvars have mul 1 *)
	  if DiGraph.eq_nodes(n1,n2) then ()
	  else DiGraph.mk_edge(n1,n2) 
	 (* the actual must have physical size greater than or equal to
	  * physical size of the formal *)
	 | (NONE, SOME n2) => () (* actual mul infinite, formal mul 1 *)
	 | (NONE, NONE) => ()    (* both regvars have mul infinite *)
	 | (SOME n1, NONE) => () (* formal mul infinite, actual mul finite. This
				  * is ok. Applying a curried function, for instance,
				  * to one argument, may not cause any allocation. 
				  * Multiplicity inference figures this out.. *)

    fun phsize_place place : phsize =
      case psi_lookup place 
	of SOME n => !(DiGraph.find_info n)
	 | NONE => INF


    (* ------------------------------------------------------------
     *  Frame part of environment for fix bound lvars.
     * ------------------------------------------------------------ *)

    val frame_env = ref (empty : env)

    fun convert_env env =
      let fun ch_entry (FORMAL_REGVARS places) = FORMAL_SIZES (map phsize_place places)
	    | ch_entry (FORMAL_SIZES sizes) = FORMAL_SIZES sizes
	    | ch_entry NOTFIXBOUND = NOTFIXBOUND
      in LvarMap.composemap ch_entry env
      end  


    (* -------------------------------------------------
     * Physical size inference traversal - build graph
     * ------------------------------------------------- *)

    fun psi_sw psi_tr_env (SWITCH(tr,sel,opt)) =
      (psi_tr_env tr;
       List.app (fn (_,tr) => psi_tr_env tr) sel;
       case opt of NONE => () | SOME tr => psi_tr_env tr)
      
    fun psi_tr env (TR(e,_,_,_) : (place at, place*mul,unit)trip) =
      case e
	of VAR{lvar,fix_bound=false,rhos_actuals=ref [],...} => ()
	 | VAR _ => die "psi_tr.variables not fully applied as assumed." 
	 | INTEGER _ => ()
	 | WORD _ => ()
	 | STRING _ => ()  (* immediate strings are allocated statically.. *)
	 | REAL _ => ()    (* immediate reals are allocated statically.. *)
	 | UB_RECORD trips => List.app (psi_tr env) trips
	 | FN{pat,body,free=ref (SOME fvs),alloc} =>
	   (case place_atplace alloc
	      of SOME place => (psi_add_place_size (place,closure_size fvs);
				psi_tr env body)
	       | NONE => die "psi_tr.FN") 
	 | FN _ => die "psi_tr.FN.free vars not available."
 	 | LETREGION{B,rhos=ref rhos,body} => 
	  (List.app (fn (place,Mul.INF) => ()
	                | (place,Mul.NUM n) => if n = 1 orelse n = 0 then psi_declare place
					       else die "psi_tr.LETREGION.mul not in {0,1}")
	   rhos; psi_tr env body)
	 | LET{k_let,pat,bind,scope} => 
	  let val env' = List.foldl(fn (pat,acc) =>
				    add_env(#1 pat,NOTFIXBOUND,acc)) env pat
	  in psi_tr env bind; psi_tr env' scope
	  end
         | FIX{free=ref (SOME fvs),shared_clos,functions,scope} =>
	  let val env' = List.foldl
	                 (fn ({lvar,rhos_formals,...},env) =>
			  let val formals = map (fn (place,mul) => 
						 (case mul
						    of Mul.NUM 0 => psi_declare place
						     | Mul.NUM 1 => psi_declare place
						     | Mul.NUM _ => die "psi_tr.FIX"
						     | Mul.INF => (); place))
			                    (!rhos_formals)
			  in add_env (lvar,FORMAL_REGVARS formals, env)
			  end) 
	                 env functions
	  in 
	    case place_atplace shared_clos
	      of SOME place => (let val sz = fix_closure_size fvs
				in psi_add_place_size (place,sz);
				map (fn {bind=TR(FN{body,...},_,_,_),...} => psi_tr env' body
			              | _ => die "psi_tr.FIX.FN expected") functions;
				psi_tr env' scope end)
	       | NONE => die "psi_tr.FIX"
	  end
	 | FIX _ => die "psi_tr.free vars not available."
 	 | APP(_,_,tr1 as TR(VAR{lvar,fix_bound,rhos_actuals=ref atps,...},_,_,_), tr2) =>
	  let val actuals = map (fn atp => case place_atplace atp
					     of SOME place => place
					      | NONE => die "APP.actual atp is IGNORE.") atps
	  in case lookup_env env lvar                               		  
	       (* If lvar is bound in the program we add edges
		* between formals and actuals, otherwise we add
		* necessary sizes to the actuals.  *)
	       of SOME (FORMAL_REGVARS formals) =>
		 (List.app psi_add_edge (BasisCompat.ListPair.zipEq(actuals,formals))
		  handle BasisCompat.ListPair.UnequalLengths => 
		    die "psi_tr.APP.region_polymorphic_application: actuals differs from formals.")
		| SOME (FORMAL_SIZES sizes) => 
		 (List.app psi_add_place_size (BasisCompat.ListPair.zipEq (actuals, sizes))
		  handle BasisCompat.ListPair.UnequalLengths => 
		    die "psi_tr.APP.region_polymorphic_application.actuals differs from sizes.")
		| _ => ();
	     psi_tr env tr2
	  end
	 | APP(_,_,tr1,tr2) => (psi_tr env tr1; psi_tr env tr2)
	 | EXCEPTION(excon,b,mu,atp,tr) =>
	  let val place = case place_atplace atp
			    of SOME place => place
			     | NONE => die "psi_tr.EXCEPTION."
	  in 
	    if b then (* nullary exception *)
	      psi_add_place_size (place,size_nullery_exn())  (* was words 2 2001-01-18, Niels *)
	    else (* unary exception *)
	      psi_add_place_size (place,size_exname()); (* was words 2 2001-01-18, Niels *)
	     psi_tr env tr
	  end
	 | RAISE tr => psi_tr env tr
	 | HANDLE(tr1,tr2) => (psi_tr env tr1; psi_tr env tr2)
	 | SWITCH_I {switch,precision} => psi_sw (psi_tr env) switch
	 | SWITCH_W {switch,precision} => psi_sw (psi_tr env) switch
	 | SWITCH_S sw => psi_sw (psi_tr env) sw
	 | SWITCH_C sw => psi_sw (psi_tr env) sw
	 | SWITCH_E sw => psi_sw (psi_tr env) sw
	 | CON0 {con, il, aux_regions, alloc} => (case place_atplace alloc
						    of SOME place => psi_add_place_size(place,size_con0()) (* was 1 2001-01-18, Niels *)
						     | NONE => () (* unboxed_con *)) 
	 | CON1 ({con, il, alloc}, tr) => (case place_atplace alloc
					     of SOME place => psi_add_place_size(place,size_con1()) (* was 2 2001-01-18, Niels *)
					      | NONE => (); psi_tr env tr)
	 | DECON ({con, il}, tr) => psi_tr env tr
	 | EXCON (excon, opt) => (case opt
				    of SOME (alloc,tr) => 
				      (case place_atplace alloc
					 of SOME place => (psi_add_place_size (place,size_excon1()); psi_tr env tr) (* was 2 2001-01-18, Niels*)
					  | NONE => die "psi_tr.EXCON")
				     | NONE => ())
	 | DEEXCON (excon,tr) => psi_tr env tr
	 | RECORD (alloc, trs) => 
            (case (place_atplace alloc, trs)
	       of (NONE, []) => ()  (* unit *)
		| (SOME place, _) => psi_add_place_size(place, size_of_record trs)
		| _ => die "psi_tr.RECORD"; List.app (psi_tr env) trs) 
	 | SELECT (i, tr) => psi_tr env tr
	 | DEREF tr => psi_tr env tr
	 | REF (alloc,tr) => (case place_atplace alloc
				of SOME place => psi_add_place_size(place,size_of_ref())
				 | NONE => die "psi_tr.REF"; psi_tr env tr) 
	 | ASSIGN (alloc,tr1,tr2) => (case place_atplace alloc
					of SOME _ => die "psi_tr.ASSIGN"
					 | NONE => (psi_tr env tr1; psi_tr env tr2))
	 | DROP tr => psi_tr env tr
	 | EQUAL ({mu_of_arg1, mu_of_arg2, alloc}, tr1,tr2) =>
	       (case place_atplace alloc
		  of SOME _ => die "psi_tr.EQUAL"
		   | NONE => (psi_tr env tr1; psi_tr env tr2))
	 | CCALL ({name, rhos_for_result, ...}, trs) =>
	     (List.app (fn (atp, i_opt) =>
	       (case noSome (place_atplace atp) "psi_tr (CCALL ...): IGNORE" of rho =>
		 (case i_opt of
		    NONE => ()
		      (*rho has logical size infinity, so it will automagically
		       get physical size infinity: nothing need be done*)
		  | SOME 0 => ()
		      (*the region contains an unboxed type*)
		  | SOME i => psi_add_place_size (rho, WORDS i))))
	     rhos_for_result ; 
	     List.app (psi_tr env) trs)
	 | EXPORT(_,tr) => psi_tr env tr
	 | RESET_REGIONS ({force, alloc,regions_for_resetting}, tr) => psi_tr env tr
	 | FRAME{declared_lvars, ...} =>
	  let val env' = List.foldr (fn ({lvar,...},frame_env) =>
				     case lookup_env env lvar
				       of SOME res => add_env (lvar, res, frame_env)
					| NONE => die "psi_tr.FRAME.lv not in env")
	                 empty declared_lvars
	  in frame_env := env'
	  end


    (* --------------------------------------------------------------------
     * ips trip: insert physical sizes instead of multiplicities in MulExp.
     * Also, insert program points on each allocation point. For FIX 
     * parameters we use a dummy program point: pp = ~1
     * All result regions in primitive CCALL are annotated with the same 
     * program point.
     * -------------------------------------------------------------------- *)
    type pp = int
    val dummypp = ~1
    local open AtInf
    in fun pp_dummy IGNORE = IGNORE
	 | pp_dummy (ATTOP a) = ATTOP (a, dummypp)
	 | pp_dummy (ATBOT a) = ATBOT (a, dummypp)
	 | pp_dummy (SAT a) = SAT (a, dummypp)
    end

    fun ips (pp_c : unit -> pp) (TR(e,mt,ateffs,mulef)) =
      let local open AtInf
	  in fun gen_pp f IGNORE = IGNORE
	       | gen_pp f (ATTOP a) = ATTOP (a, f())
	       | gen_pp f (ATBOT a) = ATBOT (a, f())
	       | gen_pp f (SAT a) = SAT (a, f())
	  end
	  val pp = gen_pp pp_c
	  val ips = ips pp_c
	  fun ips_sw (SWITCH(tr,sel,opt)) =
	     SWITCH(ips tr,map (fn (a,tr) => (a,ips tr)) sel,
		    case opt of SOME tr => SOME (ips tr) | NONE => NONE)
	  fun bind_transform (place, mul) = (place, phsize_place place)
	  val e' =
	    case e
	      of VAR {lvar,il,plain_arreffs,fix_bound,rhos_actuals,other} => 
		VAR {lvar=lvar,il=il,plain_arreffs=plain_arreffs,fix_bound=fix_bound,
		     rhos_actuals=ref (map pp (!rhos_actuals)),other=other} 
	       | INTEGER (a,t,p) => INTEGER (a,t,pp p)
	       | WORD (a,t,p) => WORD (a,t,pp p)
	       | STRING (a,p) => STRING (a,pp p)
	       | REAL (a,p) => REAL (a,pp p)
	       | UB_RECORD trs => UB_RECORD (map ips trs) 
	       | FN {pat,body,free,alloc} => FN {pat=pat,body=ips body,free=free,alloc=pp alloc}
	       | LET {k_let,pat,bind,scope} => LET {k_let=k_let,pat=pat,bind=ips bind,scope=ips scope}
	       | FIX{free,shared_clos,functions,scope} =>
		let fun ips_f {lvar,occ,tyvars,rhos,epss,Type,rhos_formals,bound_but_never_written_into,other,bind} =
		      let val rhos_formals' = map bind_transform (!rhos_formals)
                        val bound_but_never_written_into' = case bound_but_never_written_into of
                          SOME l => SOME(map bind_transform l) | NONE => NONE
		      in {lvar=lvar,occ=occ,tyvars=tyvars,rhos=rhos,epss=epss,Type=Type,
			  rhos_formals=ref rhos_formals',
                          bound_but_never_written_into = bound_but_never_written_into',
                          other=other,bind=ips bind}
		      end 
		in FIX{free=free,shared_clos=pp shared_clos,functions=map ips_f functions,scope=ips scope}
		end
	       | APP(ck,sr,tr1,tr2) => APP(ck,sr,ips tr1, ips tr2)
	       | EXCEPTION(excon,b,mu,atp,tr) => EXCEPTION(excon,b,mu,pp atp,ips tr)
	       | RAISE tr => RAISE(ips tr)
	       | HANDLE(tr1,tr2) => HANDLE(ips tr1, ips tr2)
	       | SWITCH_I {switch,precision} => SWITCH_I {switch=ips_sw switch, precision=precision}
	       | SWITCH_W {switch,precision} => SWITCH_W {switch=ips_sw switch, precision=precision}
	       | SWITCH_S sw => SWITCH_S(ips_sw sw)
	       | SWITCH_C sw => SWITCH_C(ips_sw sw)
	       | SWITCH_E sw => SWITCH_E(ips_sw sw)
	       | LETREGION{B,rhos,body} => LETREGION{B=B,rhos=ref (map bind_transform (!rhos)),
						     body=ips body}
	       | CON0 {con, il, aux_regions, alloc} => CON0 {con=con, il=il, aux_regions=map pp_dummy aux_regions, alloc=pp alloc}
	       | CON1 ({con, il, alloc}, tr) => CON1 ({con=con, il=il, alloc=pp alloc}, ips tr)
	       | DECON ({con, il}, tr) => DECON ({con=con, il=il}, ips tr)
	       | EXCON (excon, opt) => EXCON(excon, case opt
						      of SOME (alloc,tr) => SOME (pp alloc, ips tr) 
						       | NONE => NONE)
	       | DEEXCON (excon,tr) => DEEXCON (excon,ips tr)
	       | RECORD (alloc, trs) => RECORD (pp alloc, map ips trs)
	       | SELECT (i, tr) => SELECT (i, ips tr)
	       | DEREF tr => DEREF (ips tr)
	       | REF (alloc,tr) => REF (pp alloc,ips tr)
	       | ASSIGN (alloc,tr1,tr2) => ASSIGN (pp alloc,ips tr1,ips tr2)
	       | DROP (tr) => DROP (ips tr)
	       | EQUAL ({mu_of_arg1, mu_of_arg2, alloc}, tr1,tr2) =>
		EQUAL ({mu_of_arg1=mu_of_arg1, mu_of_arg2=mu_of_arg2, alloc=pp alloc}, ips tr1, ips tr2)
	       | CCALL ({name, mu_result, rhos_for_result}, trs) => 
		   let val p_point = pp_c()
		   in CCALL ({name = name,
			      mu_result = mu_result,
			      rhos_for_result =
			        map (fn (atp, i_opt) =>
				     (gen_pp (fn () => p_point) atp, i_opt))
				  rhos_for_result},
		             map ips trs)
		   end
	       | EXPORT (i,tr) => EXPORT (i,ips tr)
	       | RESET_REGIONS ({force, alloc,regions_for_resetting}, tr) => 
                     RESET_REGIONS ({force=force, alloc=pp alloc,
                                     regions_for_resetting = map pp regions_for_resetting}, ips tr)
	       | FRAME a => FRAME a

    in TR(e',mt,ateffs,mulef)
    end


    (* --------------------------------------------------------
     * Reset buckets for inserting free variables, graph and 
     * the internal environment.
     * -------------------------------------------------------- *)

    fun reset() = (reset_fvs(); reset_graph(); reset_psi_env())


    (* --------------------------------------------------------------
     * Main function; the env maps fix-bound lvars to minimal physical
     * sizes of actual region variables.
     * -------------------------------------------------------------- *)

    fun psi (pp_counter : unit -> pp, env : env, 
	     PGM{expression=tr,
		 export_datbinds,
		 import_vars,
		 export_vars,
		 export_basis,
		 export_Psi} : (place at, place*mul,unit)LambdaPgm)  
      : ((place*pp)at,place*phsize,unit)LambdaPgm * env =
      let
	val _ = reset()   (* reset free_vars-buckets, graph 
			   * and the internal environment *)
	
	val _ = insert_free_vars (tr, import_vars, export_vars)     (* Insert free variables *)
	val _ = psi_tr env tr                                       (* Build graph *)
	val _ = eval_psi_graph()                                    (* Evaluate graph *)
	val tr1 = ips pp_counter tr                                 (* Transform trip *)
	  
	val env1 = convert_env (!frame_env) (* Compute resulting environment mapping
					     * exported lvars into minimal physical 
					     * sizes of actual region variables. *)
	val _ = reset()
	  
      in (PGM{expression=tr1,
	      export_datbinds=export_datbinds,
	      import_vars=import_vars,
	      export_vars=export_vars,
	      export_basis=export_basis,
	      export_Psi=export_Psi}, env1)
      end


    (**************************)
    (* application conversion *)
    (**************************)

  fun allocates_space(place,INF) = true
    | allocates_space(place,WORDS i) = (i > 0) 

  exception GetRho

  fun get_rho(AtInf.ATTOP(rho,_)) = rho
    | get_rho(AtInf.ATBOT(rho,_)) = rho
    | get_rho(AtInf.SAT(rho,_)) = rho
    | get_rho(AtInf.IGNORE)   = raise GetRho

  fun actual_regions_match_formal_regions([],[])  = true
    | actual_regions_match_formal_regions(l as ((formal_rho,mul)::forms), rho_act::acts): bool =
          (Effect.eq_effect(formal_rho, get_rho rho_act) 
           handle GetRho => false)
           andalso actual_regions_match_formal_regions (forms,acts)
    | actual_regions_match_formal_regions(_,[]) = true
    | actual_regions_match_formal_regions([], _ ) = false

  fun remove_from_bound([],act) = []
    | remove_from_bound((b as (rho,mul))::bs, a) = 
        if (Effect.eq_effect(rho, get_rho a) handle _ => false) then
           bs  
        else b :: remove_from_bound(bs, a)

  val appConvert = fn (prog)=>
      appConvert
        allocates_space        
        actual_regions_match_formal_regions
        remove_from_bound
        prog


    (* --------------------------------
     * Pretty Printing 
     * -------------------------------- *)

    fun layout_effectpp (effect, ~1) = E.layout_effect effect
      | layout_effectpp (effect, pp) =
      if !print_all_program_points then
	PP.HNODE{start="",finish="",childsep=PP.RIGHT " ",
		 children=[E.layout_effect effect, PP.LEAF ("pp"^Int.toString pp)]}
      else E.layout_effect effect
    
    fun layout_placeXphsize (place,phsize) =
	PP.HNODE{start="",finish="",childsep=PP.RIGHT ":",
		 children=[E.layout_effect place,layout_phsize phsize]}
    fun layout_unit () = NONE 
    val layout_trip = layoutLambdaTrip (AtInf.layout_at layout_effectpp) (AtInf.layout_at layout_effectpp) 
      (SOME o layout_placeXphsize) layout_unit

    fun layout_pgm(PGM{expression,...}) = layout_trip expression

    val pu_phsize = 
	let fun toInt INF = 0
	      | toInt (WORDS _) = 1
	    val fun_INF = Pickle.con0 INF
	    fun fun_WORDS _ = Pickle.con1 WORDS (fn WORDS a => a | _ => die "pu_phsize") Pickle.int
	in Pickle.dataGen("PhysSizeInf.phsize",toInt,[fun_INF, fun_WORDS])
	end

    val pu_range_env = 
	let fun toInt (FORMAL_REGVARS _) = 0
	      | toInt (FORMAL_SIZES _) = 1
	      | toInt NOTFIXBOUND = 2
	    fun fun_FORMAL_REGVARS _ =
		Pickle.con1 FORMAL_REGVARS (fn FORMAL_REGVARS a => a | _ => die "pu_range_env.FORMAL_REGVARS") 
		Effect.pu_effects
	    fun fun_FORMAL_SIZES _ =
		Pickle.con1 FORMAL_SIZES (fn FORMAL_SIZES a => a | _ => die "pu_range_env.FORMAL_SIZES") 
		(Pickle.listGen pu_phsize)		
	    val fun_NOTFIXBOUND = Pickle.con0 NOTFIXBOUND
	in Pickle.dataGen("PhysSizeInf.range_env",toInt,[fun_FORMAL_REGVARS, fun_FORMAL_SIZES, fun_NOTFIXBOUND])
	end

    val pu_env = LvarMap.pu Lvars.pu pu_range_env

(*
    fun layout_vars(lvars,excons,places) =
          let val t1 = PP.HNODE{start = "lvars:", finish = "end of lvars;", 
                               childsep = PP.RIGHT " ", children = map (PP.LEAF o Lvars.pr_lvar) lvars}
            val t2 =  PP.HNODE{start = "excons:", finish = "end of excons;", 
                               childsep = PP.RIGHT " ", children = map (PP.LEAF o Excon.pr_excon) excons}
            val t3 =  PP.HNODE{start = "region variables:", finish = "end of region variables;", 
                               childsep = PP.RIGHT " ", children = map Effect.layout_effect places}
          in
            PP.NODE{start = "variables begin", finish = "variables end", indent = 2, 
                    childsep = PP.NOSEP, children = [t1,t2,t3]}
          end

    fun layout_pgm (PGM{expression,import_vars,export_vars,...}) = (* belongs in MulExp, actually *)
       let 
         val t1 = PP.NODE{start = "import_vars: ", finish = "end import vars", 
                          childsep = PP.RIGHT " ",
                          indent = 2, children = 
                            case import_vars of
                              ref(SOME iv)=> [layout_vars iv]
                            | _ => [PP.LEAF " (reference not set) "]}
         val t2 = PP.NODE{start = "export_vars: ", finish = "end export vars", 
                          childsep = PP.RIGHT " ",
                          indent = 2, children = [layout_vars export_vars]}
         val t3 = layout_trip expression
       in
         PP.NODE{start = "Physical Size Program (a MulExp)", 
                 finish = "end of Physical Size Program",
                 indent = 2, childsep = PP.NOSEP,
                 children = [t1,t2,t3]}
       end
ME 1998-09-07*)

  end
