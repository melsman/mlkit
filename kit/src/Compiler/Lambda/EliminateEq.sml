
functor EliminateEq (structure Name : NAME
		     structure Lvars : LVARS
		       sharing type Lvars.name = Name.name
		     structure Con : CON
		     structure TyName : TYNAME
		       sharing type TyName.name = Name.name
		     structure LambdaExp : LAMBDA_EXP
		       sharing type LambdaExp.lvar = Lvars.lvar
		       sharing type LambdaExp.con = Con.con
		       sharing type LambdaExp.TyName = TyName.TyName
		     structure Crash : CRASH
		     structure Flags : FLAGS
		     structure PP : PRETTYPRINT
		       sharing type PP.StringTree = TyName.Map.StringTree
			                          = Lvars.Map.StringTree
		     structure TyVarMap : MONO_FINMAP
		       sharing type TyVarMap.dom = LambdaExp.tyvar
	   	       sharing type TyVarMap.StringTree = PP.StringTree) 
  : ELIMINATE_EQ =
  struct

    structure List = Edlib.List

    structure TyNameMap = TyName.Map
    structure LvarMap = Lvars.Map

    open LambdaExp

    fun die s = Crash.impossible ("EliminateEq." ^ s)
    fun log s = (TextIO.output(!Flags.log, s); TextIO.flushOut (!Flags.log))

    val quotation = Flags.is_on0 "quotation"

    (* ------------------------------------------------------------
     * The Environment
     * ------------------------------------------------------------ *)

    datatype result = MONOLVAR of lvar * tyvar list | POLYLVAR of lvar | FAIL of string

    local 
      type tynamemap = result TyNameMap.map
      type tyvarmap = lvar TyVarMap.map
      type lvarmap = (tyvar list) LvarMap.map
      type env = tynamemap * tyvarmap * lvarmap

      val empty : env = (TyNameMap.empty, TyVarMap.empty, LvarMap.empty)
      val initial : env = empty
	
      fun plus ((tynamemap1, tyvarmap1, lvarmap1),(tynamemap2, tyvarmap2, lvarmap2)) =
	(TyNameMap.plus (tynamemap1,tynamemap2), 
	 TyVarMap.plus (tyvarmap1, tyvarmap2),
	 LvarMap.plus (lvarmap1, lvarmap2))

      fun add_tyname (tyname, result, (tynamemap, tyvarmap, lvarmap)) =
	(TyNameMap.add (tyname, result, tynamemap), tyvarmap, lvarmap)

      fun lookup_tyname (tynamemap, _, _) = TyNameMap.lookup tynamemap

      fun add_tyvar (tyvar, lv, (tynamemap, tyvarmap, lvarmap)) =
	(tynamemap, TyVarMap.add (tyvar, lv, tyvarmap), lvarmap)

      fun lookup_tyvar (_, tyvarmap, _) = TyVarMap.lookup tyvarmap

      fun add_lvar (lv, tvs, (tynamemap, tyvarmap, lvarmap)) =
	(tynamemap, tyvarmap, LvarMap.add (lv, tvs, lvarmap))

      fun lookup_lvar (_, _, lvarmap) = LvarMap.lookup lvarmap 

      (* only used at top-level -- when tyvarmap is empty *)
      fun env_range (tynamemap, _, _) = TyNameMap.range tynamemap
      fun env_map (f:result->result) (tynamemap,tyvarmap,lvarmap) =
	(TyNameMap.composemap f tynamemap, tyvarmap,lvarmap)

      fun restrict'((tnmap,_,lvmap),{lvars,tynames}) =
	let val tnmap' = List.foldL (fn tn => fn acc =>
				     case TyNameMap.lookup tnmap tn  (* do not scream if it *)
				       of SOME res => TyNameMap.add(tn,res,acc)     (* is not here... *)
					| NONE => acc) TyNameMap.empty tynames
	    val lvars' = TyNameMap.Fold (fn ((_,POLYLVAR lv), lvars) => lv :: lvars
	                                 | ((_,FAIL s), lvars) => lvars 
	                                 | _ => die "restrict.not POLYLVAR") [] tnmap'
	    val lvmap' = List.foldL (fn lv => fn acc =>
				     case LvarMap.lookup lvmap lv
				       of SOME res => LvarMap.add(lv,res,acc)
					| NONE => die ("restrict.lv: " ^ Lvars.pr_lvar lv ^ " not in map"))
	                 LvarMap.empty lvars
	in (lvars', (tnmap',TyVarMap.empty,lvmap'))
	end  

      fun eq_tnres (POLYLVAR lv1,POLYLVAR lv2) = Lvars.eq(lv1,lv2)
	| eq_tnres (MONOLVAR _,_) = die "eq_tnres.MONOLVAR"
	| eq_tnres (_,MONOLVAR _) = die "eq_tnres.MONOLVAR"
	| eq_tnres _ = false

      fun eq_lvres(tvs1,tvs2) = (map equality_tyvar tvs1 = map equality_tyvar tvs2)

      fun enrich_tnmap(tnmap1,tnmap2) =
	TyNameMap.Fold (fn ((tn2, res2), b) => b andalso
			   case TyNameMap.lookup tnmap1 tn2
			     of SOME res1 => eq_tnres(res1,res2)
			      | NONE => false) true tnmap2

      fun enrich_lvmap(lvmap1,lvmap2) =
	LvarMap.Fold (fn ((lv2, res2), b) => b andalso
			 case LvarMap.lookup lvmap1 lv2
			   of SOME res1 => eq_lvres(res1,res2)
			    | NONE => false) true lvmap2

      fun enrich((tnmap1,_,lvmap1),(tnmap2,_,lvmap2)) =
	enrich_tnmap(tnmap1,tnmap2) andalso enrich_lvmap(lvmap1,lvmap2)

      fun match_tnmap(tnmap,tnmap0) = 
	let val tnmap = TyNameMap.fromList (TyNameMap.list tnmap)
	in TyNameMap.Fold (fn ((tn, POLYLVAR lv),()) =>
			   (case TyNameMap.lookup tnmap0 tn
			      of SOME(POLYLVAR lv0) => Lvars.match(lv,lv0)
			       | _ => ())
                           | _ => die "match_tnmap") () tnmap;
	  tnmap
	end

      fun match((tnmap,tvmap,lvmap),(tnmap0,_,tlvmap0)) =
	(match_tnmap(tnmap,tnmap0),
	 tvmap,
	 lvmap)

      local 
	fun layout_lvar lv = PP.LEAF (Lvars.pr_lvar lv)
	fun layout_result (FAIL str) = PP.LEAF str
	  | layout_result (MONOLVAR (lv,_)) = PP.LEAF ("MONO " ^ Lvars.pr_lvar lv)
	  | layout_result (POLYLVAR lv) = PP.LEAF ("POLY " ^ Lvars.pr_lvar lv)
	fun layout_tyname tn = PP.LEAF (TyName.pr_TyName tn)
	fun layout_tyvar tv = PP.LEAF (LambdaExp.pr_tyvar tv)
	fun layout_tyvars tvs = PP.NODE {start="{", finish="}",indent=0,childsep=PP.RIGHT ",",
					 children=map layout_tyvar tvs}
	fun layout_int i = PP.LEAF(Int.toString i)
	val layout_tynamemap = 
	  TyNameMap.layoutMap {start="TyNameEnv = {", eq=" -> ", sep=", ", finish="}"}
	  layout_tyname layout_result
	val layout_tyvarmap = 
	  TyVarMap.layoutMap {start="TyVarEnv = {", eq=" -> ", sep=", ", finish="}"}
	  layout_tyvar layout_lvar
	val layout_lvarmap =
	  LvarMap.layoutMap {start="LvarEnv = {", eq=" -> ", sep=", ", finish="}"}
	  layout_lvar layout_tyvars
      in
	fun layout_env (tynamemap, tyvarmap, lvarmap) =
	  PP.NODE {start="EqElimEnv = [", finish="]", indent=2, childsep=PP.RIGHT ", ", 
		   children=[layout_tynamemap tynamemap, layout_tyvarmap tyvarmap,
			     layout_lvarmap lvarmap]}
      end
      fun say s = TextIO.output(TextIO.stdOut, s)  
    in
      type env = env
      val empty : env = empty
      val initial : env = initial
      val plus : env * env -> env = plus
      val add_tyname : TyName * result * env -> env = add_tyname
      val lookup_tyname : env -> TyName -> result option = lookup_tyname
      val add_tyvar : tyvar * lvar * env -> env = add_tyvar
      val lookup_tyvar : env -> tyvar -> lvar option = lookup_tyvar
      val add_lvar : lvar * tyvar list * env -> env = add_lvar
      val lookup_lvar : env -> lvar -> tyvar list option = lookup_lvar
      val env_range : env -> result list = env_range (* only used at top-level *)
      val env_map : (result->result) -> env -> env = env_map (* only used at top-level *)
      val enrich : env * env -> bool = enrich
      val match : env * env -> env = match
      fun restrict(e: env, {lvars:lvar list,tynames:TyName list}): lvar list * env = restrict'(e,{lvars=lvars,tynames=tynames})
handle x => 
               (say "ElimiateEq.restrict failed\n";
                say "The equality environment is:\n";
                PP.outputTree(say,  layout_env e, 70);
                say "(end of equality environment)\n";
                raise x) 
      type StringTree = PP.StringTree
      val layout_env : env -> StringTree = layout_env
    end


    (* ------------------------------------------------------------
     * Some usefull stuff
     * ------------------------------------------------------------ *)

    fun mk_eq_tau tv = let val tau = TYVARtype tv
		       in ARROWtype([RECORDtype [tau,tau]],[boolType])
		       end
    fun mk_eq_abs [] [] e = e
      | mk_eq_abs (lv::lvs) (tau::taus) e = FN {pat = [(lv,tau)], body=mk_eq_abs lvs taus e}
      | mk_eq_abs _ _ _ = die "mk_eq_abs"

    fun mk_env_tyvars [] [] env = env
      | mk_env_tyvars (tv::tvs) (lv::lvs) env = mk_env_tyvars tvs lvs (add_tyvar (tv,lv,env))
      | mk_env_tyvars _ _ _ = die "mk_env_tyvars"

    val lamb_true = PRIM(CONprim {con = Con.con_TRUE, instances = []}, [])
    val lamb_false = PRIM(CONprim {con = Con.con_FALSE, instances = []}, [])
    fun lamb_var lv = VAR{lvar=lv,instances=[]}

    fun monolet {lvar, Type, bind, scope} = 
      LET {pat=[(lvar,[],Type)], bind=bind, scope=scope}

    (*`is_eq_prim_tn tyname' iff EQUALprim is defined on that tyname (in
     which case the equality check code is generated by the backend;
     otherwise, the equality check code must be generated here).*)

    fun is_eq_prim_tn tn = 
      List.exists (fn tn' => TyName.eq(tn',tn))
      [TyName.tyName_INT31, TyName.tyName_INT32,
       TyName.tyName_WORD8, TyName.tyName_WORD31, TyName.tyName_WORD32, 
       TyName.tyName_BOOL, TyName.tyName_STRING, TyName.tyName_REF, TyName.tyName_ARRAY,
       TyName.tyName_CHARARRAY] (*not tyName_REAL*)



    (* ---------------------------------------------------------------
     * Generate a lambda expression for checking equality of a pair
     * of two values of a given type. 
     * --------------------------------------------------------------- *)

    exception DONT_SUPPORT_EQ of string (* When generating functions for checking
					 * equality of datatypes, this exception
					 * may be raised. *)
    fun mk_prim_eq tau =
      let val p = Lvars.newLvar()
      in FN {pat = [(p, RECORDtype [tau, tau])],
	     body = PRIM(EQUALprim {instance = RECORDtype[tau, tau]},
			 [PRIM(SELECTprim 0, [lamb_var p]),
			  PRIM(SELECTprim 1, [lamb_var p])])}
      end

    fun gen_type_eq env tau = (* may raise DONT_SUPPORT_EQ *)
      let 
	fun gen tau =
	 (case tau
	    of (TYVARtype tv) => (case lookup_tyvar env tv
				    of SOME lv => VAR {lvar = lv, instances =[]}
				     | NONE => FN {pat=[(Lvars.newLvar(),RECORDtype [TYVARtype tv, TYVARtype tv])],
						   body=lamb_false}) (* the function will never be applied. *)
				    (* --------------  
				     * old; check out testprogs/eq_1.sml
				     * if equality_tyvar tv then
				     *   die "gen_type_eq. equality type variable not in env."
				     * else die "gen_type_eq. ordinary type variable not in env.")
				     ---------------- *)
	   | (ARROWtype _) => die "gen_type_eq.arrow type."
	   | (CONStype (taus,tn)) =>
	      let fun apply e [] = e
		    | apply e (tau::taus) = apply (APP(e, gen tau)) taus
		fun dont_support() = 
		  raise DONT_SUPPORT_EQ (TyName.pr_TyName tn)
	      in
		if is_eq_prim_tn tn then mk_prim_eq tau
		else case lookup_tyname env tn
		       of SOME (POLYLVAR lv) =>
			 apply (VAR {lvar = lv, instances = taus}) taus
			| SOME (MONOLVAR (lv, tyvars)) =>
			    if map (fn TYVARtype tv => tv
		                     | _ => dont_support()) taus = tyvars then
			      apply (lamb_var lv) taus
			    else dont_support()
			| SOME (FAIL str) => (* die ("gen_type_eq -- Equality not supported for " ^ 
						  TyName.pr_TyName tn ^ ".") ME 1998-11-17 *)
			      raise DONT_SUPPORT_EQ str
			| NONE => die ("gen_type_eq. type name " ^ TyName.pr_TyName tn ^ 
				       " not in env.")
	      end
	   | (tau as RECORDtype taus) =>
		 let val p = Lvars.newLvar()
		 in FN {pat = [(p, RECORDtype [tau,tau])],
			body = gen_switch 0 p taus}
		 end)

        and gen_switch n p =
	 (fn [] => lamb_true
	   | [tau] => let val v = VAR {lvar=p, instances=[]}
			  fun sel k = PRIM(SELECTprim n, [PRIM(SELECTprim k, [v])])
		      in APP (gen tau, PRIM(RECORDprim, [sel 0, sel 1]))
		      end
	   | (tau :: taus) => let val v = VAR {lvar=p, instances=[]}
				  fun sel k = PRIM(SELECTprim n, [PRIM(SELECTprim k, [v])])
				  val e = APP (gen tau, PRIM(RECORDprim, [sel 0, sel 1]))
			      in SWITCH_C (SWITCH (e, [((Con.con_TRUE,NONE), gen_switch (n+1) p taus)], 
						   SOME lamb_false))
			      end)
      in gen tau
      end

    
    (* --------------------------------------------------------------
     * Generate equality functions for datatype bindings that
     * respects equality (and that are not polymorphically recursive).
     *
     * For now, we only allow values of some datatypes to be checked
     * for equality. We only allow equality for datatypes that are not
     * polymorphically recursive. Even simple cases like
     *
     *   datatype 'a t = A of 'a s | B
     *        and 'b s = C of 'b t
     *
     * are not supported. Some cases (as the above) can be supported, 
     * however.
     * -------------------------------------------------------------- *)


    (* Generate a fix abstraction for a single datatype binding *)
    fun gen_db env (tyvars,tn,cbs) = (* may raise DONT_SUPPORT_EQ *)
      let 
	fun mk_tau tau = let val tau_arg = RECORDtype [tau, tau]
			 in ARROWtype([tau_arg],[boolType])
			 end
	val tau_tn = CONStype (map TYVARtype tyvars, tn)
	fun gen_tau [] = mk_tau tau_tn
	  | gen_tau (tv :: tvs) = ARROWtype([mk_tau (TYVARtype tv)], [gen_tau tvs])

	val (p,p0,p1) = (Lvars.newLvar(), Lvars.newLvar(), Lvars.newLvar())
	val lvs = map (fn _ => Lvars.newLvar()) tyvars

	val env' = let fun extend_env [] [] env = env
			 | extend_env (tv::tvs) (lv::lvs) env = 
	                      extend_env tvs lvs (add_tyvar (tv,lv,env))
			 | extend_env _ _ _ = die "extend_env"
		   in extend_env tyvars lvs env
		   end

	fun mk_abs_eq_fns [] [] e = FN {pat = [(p, RECORDtype [tau_tn,tau_tn])], body=e}
	  | mk_abs_eq_fns (tv::tvs) (lv::lvs) e =  FN {pat = [(lv, mk_tau (TYVARtype tv))], 
						       body = mk_abs_eq_fns tvs lvs e} 
	  | mk_abs_eq_fns _ _ _ = die "mk_abs_eq_fns"
	  
	fun mk_pn pn n e = (* p is free *)
	  monolet {lvar=pn, Type=tau_tn, 
		   bind=PRIM(SELECTprim n, [lamb_var p]),
		   scope=e} 

	fun mk_sw c e = SWITCH_C(SWITCH(lamb_var p1,[(c, e)], SOME lamb_false))
	fun mk_nullary_sw c = mk_sw (c,NONE) lamb_true 
	fun mk_unary_sw c tau p0' =
	  let 
	    val p1' = Lvars.newLvar()
	    fun mk_decon p' p e = 
	      monolet {lvar=p', Type=tau, 
		       bind=PRIM(DECONprim {con=c, instances=map TYVARtype tyvars,lv_opt=SOME p'}, [lamb_var p]),
		       scope=e}
	    val lamb_eq_fn_tau = gen_type_eq env' tau
	    val lamb_true_case =
	      mk_decon p0' p0
	      (mk_decon p1' p1
	       (APP(lamb_eq_fn_tau, PRIM(RECORDprim, [lamb_var p0', lamb_var p1']))))

	  in mk_sw (c,SOME p1') lamb_true_case
	  end

	fun mk_cases [] = []
	  | mk_cases ((c, typeopt)::rest) =
	  let val p = case typeopt
			of NONE => ((c,NONE), mk_nullary_sw c)
			 | SOME tau => 
			    let val lv = Lvars.newLvar()
			    in ((c,SOME lv), mk_unary_sw c tau lv) 
			    end
	  in p :: mk_cases rest
	  end
	val big_sw = SWITCH_C(SWITCH(lamb_var p0, mk_cases cbs, NONE))
	val bind  = mk_abs_eq_fns tyvars lvs
	             (mk_pn p0 0
		      (mk_pn p1 1 big_sw))
	val lvar = case lookup_tyname env' tn
		     of SOME(MONOLVAR (lv,_)) => lv
		      | _ => die "gen_db.lvar" 
      in			
	{lvar=lvar, tyvars=tyvars, Type=gen_tau tyvars, bind=bind}
      end



    (* Generate a FIX and a ``polymorphic'' environment for a list 
     * of mutually recursive datatype bindings. *)
    fun gen_datatype_eq_dbs env0 dbs = (* may raise DONT_SUPPORT_EQ *)
      let 
	(* First create a monomorphic environment for tynames in dbs. 
	 * If a tyname does not admit equality then don't introduce it. *)
	fun mono_env [] env = env
	  | mono_env ((tyvars,tn,_)::dbs') env =
	  let val env' = if TyName.equality tn then 
	                    let val lvar = Lvars.new_named_lvar ("eq_" ^ TyName.pr_TyName tn)
			    in add_tyname (tn, MONOLVAR (lvar, tyvars), env)
			    end
			 else env
	  in mono_env dbs' env'
	  end
	    
	val env = mono_env dbs empty
	val env' = plus (env0, env)    (* the environment in which to generate functions *) 

	fun gen_dbs [] = []
	  | gen_dbs ((db as (_,tn,_)) :: dbs) = 
	  if TyName.equality tn then let val function = gen_db env' db
				     in function :: gen_dbs dbs
				     end
	  else gen_dbs dbs

	val functions = gen_dbs dbs (* force evaluation of `gen_dbs' to make sure 
				     * DONT_SUPPORT_EQ is raised if needed. *)
	fun fix dbs e = FIX {functions=functions, scope=e}

	(* Now, - make the monomorphic environment polymorphic *)
	fun poly_env env = env_map (fn MONOLVAR (lv,_) => POLYLVAR lv
                                     | _ => die "poly_env") env
      in (fix dbs, poly_env env)
      end


   (* Generate an expression containing a sequence of nested FIX 
    * expressions, one for each list of mutually recursive datatype 
    * bindings in dbss. *)
    fun gen_datatype_eq env0 dbss =
      case dbss
	of [] => (fn lexp => lexp, empty)
	 | (dbs :: dbss') =>
	  let 
	    fun fail_env [] env s = env  (* do not support equality if exn is raised *)
	      | fail_env ((_,tn,_)::dbs) env s = 
	      let val env' = if TyName.equality tn then add_tyname (tn, FAIL s, env)
			     else env
	      in fail_env dbs env' s
	      end
	    val (f, env) = ((gen_datatype_eq_dbs env0 dbs)
			    handle DONT_SUPPORT_EQ s => (fn lexp => lexp, fail_env dbs empty s))
	    val (f', env') = gen_datatype_eq (plus(env0,env)) dbss'
	  in 
	    (f o f', plus(env, env'))
	  end

    (* ------------------------------------------------------------
     * Environment and code for built-in datatypes.
     * Only list & table is considered. The other type names are considered
     * basic type names and are treated directly in gen_type_eq.
     * ------------------------------------------------------------ *)

    fun gen_datatype_builtin_eq () =
      let 
	val tn_list = TyName.tyName_LIST
	val tv = fresh_tyvar()
	val tau_tv = TYVARtype tv
	val cbs = [(Con.con_CONS, SOME (RECORDtype [tau_tv, CONStype([tau_tv], tn_list)])),
		   (Con.con_NIL, NONE)]
	val dbss = [[([tv], tn_list,cbs)]]
	val (f, e) = gen_datatype_eq empty dbss
	val (f', e') = gen_datatype_for_table ()
        val (f'', e'') = gen_datatype_for_quotation ()
      in 
	(f o f' o f'', plus(plus(e,e'),e''))
      end
    and gen_datatype_for_quotation () =
      if quotation() then
        let
          val tv = fresh_tyvar()
          val tau_tv = TYVARtype tv
          val cbs = [(Con.con_QUOTE, SOME (CONStype([], TyName.tyName_STRING))),
                     (Con.con_ANTIQUOTE, SOME tau_tv)]
          val dbss = [[([tv], TyName.tyName_FRAG, cbs)]]
        in gen_datatype_eq empty dbss
        end
      else (fn e => e, empty)
    and gen_datatype_for_table () =
 
(*the word_table equality function written in sml (7 lines):
  
    fun eq_table eq_'a (table1, table2) =
	   let val n1 = prim ("table_size", table1)
	       val n2 = prim ("table_size", table2)
	       fun loop j = j < 0 orelse eq_'a (word_sub0 (table1, j), word_sub0 (table2, j))
					 andalso loop (j - 1)
	   in n1 = n2 andalso loop (n1 - 1)
	   end

  The same written in LambdaExp (15 lines):

    FIX eq_table = FN eq_'a . FN table_pair .
    LET table1 = #0 table_pair IN
    LET table2 = #1 table_pair IN
    LET n1 = PRIM (CCALLprim "table_size", [table1]) IN
    LET n2 = PRIM (CCALLprim "table_size", [table2]) IN
    FIX loop = FN j:INT .
      SWITCH_C (PRIM (LESS_INTprim, [j, 0])) OF true => true
      | _ => SWITCH_C (APP (eq_'a, (RECORDprim, [PRIM (CCALLprim "word_sub0", [table1, j]),
	                                         PRIM (CCALLprim "word_sub0", [table2, j])]))) OF
		true => APP (loop, (PRIM (MINUS_INTprim, [j, 1])))
		| _ => false
    IN
      SWITCH_C (PRIM (EQUALprim INT*INT, [n1, n2])) OF false => false
      | _ => APP (loop, PRIM (MINUS_INTprim, [n1, 1]))
    IN scope

  (How many lines do you think are in the e n c o d i n g of this
  LambdaExp?  Good luck reading it.)*)

 let
   val tyname = TyName.tyName_VECTOR
   val s = TyName.pr_TyName tyname
   val alpha = fresh_tyvar {}
   val tau_alpha = TYVARtype alpha
   val tau_tyname = CONStype ([tau_alpha], tyname)
   val lvar_eq_table = Lvars.new_named_lvar ("eq_" ^ s)
   val lvar_eq_alpha = Lvars.new_named_lvar "eq_'a"
   val lvar_table_pair = Lvars.new_named_lvar (s ^ "_pair")
   val lvar_table1 = Lvars.new_named_lvar (s ^ "1")
   val lvar_table2 = Lvars.new_named_lvar (s ^ "2")
   val lvar_n1 = Lvars.new_named_lvar "n1"
   val lvar_n2 = Lvars.new_named_lvar "n2"
   val lvar_loop = Lvars.new_named_lvar "loop"
   val lvar_j = Lvars.new_named_lvar "j"
   val var_eq_alpha = lamb_var lvar_eq_alpha
   val var_table_pair = lamb_var lvar_table_pair
   val var_table1 = lamb_var lvar_table1
   val var_table2 = lamb_var lvar_table2
   val var_n1 = lamb_var lvar_n1
   val var_n2 = lamb_var lvar_n2
   val var_loop = lamb_var lvar_loop
   val var_j = lamb_var lvar_j

   fun tau_for_eq_fun tau = ARROWtype ([RECORDtype [tau, tau]], [boolType])

   fun let_nX_equal_table_size_in_bytes lvar_nX var_tableX scope =
	 let val alpha' = fresh_tyvar ()
	 in monolet {lvar = lvar_nX, Type = intDefaultType(), bind =
		     PRIM (CCALLprim {name = "table_size",
				      (*alpha' is instantiated to alpha (from above):*)
				      tyvars = [alpha'], instances = [TYVARtype alpha],
				      Type = ARROWtype ([CONStype ([TYVARtype alpha'], tyname)], 
							[intDefaultType()])},
		           [var_tableX]),
		     scope = scope}
	 end

   fun sub var_tableX =
	 let val alpha' = fresh_tyvar {}
	     val tau_alpha' = TYVARtype alpha'
	 in PRIM (CCALLprim {name = "word_sub0",
			     (*alpha' is instantiated to alpha (from above):*)
			     tyvars = [alpha'], instances = [TYVARtype alpha],
			     Type = ARROWtype ([CONStype ([tau_alpha'], tyname), intDefaultType()],
					       [tau_alpha'])},
	          [var_tableX, var_j])
	 end

   fun INTEGER' i = INTEGER(Int32.fromInt i, intDefaultType())

   val tag_values = Flags.is_on0 "tag_values"

   fun ccall name argtypes restype =
     CCALLprim {name = name, instances = [], tyvars = [],
		Type = ARROWtype (argtypes, [restype])}

   fun MINUS_INTprim() = 
     let val n = if tag_values() then "__minus_int31" else "__minus_int32ub"
       val t = intDefaultType()
     in ccall n [t, t] t
     end

   fun LESS_INTprim() = 
     let val n = if tag_values() then "__less_int31" else "__less_int32ub"
       val t = intDefaultType()
     in ccall n [t,t] boolType
     end

   fun bind_loop() =
     FN {pat = [(lvar_j, intDefaultType())], body =
	 SWITCH_C (SWITCH (PRIM (LESS_INTprim(), [var_j, INTEGER' 0]),
	   [((Con.con_TRUE,NONE), lamb_true)],
	   SOME (SWITCH_C (SWITCH
		  ((APP (var_eq_alpha, PRIM (RECORDprim, [sub var_table1, sub var_table2]))),
		   [((Con.con_TRUE,NONE), APP (var_loop, PRIM (MINUS_INTprim(), [var_j, INTEGER' 1])))],
		   SOME lamb_false)))))}

   fun function_loop() = {lvar = lvar_loop,
			  tyvars = [], Type = ARROWtype ([intDefaultType()], [boolType]),
			  bind = bind_loop()}
       
   fun bind_eq_table() =
     FN {pat = [(lvar_eq_alpha, tau_for_eq_fun tau_alpha)], body =
	 FN {pat = [(lvar_table_pair, RECORDtype [tau_tyname, tau_tyname])], body =
	     monolet {lvar = lvar_table1, Type = tau_tyname, bind =
		      PRIM (SELECTprim 0, [var_table_pair]), scope =
		      monolet {lvar = lvar_table2, Type = tau_tyname, bind =
			       PRIM (SELECTprim 1, [var_table_pair]), scope =
			       let_nX_equal_table_size_in_bytes lvar_n1 var_table1
			       (let_nX_equal_table_size_in_bytes lvar_n2 var_table2
				(FIX {functions = [function_loop()], scope =
				      SWITCH_C (SWITCH
					(PRIM (EQUALprim {instance = RECORDtype [intDefaultType(), 
										 intDefaultType()]},
					       [var_n1, var_n2]),
					 [((Con.con_TRUE,NONE),
					   APP (var_loop, PRIM (MINUS_INTprim(), [var_n2, INTEGER' 1])))],
					 SOME lamb_false))}))}}}}
     
   fun function_eq_table() = {lvar = lvar_eq_table,
			      tyvars = [alpha],
			      Type = ARROWtype ([tau_for_eq_fun tau_alpha],
						[tau_for_eq_fun tau_tyname]),
			      bind = bind_eq_table()}

   val f = fn scope => FIX {functions = [function_eq_table()], scope = scope}
   val env = add_tyname (tyname, POLYLVAR lvar_eq_table, empty)
 in
   (f, env)
 end



    (* ------------------------------------------------------------
     * Translation
     * ------------------------------------------------------------ *)

    fun extend_frame env lexp : LambdaExp =
      let
	fun get_lvars [] lvars = lvars
	  | get_lvars (FAIL _ :: rest) lvars = get_lvars rest lvars
	  | get_lvars (POLYLVAR lv :: rest) lvars = get_lvars rest (lv::lvars)
	  | get_lvars _ _ = die "extend_frame.get_lvars"

	val lvars = get_lvars (env_range env) []

	(* Assume only one frame.. *)
	fun f e =
	  let fun f_sw (SWITCH(e,sel,opt)) = 
	    SWITCH(f e,map (fn (a,e) => (a, f e)) sel,
		   case opt of SOME e => SOME (f e) | NONE => NONE)
	  in case e
	       of VAR _ => e
		| INTEGER _ => e
		| WORD _ => e
		| STRING _ => e
		| REAL _ => e
		| FN{pat,body} => FN{pat=pat,body=f body}
		| LET {pat, bind, scope} => LET {pat=pat, bind=f bind,scope=f scope}
		| FIX {functions, scope} => 
		 FIX {functions=map (fn {lvar,tyvars,Type,bind} => {lvar=lvar,tyvars=tyvars,Type=Type,bind=f bind}) functions, 
		      scope=f scope}
		| APP(e1,e2) => APP(f e1, f e2)
		| EXCEPTION (excon,tauopt,scope) => EXCEPTION (excon, tauopt,f scope)
		| RAISE(e,tau) => RAISE(f e, tau)
		| HANDLE(e1,e2) => HANDLE(f e1, f e2)
		| SWITCH_I {switch,precision} => SWITCH_I {switch=f_sw switch, precision=precision} 
		| SWITCH_W {switch,precision} => SWITCH_W {switch=f_sw switch, precision=precision} 
		| SWITCH_S sw => SWITCH_S(f_sw sw) 
		| SWITCH_C sw => SWITCH_C(f_sw sw) 
		| SWITCH_E sw => SWITCH_E(f_sw sw) 
		| PRIM(p, es) => PRIM(p, map f es)
		| FRAME {declared_lvars,declared_excons} =>  (* frame is in global scope *) 
		 let val new_declared_lvars = 
		   map (fn lv => let val tv = fresh_tyvar()
				 in {lvar=lv,tyvars=[tv],Type=TYVARtype tv}  (* dummy type scheme *)
				 end) lvars
		 in FRAME {declared_lvars = declared_lvars @ new_declared_lvars,
			   declared_excons = declared_excons}
		 end
	  end
      in 
	f lexp
      end


    (* To deal with fix and the fact that for VAR we do not know what
     * instances are instances of equality tyvars we extend the scheme
     * as follows :
     *
     * 1. The translation environment is extended to map lvars to
     *    lists of tyvars (all tyvars).
     * 2. In the case for   let x:\/e1...en.a1...am = e in e'   we 
     *    bind x to e1...en.a1..am in the environment for e'.
     * 3. In the case for fix we do as above, but we also extend the
     *    environment for the bindings similarly.
     * 4. For the VAR case, when the instance list is empty, and the
     *    lvar is mapped to e1..en.a1..am in env, then we are dealing 
     *    with a recursive call (unless n=m=0).
     *)

    fun apply_eq_fns env [] e = e
      | apply_eq_fns env (tau::taus) e = let val f = APP(e, gen_type_eq env tau)
					 in apply_eq_fns env taus f
					 end

    fun eq_tyvars [] = []
      | eq_tyvars (tv::tvs) = if equality_tyvar tv then tv :: eq_tyvars tvs
			      else eq_tyvars tvs


    (* For incremental compilation we export the environment
     * for lvars in the frame. We assign the env to env_frame 
     * in the FRAME-case below. *)
    val env_frame = ref empty

    fun t_switch t env (SWITCH(lexp, sels, opt)) =
      SWITCH(t env lexp, map (fn (a, lexp) => (a, t env lexp)) sels,
	     case opt 
	       of SOME lexp => SOME (t env lexp) 
		| NONE => NONE)

    fun t env lexp =
      case lexp
	of VAR {lvar, instances=[]} =>  (* maybe a recursive call *)
	  (case lookup_lvar env lvar
	     of SOME tyvars => apply_eq_fns env (map TYVARtype (eq_tyvars tyvars)) lexp
	      | NONE => lexp)
	 | VAR {lvar, instances} => (* not a recursive call *)  
	    (case lookup_lvar env lvar
	       of SOME tyvars => (* those of the tyvars that admit equality
				  * determines what instances to use. *)
		 let fun sel [] [] = []
		       | sel (tv::tvs) (tau::taus) = 
		           if equality_tyvar tv then tau :: sel tvs taus
			   else sel tvs taus
		       | sel [] _ = die ("t.VAR.sel [] _.  lvar=" ^ Lvars.pr_lvar lvar)
		       | sel _ [] = die ("t.VAR.sel _ [].  lvar=" ^ Lvars.pr_lvar lvar)
		     val instances' = sel tyvars instances
		 in
		   apply_eq_fns env instances' lexp
		 end
		| NONE => die "f.VAR.lvar not in env")
	 | PRIM(EQUALprim {instance}, [lexp1, lexp2]) =>
	       (case instance
		  of RECORDtype [tau,_] => 
		    let val e = PRIM(RECORDprim, [t env lexp1,t env lexp2])
		    in APP(gen_type_eq env tau, e)
		    end
		   | _ => die "f.EQUALprim")
	 | LET {pat=[(lvar,tvs,tau)],bind,scope} =>
	    let val eq_tvs = eq_tyvars tvs
	      val eq_lvs = map (fn _ => Lvars.newLvar()) eq_tvs
	      val eq_taus = map mk_eq_tau eq_tvs
	      fun mk_tau [] = tau
		| mk_tau (tau::taus) = ARROWtype([tau],[mk_tau taus])
	      val tau' = mk_tau eq_taus
	      val env_bind = mk_env_tyvars eq_tvs eq_lvs env
	      val env_scope = add_lvar (lvar,tvs,env)
	      val bind' = mk_eq_abs eq_lvs eq_taus (t env_bind bind)
	      val scope' = t env_scope scope
	    in
	      LET {pat=[(lvar,tvs,tau')], bind=bind', scope=scope'} 
	    end
	 | LET {pat=nil,bind,scope} =>
	    let val bind' = t env bind
	      val scope' = t env scope
	    in LET {pat=nil, bind=bind', scope=scope'} 
	    end
	 | LET _ => die "t.LET.not single" 

	 | FIX {functions,scope} =>
	    let fun env_fs [] env = env
		  | env_fs ({lvar,tyvars,Type,bind}::fs) env = env_fs fs (add_lvar (lvar,tyvars,env))
	      val env_scope = env_fs functions env
	      val env_bind_common = env_scope (* common for all binds in functions *)

	      fun f {lvar,tyvars,Type,bind} =
		let val eq_tvs = eq_tyvars tyvars
		  val eq_lvs = map (fn _ => Lvars.newLvar()) eq_tvs
		  val eq_taus = map mk_eq_tau eq_tvs
		  fun mk_tau [] = Type
		    | mk_tau (tau::taus) = ARROWtype([tau],[mk_tau taus])
		  val Type' = mk_tau eq_taus
		  val env_bind = mk_env_tyvars eq_tvs eq_lvs env_bind_common
		  val bind' = mk_eq_abs eq_lvs eq_taus (t env_bind bind)
		in {lvar=lvar, tyvars=tyvars, Type=Type', bind=bind'}
		end

	      val functions' = map f functions
	      val scope' = t env_scope scope
	    in
	      FIX {functions=functions', scope=scope'}
	    end
	 | FRAME{declared_lvars,...} =>
	    let val lvars = map #lvar declared_lvars
	        val _ = env_frame := (List.foldL (fn lv => fn acc => 
						  case lookup_lvar env lv
						    of SOME tvs => add_lvar(lv,tvs,acc)
						     | NONE => acc) empty lvars)
	    in lexp
	    end
	 (* the rest is just trivial traversal *) 
	 | FN {pat, body} => FN {pat=pat, body=t env body}
	 | APP(lexp1, lexp2) => APP(t env lexp1, t env lexp2)
	 | EXCEPTION(excon, tauopt, lexp) => EXCEPTION(excon, tauopt, t env lexp)
	 | RAISE(lexp,tau) => RAISE(t env lexp,tau)
	 | HANDLE(lexp1, lexp2) => HANDLE(t env lexp1, t env lexp2)
	 | SWITCH_I {switch,precision} => SWITCH_I {switch=t_switch t env switch, precision=precision}
	 | SWITCH_W {switch,precision} => SWITCH_W {switch=t_switch t env switch, precision=precision}
	 | SWITCH_S sw => SWITCH_S (t_switch t env sw)
	 | SWITCH_C sw => SWITCH_C (t_switch t env sw) 
	 | SWITCH_E sw => SWITCH_E (t_switch t env sw)   
	 | PRIM(prim, lexps) => PRIM(prim, map (t env) lexps)
	 | _ => lexp
  
    fun elim_eq (env0, PGM ((datbinds as DATBINDS dbss), lexp)) : LambdaPgm * env =
      let 
	val op + = plus
	val (f', env_builtin_dat) = gen_datatype_builtin_eq ()
	val env1 = env0 + env_builtin_dat
	val (f, env_dat) = gen_datatype_eq env1 dbss
	val env2 = env1 + env_dat
	val _ = env_frame := empty
	val lexp' = t env2 lexp                         (* env_frame is updated as a side-effect. *)
	val env_export = env_dat + (!env_frame)
	val lexp'' = extend_frame env_dat lexp'         (* don't put eq. functions for built-in 
							 * datatypes in frame. For in-lining, we
							 * better introduce them in each module. *)
      in (PGM (datbinds, (f' o f) lexp''), env_export)
      end handle e as DONT_SUPPORT_EQ s => 
	(log ("\n ** Equality not supported for datatype " ^ s ^ "\n");
	 log (" ** Rewrite the program to use an explicit equality function\n");
	 log (" ** for this particular datatype.\n\n");
	 raise e)
	 
  end
