(*$EliminateEq: LVARS CON TYNAME LAMBDA_EXP CRASH FLAGS PRETTYPRINT ORDER_FINMAP
                ELIMINATE_EQ MONO_FINMAP *)

functor EliminateEq (structure Lvars : LVARS
		     structure Con : CON
		     structure TyName : TYNAME
		     structure LambdaExp : LAMBDA_EXP
		       sharing type LambdaExp.lvar = Lvars.lvar
			   and type LambdaExp.con = Con.con
			   and type LambdaExp.TyName = TyName.TyName
		     structure Crash : CRASH
		     structure Flags : FLAGS
		     structure PP : PRETTYPRINT
		     structure TyNameMap : MONO_FINMAP
		       sharing type TyNameMap.dom = TyName.TyName
			   and type TyNameMap.StringTree = PP.StringTree
		     structure TyVarMap : ORDER_FINMAP
		       sharing type TyVarMap.dom = LambdaExp.tyvar
	   	           and type TyVarMap.StringTree = PP.StringTree
		     structure LvarMap : MONO_FINMAP
		       sharing type LvarMap.dom = Lvars.lvar
			   and type LvarMap.StringTree = PP.StringTree) 
  : ELIMINATE_EQ =
  struct
    open LambdaExp

    fun die s = Crash.impossible ("EliminateEq." ^ s)

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
      val initial : env = (TyNameMap.empty, TyVarMap.empty, LvarMap.empty)
	
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

      fun restrict((tnmap,tvmap,lvmap),{lvars,tynames}) =
	let val _ = if TyVarMap.isEmpty tvmap then () 
		    else die "restrict.tvmap not empty"
	    val tnmap' = List.foldL (fn tn => fn acc =>
				     case TyNameMap.lookup tnmap tn                (* don't scream if it *)
				       of Some res => TyNameMap.add(tn,res,acc)    (* is not here... *)
					| None => acc) TyNameMap.empty tynames
	    val lvars' = TyNameMap.Fold (fn ((_,POLYLVAR lv), lvars) => lv :: lvars
	                                 | _ => die "restrict.not POLYLVAR") lvars tnmap'
	    val lvmap' = List.foldL (fn lv => fn acc =>
				     case LvarMap.lookup lvmap lv
				       of Some res => LvarMap.add(lv,res,acc)
					| None => die "restrict.lv not in map") 
	                 LvarMap.empty lvars
	in (lvars', (tnmap',tvmap,lvmap'))
	end

      fun eq_tnres (POLYLVAR lv1,POLYLVAR lv2) = Lvars.eq(lv1,lv2)
	| eq_tnres (MONOLVAR _,_) = die "eq_tnres.MONOLVAR"
	| eq_tnres (_,MONOLVAR _) = die "eq_tnres.MONOLVAR"
	| eq_tnres _ = false

      fun eq_lvres(tvs1,tvs2) = (map equality_tyvar tvs1 = map equality_tyvar tvs2)

      fun enrich_tnmap(tnmap1,tnmap2) =
	TyNameMap.Fold (fn ((tn2, res2), b) => b andalso
			   case TyNameMap.lookup tnmap1 tn2
			     of Some res1 => eq_tnres(res1,res2)
			      | None => false) true tnmap2

      fun enrich_lvmap(lvmap1,lvmap2) =
	LvarMap.Fold (fn ((lv2, res2), b) => b andalso
			 case LvarMap.lookup lvmap1 lv2
			   of Some res1 => eq_lvres(res1,res2)
			    | None => false) true lvmap2

      fun enrich((tnmap1,tvmap1,lvmap1),(tnmap2,tvmap2,lvmap2)) =
	let val _ = if TyVarMap.isEmpty tvmap1 andalso TyVarMap.isEmpty tvmap2 then () 
		    else die "enrich.tvmaps not empty"
	in enrich_tnmap(tnmap1,tnmap2) andalso enrich_lvmap(lvmap1,lvmap2)
	end

      fun match_tnmap(tnmap,tnmap0) = 
	let val tnmap = TyNameMap.fromList (TyNameMap.list tnmap)
	in TyNameMap.Fold (fn ((tn0, POLYLVAR lv0),()) =>
			   (case TyNameMap.lookup tnmap tn0
			      of Some(POLYLVAR lv) => Lvars.match(lv,lv0)
			       | _ => ())
                           | _ => die "match_tnmap") () tnmap0;
	  tnmap
	end

      fun match((tnmap,tvmap,lvmap),(tnmap0,tvmap0,lvmap0)) =
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
    in
      type env = env
      val empty : env = empty
      val initial : env = initial
      val plus : env * env -> env = plus
      val add_tyname : TyName * result * env -> env = add_tyname
      val lookup_tyname : env -> TyName -> result Option = lookup_tyname
      val add_tyvar : tyvar * lvar * env -> env = add_tyvar
      val lookup_tyvar : env -> tyvar -> lvar Option = lookup_tyvar
      val add_lvar : lvar * tyvar list * env -> env = add_lvar
      val lookup_lvar : env -> lvar -> tyvar list Option = lookup_lvar
      val env_range : env -> result list = env_range (* only used at top-level *)
      val env_map : (result->result) -> env -> env = env_map (* only used at top-level *)
      val enrich : env * env -> bool = enrich
      val match : env * env -> env = match
      val restrict : env * {lvars:lvar list,tynames:TyName list} -> lvar list * env = restrict
      type StringTree = PP.StringTree
      val layout_env : env -> StringTree = layout_env
    end


    (* ------------------------------------------------------------
     * Some usefull stuff
     * ------------------------------------------------------------ *)

    val tau_bool = CONStype([], TyName.tyName_BOOL)
    fun mk_eq_tau tv = let val tau = TYVARtype tv
		       in ARROWtype([RECORDtype [tau,tau]],[tau_bool])
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

    fun is_prim_tn tn = 
      let open TyName
      in List.exists (fn tn' => TyName.eq(tn',tn)) [tyName_INT,tyName_REAL,tyName_BOOL,tyName_STRING,tyName_REF]
      end



    (* ---------------------------------------------------------------
     * Generate a lambda expression for checking equality of a pair
     * of two values of a given type. 
     * --------------------------------------------------------------- *)

    exception DONT_SUPPORT_EQ  (* When generating functions for checking
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
				    of Some lv => VAR {lvar = lv, instances =[]}
				     | None => FN {pat=[(Lvars.newLvar(),RECORDtype [TYVARtype tv, TYVARtype tv])],
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
	      in
		if is_prim_tn tn then mk_prim_eq tau
		else case lookup_tyname env tn
		       of Some (POLYLVAR lv) =>
			 apply (VAR {lvar = lv, instances = taus}) taus
			| Some (MONOLVAR (lv, tyvars)) =>
			    if map (fn TYVARtype tv => tv
		                     | _ => raise DONT_SUPPORT_EQ) taus = tyvars then
			      apply (lamb_var lv) taus
			    else raise DONT_SUPPORT_EQ
			| Some (FAIL str) => die ("gen_type_eq -- Equality not supported for " ^ 
						  TyName.pr_TyName tn ^ ".")
			| None => die ("gen_type_eq. type name " ^ TyName.pr_TyName tn ^ 
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
			      in SWITCH_C (SWITCH (e, [(Con.con_TRUE, gen_switch (n+1) p taus)], 
						   Some lamb_false))
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
     *        and 'b t = C of 'b t
     *
     * are not supported. Some cases (as the above) can be supported, 
     * however.
     * -------------------------------------------------------------- *)


    (* Generate a fix abstraction for a single datatype binding *)
    fun gen_db env (tyvars,tn,cbs) = (* may raise DONT_SUPPORT_EQ *)
      let 
	val tau_res = CONStype([], TyName.tyName_BOOL)
	fun mk_tau tau = let val tau_arg = RECORDtype [tau, tau]
			 in ARROWtype([tau_arg],[tau_res])
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

	fun mk_sw c e = SWITCH_C(SWITCH(lamb_var p1,[(c, e)], Some lamb_false))
	fun mk_nullary_sw c = mk_sw c lamb_true 
	fun mk_unary_sw c tau =
	  let 
	    val p0' = Lvars.newLvar()
	    val p1' = Lvars.newLvar()
	    fun mk_decon p' p e = 
	      monolet {lvar=p', Type=tau, 
		       bind=PRIM(DECONprim {con=c, instances=map TYVARtype tyvars}, [lamb_var p]),
		       scope=e}
	    val lamb_eq_fn_tau = gen_type_eq env' tau
	    val lamb_true_case =
	      mk_decon p0' p0
	      (mk_decon p1' p1
	       (APP(lamb_eq_fn_tau, PRIM(RECORDprim, [lamb_var p0', lamb_var p1']))))

	  in mk_sw c lamb_true_case
	  end

	fun mk_cases [] = []
	  | mk_cases ((c, typeopt)::rest) =
	  let val p = case typeopt
			of None => (c, mk_nullary_sw c)
			 | Some tau => (c, mk_unary_sw c tau) 
	  in p :: mk_cases rest
	  end
	val big_sw = SWITCH_C(SWITCH(lamb_var p0, mk_cases cbs, None))
	val bind  = mk_abs_eq_fns tyvars lvs
	             (mk_pn p0 0
		      (mk_pn p1 1 big_sw))
	val lvar = case lookup_tyname env' tn
		     of Some(MONOLVAR (lv,_)) => lv
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
	                   add_tyname (tn, MONOLVAR (Lvars.newLvar(), tyvars), env)
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
	    fun fail_env [] env = env  (* do not support equality if exn is raised *)
	      | fail_env ((_,tn,_)::dbs) env = 
	      let val env' = if TyName.equality tn then add_tyname (tn, FAIL "eq. not supported by compiler", env)
			     else env
	      in fail_env dbs env'
	      end
	    val (f, env) = ((gen_datatype_eq_dbs env0 dbs)
			    handle DONT_SUPPORT_EQ => (fn lexp => lexp, fail_env dbs empty))
	    val (f', env') = gen_datatype_eq (plus(env0,env)) dbss'
	  in 
	    (f o f', plus(env, env'))
	  end

    (* ------------------------------------------------------------
     * Environment and code for built-in datatypes.
     * Only list is considered. The other type names are considered
     * basic type names and are treated directly in gen_type_eq.
     * ------------------------------------------------------------ *)

    fun gen_datatype_builtin_eq () =
      let 
	val tn_list = TyName.tyName_LIST
	val tv = fresh_tyvar()
	val tau_tv = TYVARtype tv
	val cbs = [(Con.con_CONS, Some (RECORDtype [tau_tv, CONStype([tau_tv], tn_list)])),
		   (Con.con_NIL, None)]
	val dbss = [[([tv], tn_list,cbs)]]
      in 
	gen_datatype_eq empty dbss
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
		   case opt of Some e => Some (f e) | None => None)
	  in case e
	       of VAR _ => e
		| INTEGER _ => e
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
		| SWITCH_I sw => SWITCH_I(f_sw sw) 
		| SWITCH_S sw => SWITCH_S(f_sw sw) 
		| SWITCH_R sw => SWITCH_R(f_sw sw) 
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
	       of Some lexp => Some (t env lexp) 
		| None => None)

    fun t env lexp =
      case lexp
	of VAR {lvar, instances=[]} =>  (* maybe a recursive call *)
	  (case lookup_lvar env lvar
	     of Some tyvars => apply_eq_fns env (map TYVARtype (eq_tyvars tyvars)) lexp
	      | None => lexp)
	 | VAR {lvar, instances} => (* not a recursive call *)  
	    (case lookup_lvar env lvar
	       of Some tyvars => (* those of the tyvars that admit equality
				  * determines what instances to use. *)
		 let fun sel [] [] = []
		       | sel (tv::tvs) (tau::taus) = 
		           if equality_tyvar tv then tau :: sel tvs taus
			   else sel tvs taus
		       | sel _ _ = die "t.VAR.sel"
		     val instances' = sel tyvars instances
		 in
		   apply_eq_fns env instances' lexp
		 end
		| None => die "f.VAR.lvar not in env")
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
						    of Some tvs => add_lvar(lv,tvs,acc)
						     | None => acc) empty lvars)
	    in lexp
	    end
	 (* the rest is just trivial traversal *) 
	 | FN {pat, body} => FN {pat=pat, body=t env body}
	 | APP(lexp1, lexp2) => APP(t env lexp1, t env lexp2)
	 | EXCEPTION(excon, tauopt, lexp) => EXCEPTION(excon, tauopt, t env lexp)
	 | RAISE(lexp,tau) => RAISE(t env lexp,tau)
	 | HANDLE(lexp1, lexp2) => HANDLE(t env lexp1, t env lexp2)
	 | SWITCH_I sw => SWITCH_I (t_switch t env sw)
	 | SWITCH_S sw => SWITCH_S (t_switch t env sw)
	 | SWITCH_R sw => SWITCH_R (t_switch t env sw)
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
      end
      
  end
