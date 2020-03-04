
structure LambdaStatSem: LAMBDA_STAT_SEM =
  struct
    structure PP = PrettyPrint

    (* ---------------------------------------------------------
     * We assume lambda variables and constructors and exception
     * constructors are distinct and tyvars implemented as
     * naturals.
     * --------------------------------------------------------- *)

    val letrec_polymorphism_only = ref false   (* see the main function below. *)

    open LambdaExp TyName

    fun die s = Crash.impossible ("LambdaStatSem." ^ s)
    fun log_st stringtree = (PP.outputTree ((fn s => TextIO.output(!Flags.log, s)) , stringtree, !Flags.colwidth);
			     TextIO.output(!Flags.log, "\n\n"))
    fun StringTree_to_string st = PP.flatten (PP.format (!Flags.colwidth, st))
    val pr_Type = StringTree_to_string o layoutType

    local
        fun f0 separator pp_x [] = ""
          | f0 separator pp_x [x] = pp_x x
          | f0 separator pp_x (x::xs) = pp_x x ^ separator ^ f0 separator pp_x xs
    in
        fun pp_list0 start finish separator pp_x xs = start ^ f0 separator pp_x xs ^ finish
        fun pp_list a = pp_list0 "[" "]" ", " a
        fun pp_set a = pp_list0 "{" "}" ", " a
        fun pp_tuple a = pp_list0 "(" ")" "," a
        fun pp_enumeration a = pp_list0 "" "" ", " a
    end

    fun log s = TextIO.output(!Flags.log, s)

    (* =================================
     *  LAMBDA STAT OBJECT (Begin)
     * ================================= *)

    (* ---------------------------------------------------------
     *  Manipulations of Types and Type Schemes
     * --------------------------------------------------------- *)

    type TypeScheme = tyvar list * Type

    fun ftv_Type Type : NatSet.Set =
      let fun f (TYVARtype tyvar, s) = NatSet.insert tyvar s
	    | f (ARROWtype (tl1, tl2), s) = foldl f (foldl f s tl1) tl2
	    | f (CONStype (ts, _), s) = foldl f s ts
	    | f (RECORDtype ts, s) = foldl f s ts
      in f (Type, NatSet.empty)
      end

    fun ftv_TypeScheme (bound_tyvars, Type) =
      NatSet.difference (ftv_Type Type) (NatSet.fromList bound_tyvars)

    fun close_Type Type : TypeScheme = (NatSet.list (ftv_Type Type), Type)


    (* ---------------------------------------------------------
     *  Environment
     * --------------------------------------------------------- *)

    structure E : sig
		    type env
		    val empty : env
		    val initial : env
		    val plus : env * env -> env
		    val add_con : con * TypeScheme * env -> env
		    val add_tyname : TyName * con list * env -> env
		    val add_lvar : lvar * TypeScheme * env -> env
		    val add_excon : excon * Type option * env -> env
		    val lookup_con : env -> con -> TypeScheme
		    val lookup_tyname : env -> TyName -> con list
		    val lookup_lvar : env -> lvar -> TypeScheme
		    val lookup_excon : env -> excon -> Type option
		    val ftv_env : env -> NatSet.Set
                    val isin_tv : env -> tyvar -> bool
                    val add_tyvars : tyvar list * env -> env
		    val restrict : env * {lvars:lvar list,
					  tynames:TyName list,
					  cons: con list,
					  excons:excon list} -> env
		    val enrich : env * env -> bool
		    type StringTree
		    val layout_env : env -> StringTree
		    val layoutTypes : Type list -> StringTree

                    val prTypeScheme : tyvar list * Type -> string
                    val prType : Type -> string
                    val prTypes : Type list -> string

		    val pu : env Pickle.pu
		  end =
      struct

	structure ConMap = Con.Map
	structure LvarMap = Lvars.Map
	structure ExconMap = Excon.Map
	structure TyNameMap = TyName.Map

	(* maintain the set of free type variables of an environment;
	 * operations are simple since we know that variables are
	 * unique. This makes ftv_env cheap. *)

	type env = {ftv : NatSet.Set,
		    con_env : TypeScheme ConMap.map,
		    tyname_env : (con list) TyNameMap.map,   (* the con list is the domain of TE *)
		    lvar_env : TypeScheme LvarMap.map,
		    excon_env : (Type option) ExconMap.map}

	val empty_con_env = ConMap.empty
	val empty_tyname_env = TyNameMap.empty
	val empty_lvar_env = LvarMap.empty
	val empty_excon_env = ExconMap.empty

	val empty : env = {ftv = NatSet.empty,
			   con_env = empty_con_env,
			   tyname_env = empty_tyname_env,
			   lvar_env = empty_lvar_env,
			   excon_env = empty_excon_env}

	val initial_con_env =
	  let
(*	    val typescheme_REF =
	      let val tyvar = fresh_tyvar()
	      in close_Type (ARROWtype([TYVARtype tyvar], [CONStype([TYVARtype tyvar], tyName_REF)]))
	      end
*)
	    val typescheme_TRUE = close_Type (CONStype([], tyName_BOOL))
	    val typescheme_FALSE = close_Type (CONStype([], tyName_BOOL))
	    val typescheme_NIL =
	      let val tyvar = fresh_tyvar()
	      in close_Type (CONStype([TYVARtype tyvar], tyName_LIST))
	      end
	    val typescheme_CONS =
	      let val tyvar = fresh_tyvar()
	      in close_Type (ARROWtype([RECORDtype[TYVARtype tyvar,
						  CONStype([TYVARtype tyvar], tyName_LIST)]],
				       [CONStype([TYVARtype tyvar], tyName_LIST)]))
	      end
	    val typescheme_QUOTE =
	      let val tyvar = fresh_tyvar()
	      in close_Type (ARROWtype([CONStype([],tyName_STRING)],
				       [CONStype([TYVARtype tyvar], tyName_FRAG)]))
	      end
	    val typescheme_ANTIQUOTE =
	      let val tyvar = fresh_tyvar()
	      in close_Type (ARROWtype([TYVARtype tyvar],
				       [CONStype([TYVARtype tyvar], tyName_FRAG)]))
	      end
	    val typescheme_INTINF =
		close_Type(ARROWtype([RECORDtype[CONStype([CONStype([],tyName_INT31)],tyName_LIST),
						 CONStype([],tyName_BOOL)]],
				     [CONStype([], tyName_INTINF)]))

	  in
	    ConMap.fromList [ (Con.con_TRUE, typescheme_TRUE),
			      (Con.con_FALSE, typescheme_FALSE),
			      (Con.con_NIL, typescheme_NIL),
			      (Con.con_CONS, typescheme_CONS),
			      (Con.con_QUOTE, typescheme_QUOTE),
			      (Con.con_ANTIQUOTE, typescheme_ANTIQUOTE),
			      (Con.con_INTINF, typescheme_INTINF)]
	  end

	val initial_tyname_env =
	  TyNameMap.fromList  [(tyName_BOOL, [Con.con_TRUE, Con.con_FALSE]),
			       (tyName_INT31, []),
			       (tyName_INT32, []),
			       (tyName_INTINF, [Con.con_INTINF]),
			       (tyName_WORD8, []),
			       (tyName_WORD31, []),
			       (tyName_WORD32, []),
			       (tyName_REAL, []),
			       (tyName_STRING, []),
			       (tyName_CHAR, []),
			       (tyName_LIST, [Con.con_NIL, Con.con_CONS]),
			       (tyName_FRAG, [Con.con_QUOTE, Con.con_ANTIQUOTE]),
			       (tyName_CHARARRAY, []),
			       (tyName_FOREIGNPTR, []),
			       (tyName_ARRAY, []),
			       (tyName_VECTOR, []),
			       (tyName_REF, [(*Con.con_REF*)]),
			       (tyName_EXN, [])]

	val initial_lvar_env = empty_lvar_env

	val initial_excon_env =
	  ExconMap.fromList  [(Excon.ex_DIV, NONE : Type option),
			      (Excon.ex_MATCH, NONE),
			      (Excon.ex_BIND, NONE),
			      (Excon.ex_OVERFLOW, NONE),
			      (Excon.ex_INTERRUPT, NONE)]

	val ftv_initial =
	  ConMap.fold (fn (sigma,set) => NatSet.union (ftv_TypeScheme sigma) set)
	  (LvarMap.fold (fn (sigma,set) => NatSet.union (ftv_TypeScheme sigma) set)
	   (ExconMap.fold (fn (SOME Type,set) => NatSet.union (ftv_Type Type) set
	                  | (NONE,set) => set) NatSet.empty initial_excon_env)
	   initial_lvar_env)
	  initial_con_env

	val initial : env = {ftv=ftv_initial,
			     con_env=initial_con_env,
			     tyname_env=initial_tyname_env,
			     lvar_env=initial_lvar_env,
			     excon_env=initial_excon_env}

	fun plus ({ftv,con_env, tyname_env, lvar_env, excon_env},
		  {ftv=ftv', con_env=con_env', tyname_env=tyname_env',
		   lvar_env=lvar_env', excon_env=excon_env'}) : env =
	  {ftv=NatSet.union ftv ftv',
	   con_env=ConMap.plus (con_env,con_env'),
	   tyname_env=TyNameMap.plus (tyname_env,tyname_env'),
	   lvar_env=LvarMap.plus (lvar_env,lvar_env'),
	   excon_env=ExconMap.plus (excon_env,excon_env')}

	fun add_con (con, TypeScheme, {ftv,con_env,tyname_env,lvar_env,excon_env}) =
	  {ftv=NatSet.union ftv (ftv_TypeScheme TypeScheme),
	   con_env=ConMap.add (con,TypeScheme,con_env),
	   tyname_env=tyname_env,
	   lvar_env=lvar_env,
	   excon_env=excon_env}

	fun add_tyname (tyname, conlist, {ftv,con_env,tyname_env,lvar_env,excon_env}) =
	  {ftv=ftv,
	   con_env=con_env,
	   tyname_env=TyNameMap.add (tyname, conlist, tyname_env),
	   lvar_env=lvar_env,
	   excon_env=excon_env}

	fun add_lvar (lvar, TypeScheme, {ftv,con_env,tyname_env,lvar_env,excon_env}) =
	  {ftv=NatSet.union ftv (ftv_TypeScheme TypeScheme),
	   con_env=con_env,
	   tyname_env=tyname_env,
	   lvar_env=LvarMap.add (lvar,TypeScheme,lvar_env),
	   excon_env=excon_env}

	fun add_excon (excon, TypeOpt, {ftv,con_env,tyname_env,lvar_env,excon_env}) =
	  let val ftv' = case TypeOpt
			   of SOME Type => NatSet.union ftv (ftv_Type Type)
			    | NONE => ftv
	  in {ftv=ftv', con_env=con_env,tyname_env=tyname_env,
	      lvar_env=lvar_env,excon_env=ExconMap.add (excon,TypeOpt,excon_env)}
	  end

	fun lookup_con ({con_env,...} : env) con =
	  case ConMap.lookup con_env con
	    of SOME r => r
	     | NONE => die ("lookup_con.Cannot find " ^ Con.pr_con con)

	fun lookup_tyname ({tyname_env,...} : env) tyname =
	  case TyNameMap.lookup tyname_env tyname
	    of SOME r => r
	     | NONE => die ("lookup_tyname.Cannot find " ^ pr_TyName tyname)

	fun lookup_lvar ({lvar_env,...} : env) lvar =
	  case LvarMap.lookup lvar_env lvar
	    of SOME r => r
	     | NONE => die ("lookup_lvar.Cannot find " ^ Lvars.pr_lvar lvar)

	fun lookup_excon ({excon_env,...} : env) excon =
	  case ExconMap.lookup excon_env excon
	    of SOME r => r
	     | NONE => die ("lookup_excon.Cannot find " ^ Excon.pr_excon excon)

	fun ftv_env ({ftv,...} : env) = ftv

        fun isin_tv (e:env) tv =
            NatSet.member tv (ftv_env e)

        fun add_tyvars (tvs, {ftv,con_env,tyname_env,lvar_env,excon_env} : env) : env =
	  {ftv=NatSet.addList tvs ftv,
	   con_env=con_env,
	   tyname_env=tyname_env,
	   lvar_env=lvar_env,
	   excon_env=excon_env}

	type StringTree = PP.StringTree

	fun layout_con con = PP.LEAF (Con.pr_con con)
	fun layout_seq start finish layout_elem l = PP.NODE {start=start, finish=finish, indent=0, childsep=PP.RIGHT ",",
							     children=map layout_elem l}
	fun layout_cons cons = layout_seq "[" "]" layout_con cons
	fun layout_tyname tyname = PP.LEAF (TyName.pr_TyName tyname)
	fun layout_excon excon = PP.LEAF (Excon.pr_excon excon)
	fun layoutTypeOpt (SOME Type) = layoutType Type
	  | layoutTypeOpt (NONE) = PP.LEAF "NONE"
	fun layoutTypes ts = layout_seq "[" "]" layoutType ts
	fun layout_lvar lvar = PP.LEAF (Lvars.pr_lvar lvar)

	fun layout_tyvars tyvars = layout_seq "(" ")" (PP.LEAF o pr_tyvar) tyvars
	fun layoutTypeScheme (tyvars, Type) = PP.NODE {start="\\/", finish="", indent=0, childsep=PP.LEFT ".",
							children=[layout_tyvars tyvars, layoutType Type]}
        val prTypeScheme =
            StringTree_to_string o layoutTypeScheme

        val prType =
            StringTree_to_string o layoutType

        val prTypes =
            StringTree_to_string o layoutTypes

	fun layout_con_env con_env =
	  ConMap.layoutMap {start="ConEnv: {", eq=" -> ", sep=", ", finish="}"}
	  layout_con layoutTypeScheme con_env

	fun layout_tyname_env tyname_env =
	  TyNameMap.layoutMap {start="TyNameEnv: {", eq=" -> ", sep=", ", finish="}"}
	  layout_tyname layout_cons tyname_env

	fun layout_lvar_env lvar_env =
	  LvarMap.layoutMap {start="LvarEnv: {", eq=" -> ", sep=", ", finish="}"}
	  layout_lvar layoutTypeScheme lvar_env

	fun layout_excon_env excon_env =
	  ExconMap.layoutMap {start="ExConEnv: {", eq=" -> ", sep=", ", finish="}"}
	  layout_excon layoutTypeOpt excon_env

	fun layout_env ({con_env, tyname_env, lvar_env, excon_env,...} : env) =
	  PP.NODE {start="LambdaStatEnv: [",finish="]",childsep=PP.RIGHT "; ",
		   indent=2, children=[layout_con_env con_env,
				       layout_tyname_env tyname_env,
				       layout_lvar_env lvar_env,
				       layout_excon_env excon_env]}

	fun restrict(env as {ftv,con_env,tyname_env,lvar_env,excon_env}: env,{cons,tynames,lvars,excons}) =
	  let
              fun say s = print(s^"\n");
              fun sayenv() = PP.outputTree(print,layout_env env, !Flags.colwidth)
	      fun sayset() = PP.outputTree(print,NatSet.layoutSet {start="{",finish="}",
								   sep=","} (PP.LEAF o pr_tyvar) ftv,
					   !Flags.colwidth)
	      val _ = if NatSet.isEmpty ftv then () (* there can no-longer be free type variables in
						     * a topdec - see EfficientElab/ElabTopdec.sml; mael 2007-11-05 *)
		      else (say ("Restrict: Problem with set of free type variables");
			    say ("not being empty. Here is the environment: ");
			    sayenv();
			    say ("Non-empty set is:");
			    sayset();
			    die "restrict.ftvset not empty")
	      val con_env1 = ConMap.restrict(Con.pr_con,con_env,cons)
                             handle ConMap.Restrict s =>
                               (say ("Problem with constructor environment; constructor " ^ s);
                                say ("is not in the domain of the environment:");
                                sayenv();
                                die "restrict")
	      val tyname_env1 = TyNameMap.restrict(TyName.pr_TyName,tyname_env,tynames)
                             handle TyNameMap.Restrict s =>
                               (say ("Problem with tyname environment; tyname " ^ s);
                                say ("is not in the domain of the environment:");
                                sayenv();
                                die "restrict")
	      val lvar_env1 = LvarMap.restrict(Lvars.pr_lvar,lvar_env,lvars)
                             handle LvarMap.Restrict s =>
                               (say ("Problem with lvar environment; lvar " ^ s);
                                say ("is not in the domain of the environment:");
                                sayenv();
                                die "restrict")
	      val excon_env1 = ExconMap.restrict(Excon.pr_excon,excon_env,excons)
                             handle ExconMap.Restrict s =>
                               (say ("Problem with excon environment; excon " ^ s);
                                say ("is not in the domain of the environment:");
                                sayenv();
                                die "restrict")
	  in {ftv=ftv,con_env=con_env1, tyname_env=tyname_env1,
	      lvar_env=lvar_env1, excon_env=excon_env1}
	  end

	fun enrich _ = true  (* Well, - this module is only here for
			      * the purpose of debugging!! *)


	val pu =
	    let fun to ((f,ce,te,le),ee) = {ftv=f,con_env=ce,tyname_env=te,
					  lvar_env=le,excon_env=ee}
		fun from {ftv=f,con_env=ce,tyname_env=te,
			  lvar_env=le,excon_env=ee} = ((f,ce,te,le),ee)
		val pu_f = NatSet.pu LambdaExp.pu_tyvar
		val pu_ce = Con.Map.pu Con.pu LambdaExp.pu_TypeScheme
		val pu_te = TyName.Map.pu TyName.pu (Pickle.listGen Con.pu)
		val pu_le = Lvars.Map.pu Lvars.pu LambdaExp.pu_TypeScheme
		val pu_ee = Excon.Map.pu Excon.pu (Pickle.optionGen LambdaExp.pu_Type)
	    in Pickle.convert (to,from)
		(Pickle.pairGen0(Pickle.tup4Gen0(pu_f,pu_ce,pu_te,pu_le),pu_ee))
	    end

      end


    open E

    (* ---------------------------------------------------------
     *  Semantic Operations
     * --------------------------------------------------------- *)

    fun valid_t (e:env) (ty:Type) : unit =
	case ty of
	    CONStype(ts,tn) => (lookup_tyname e tn; valid_ts e ts)
	  | ARROWtype(ts1,ts2) => (valid_ts e ts1; valid_ts e ts2)
	  | TYVARtype tv => if isin_tv e tv then ()
                            else die ("valid_t.non-bound type variable " ^ pr_tyvar tv)
	  | RECORDtype ts => valid_ts e ts
    and valid_ts (e:env) nil = ()
      | valid_ts (e:env) (t::ts) = (valid_t e t; valid_ts e ts)

    structure TVS = TyvarSet
    fun valid_s e (tvs,ty) =
        let val s = tyvars_Type TVS.empty ty TVS.empty
          val _ = app (fn tv => if TVS.member tv s then ()
                                else die ("valid_s.Type variable " ^ pr_tyvar tv ^ " not in " ^ prTypeScheme(tvs,ty)))
                      tvs
        in
          valid_t (add_tyvars(tvs,e)) ty
        end

    fun mk_instance ((tyvars,Type):TypeScheme, instances : Type list) =
      let val S = LambdaBasics.mk_subst (fn () => "mk_instance") (tyvars, instances)
      in LambdaBasics.on_Type S Type
      end

    (* we CANNOT use `=' to check equality of types - we use eq_Type. *)

    val eq_Type = LambdaBasics.eq_Type
    val eq_Types = LambdaBasics.eq_Types

    fun eqType s (tau,tau') = if eq_Type(tau,tau') then ()
			      else (log "--------------------------------\n";
				    log ("Error in lambda type checking (" ^ s ^ "):\n");
				    log "The type\n";
				    log_st (layoutType tau);
				    log	"is not compatible with type\n";
				    log_st (layoutType tau');
				    log "--------------------------------\n";
				    die ("eqType"))
    fun eqTypes s ([],[]) = ()
      | eqTypes s (ty1::tys1, ty2::tys2) = (eqType s (ty1,ty2); eqTypes s (tys1, tys2))
      | eqTypes s _ = die "eqTypes"


    val unit_Type = RECORDtype []

    fun tyvars_not_in_env(tyvars, env) =
      if NatSet.isEmpty (NatSet.intersect (NatSet.fromList tyvars) (ftv_env env)) then ()
      else die "tyvars_not_in_env.TYVARS in Env!!"



    (* ---------------------------------------------------------
     *  Type Checking
     * --------------------------------------------------------- *)

    (* Each rule is of the form
     *
     *    E |- lexp : TypeList      where TypeList = Frame ...
     *                                             | RaisedExnBind
     *                                             | Types t1...tn
     *
     * The least upper bound, lub of two TypeList's is used for
     * infer a TypeList for branching expressions. Latice:
     *
     *                  Types o o  Frame
     *                         \|
     *                          o  RaisedExnBind
     *)

    fun lub (tl as Types ts, Types ts') = if eq_Types(ts,ts') then tl
					  else die "lub. Types vs. Types"
      | lub (RaisedExnBind, tl) = tl
      | lub (tl, RaisedExnBind) = tl
      | lub _ = die "lub. not defined for these args"

    fun unTypeList s (Types taus) = taus
      | unTypeList s _ = die ("unTypeList." ^ s)

    fun unTypeListOne s (Types [t]) = t
      | unTypeListOne s _ = die ("unTypeListOne." ^ s)

    infix plus (* on environments *)


    (* Type checking of switches *)
    fun type_switch type_lexp get_tyname (SWITCH (lexp, sel, defopt)) : TypeList =
      let
	val tyname = case type_lexp lexp
		       of Types [CONStype(_, tyname)] => tyname
			| _ => die "SWITCH.Wrong typelist kind"

	fun check sel (SOME e) NONE = check sel NONE (SOME (type_lexp e))
	  | check [] NONE (SOME tl) = tl
	  | check ((a,e)::sel) NONE opttl =
	  let val tn = get_tyname a
	      val tl = type_lexp e
	  in if TyName.eq(tn,tyname) then
	       case opttl
		 of SOME tl' => check sel NONE (SOME(lub(tl, tl')))
		  | NONE => check sel NONE (SOME tl)
	     else let val save_flag = !(Flags.lookup_flag_entry "print_type_name_stamps")
		      val _ = Flags.lookup_flag_entry "print_type_name_stamps" := true;
		      val tn_s = TyName.pr_TyName tn
		      val tyname_s = TyName.pr_TyName tyname
		  in Flags.lookup_flag_entry "print_type_name_stamps" := save_flag;
		    die ("SWITCH.wrong tyname; the type names " ^ tn_s ^ " and " ^ tyname_s ^ " disagree")
		  end
	  end
	  | check _ _ _ = die "check. error"
      in
	check sel defopt NONE
      end


   exception AbortExp

    (* Type checking of primitives *)
    fun type_prim (env:env) (prim:Type prim) lexps : Type list =
      let val type_e = type_lexp env
      in
	case prim
	  of CONprim{con,instances} =>
	    (valid_ts env instances;
	     case lexps
	       of [] => [mk_instance(lookup_con env con, instances)]
		| [lexp] =>
		 (case mk_instance(lookup_con env con, instances)
		    of ARROWtype([t1],[t2]) =>
		      let val ts = unTypeList "CONprim" (type_e lexp)
		      in (eqTypes ("CONprim: " (* ^ Con.pr_con con *)) ([t1],ts); [t2])
		      end
		     | _ => die "CONprim.Unary constructor does not have arrow type")
		| _ => die "CONprim.Wrong number of args")
	   | DECONprim{con,instances,...} =>
	       (valid_ts env instances;
		case lexps
		  of [lexp] => (case mk_instance(lookup_con env con, instances)
				  of ARROWtype([t1],[t2]) =>
				    let val ts = unTypeList "DECONprim0" (type_e lexp)
				    in if eq_Types([t2],ts) then [t1]
				       else die ("DECONprim: " ^ Con.pr_con con
                                                 ^ "; ts = " ^ prTypes ts ^ "; t2 = " ^ prType t2)
				    end
				   | _ => die "DECONprim.Unary constructor does not have arrow type")
		   | _ => die "DECONprim.Wrong number of args")
	   | EXCONprim excon =>
		  (case lexps
		     of [] => (case lookup_excon env excon
				 of NONE => [CONStype([],tyName_EXN)]
				  | SOME _ => die "EXCONprim.Unary excon not fully applied")
		      | [lexp] => (case lookup_excon env excon
				     of SOME t =>
				       let val s = ("EXCONprim: " (* ^ Excon.pr_excon excon *))
					   val ts = unTypeList s (type_e lexp)
				       in if eq_Types([t],ts) then [CONStype([],tyName_EXN)]
					  else die s
				       end
				      | NONE => die "EXCONprim.Nullary excon applied to arg.")
		      | _ => die "EXCONprim.Wrong number of args")
	   | DEEXCONprim excon =>
		     (case lexps
			of [lexp] => (case lookup_excon env excon
					of SOME t =>
					  let val s = ("DEEXCONprim: " (* ^ Excon.pr_excon excon *))
					      val ts = unTypeList s (type_e lexp)
					  in if eq_Types(ts,[CONStype([],tyName_EXN)]) then [t]
					     else die s
					  end
					 | NONE => die "DEEXCONprim.Unary excon does not have arrow type")
			 | _ => die "DEEXCONprim.Wrong number of args")
	   | RECORDprim _ => [RECORDtype(map ((unTypeListOne "RECORDprim") o type_e) lexps)]
	   | SELECTprim i =>
			(case lexps
			   of [lexp] =>
			     (case type_e lexp
				of Types [RECORDtype ts] => ([List.nth (ts,i)]
							     handle _ => die "SELECTprim.Index out of range")
				 | _ => die "SELECTprim.Arg not of record type")
			    | _ => die "SELECTprim.Wrong number of args.")
	   | UB_RECORDprim => map ((unTypeListOne "UB_RECORDprim") o type_e) lexps
	   | DEREFprim {instance} => (* instance: argument type of primitive *)
	       (valid_t env instance;
		case lexps
		  of [lexp] => (case instance
				  of CONStype([t], tyName_REF) =>
				    let val s = "DEREFprim"
				        val ts = unTypeList s (type_e lexp)
				    in if eq_Types(ts,[instance]) then [t]
				       else die s
				    end
				   | _ => die "DEREFprim.Wrong instance")
		   | _ => die "DEREFprim.Wrong number of args")
	   | REFprim {instance} => (* as CONprim *)
		  let val typescheme_REF =
		         let val tyvar = fresh_tyvar()
			 in close_Type (ARROWtype([TYVARtype tyvar], [CONStype([TYVARtype tyvar], tyName_REF)]))
			 end
		  in valid_t env instance;
		      case lexps
		       of [lexp] =>
			 (case mk_instance(typescheme_REF, [instance])
			    of ARROWtype([t1],[t2]) =>
			      let val s = "REFprim"
				  val ts = unTypeList s (type_e lexp)
			      in if eq_Types(ts,[t1]) then [t2]
				 else die s
			      end
			     | _ => die "REFprim.type scheme for ref does not have arrow type")
		      | _ => die "REFprim.Wrong number of args"
		  end
	   | ASSIGNprim {instance} => (* instance: argument type of primitive *)
	       (valid_t env instance;
		case lexps
		  of [lexp1, lexp2] => (case instance
					  of RECORDtype [CONStype([t], tyName_REF), t'] =>
					    let val ts1 = unTypeList "ASSIGNprim1" (type_e lexp1)
					        val ts2 = unTypeList "ASSIGNprim2" (type_e lexp2)
					    in if eq_Type(t,t') andalso eq_Types(ts1,[CONStype([t], tyName_REF)])
					                 andalso eq_Types(ts2,[t']) then [unit_Type]
					       else die "ASSIGNprim3"
					    end
					   | _ => die "ASSIGNprim.Wrong instance kind")
	           | _ => die "ASSIGNprim.Wrong number of args")
	   | DROPprim =>
		  (case lexps
		     of [lexp] => (type_e lexp; nil)
		      | _ => die "DROPprim -- one parameter expected")
	   | EQUALprim {instance} => (* instance: argument type of primitive *)
	       (valid_t env instance;
		case lexps
		  of [lexp1,lexp2] => (case instance
					 of RECORDtype [t1,t2] =>
					   let val ts1 = unTypeList "EQUALprim1" (type_e lexp1)
					       val ts2 = unTypeList "EQUALprim2" (type_e lexp2)
					   in if eq_Type(t1,t2) andalso eq_Types(ts1,[t1])
					                 andalso eq_Types(ts2,[t2]) then [CONStype([], tyName_BOOL)]
					      else die "EQUALprim3"
					   end
					  | _ => die "EQUALprim.Wrong instance kind")
		   | _ => die "EQUALprim.Wrong number of args")
	   | CCALLprim {name, instances, tyvars, Type} =>
	       (valid_ts env instances;
		valid_s env (tyvars,Type);
		case mk_instance ((tyvars, Type), instances) of
		  ARROWtype (ts_arg, ts_res) =>
		    let val ts = map (unTypeListOne "CCALL" o type_e) lexps
		        val ts_res =
			  if eq_Types (ts, ts_arg) then ts_res
			  else (log ("c function " ^ name ^ " expected types:\n");
				log_st (layoutTypes ts_arg);
				log "but found types:\n"; log_st (layoutTypes ts);
				die "c function call")
			val _ =
			  if name = "id" then  (* check that casts are only performed on unboxed values;
						* casting of boxed values is region unsafe! *)
			    (case (ts_arg, ts_res)
			       of ([ta], [tr]) =>
				 let open LambdaExp
				     val unboxed_types = [boolType, unitType, int31Type, word31Type,
							  intDefaultType(), wordDefaultType(), foreignptrType]
				     fun ok t = List.exists (fn t' => LambdaBasics.eq_Type(t,t')) unboxed_types
				 in if ok ta andalso ok tr then ()
				    else die "c function `id' is used to cast to or from a boxed type; \
			                     \ it is region-unsafe to use `id' this way! Rewrite your program!!"
				 end
		                | _ => die "c function `id' does not have a valid type")
			  else ()
		    in ts_res
		    end
		| _ => die ("c function " ^ name ^ " does not have arrow type"))
	   | EXPORTprim {name, instance_arg, instance_res} =>
	       (valid_t env instance_arg;
		valid_t env instance_res;
		let val arrowType = ARROWtype([instance_arg],[instance_res])
		    val ts = map (unTypeListOne "EXPORT" o type_e) lexps
		in if eq_Types ([arrowType],ts) then [unitType]
		   else (log ("Exported function " ^ name ^ " expected function of type:\n");
			 log_st (layoutType arrowType);
			 log "but found type:\n"; log_st (layoutTypes ts);
			 die "Export of function")
		end)
	   | RESET_REGIONSprim {instance} =>
	     (valid_t env instance;
	      case lexps
		of [lexp] =>
		  let val ts = unTypeList "RESET_REGIONSprim1" (type_e lexp)
		  in if eq_Types(ts,[instance]) then [unit_Type]
		     else die "RESET_REGIONSprim2"
		  end
		 | _ => die "RESET_REGIONSprim.Wrong number of args")
	   | FORCE_RESET_REGIONSprim {instance} =>
	     (valid_t env instance;
	      case lexps
		of [lexp] =>
		  let val ts = unTypeList "FORCE_RESET_REGIONSprim" (type_e lexp)
		  in if eq_Types(ts,[instance]) then [unit_Type]
		     else die "FORCE_RESET_REGIONSprim"
		  end
		 | _ => die "FORCE_RESET_REGIONSprim.Wrong number of args")
      end (*fun type_prim*)


    (* Type checking of lambda expressions *)
    and type_lexp (env:env) (lexp:LambdaExp) : TypeList =
      (case lexp
	of VAR{lvar,instances,regvars} => (valid_ts env instances;
				           Types [mk_instance(lookup_lvar env lvar, instances)])
	 | INTEGER (i,t) => (valid_t env t; Types [t])    (* TODO: i31, i32 - compare with literal i *)
	 | WORD (w,t) => (valid_t env t; Types [t])       (* TODO: w31, w32 - compare with literal w *)
	 | STRING s => Types [CONStype([], tyName_STRING)]
	 | REAL s => Types [CONStype([], tyName_REAL)]
	 | FN {pat,body} =>
	  let val env' = foldl (fn ((lvar,Type), env) =>
				     add_lvar(lvar,([],Type),env)) env pat
	      val ts_body = unTypeList "FN" (type_lexp env' body)
	      val ts_arg = map #2 pat
	  in valid_ts env ts_arg; Types [ARROWtype(ts_arg, ts_body)]
	  end
	 | LET {pat=nil,bind,scope} =>   (* wild card *)
	  let val ts = unTypeList "WILD" (type_lexp env bind)
	    val _ = if List.null ts then ()
		    else die "LET.wild -- the binding must be surrounded by a drop expression"
	  in type_lexp env scope
	  end
	 | LET {pat,bind,scope} =>
	  let val env_scope = foldl (fn ((lvar,tyvars,Type), env) =>
				     add_lvar(lvar,(tyvars,Type),env)) env pat

              val tvs = foldl (fn ((_,tvs,_),acc) => tvs @ acc) nil pat
              val env_bind = add_tyvars (tvs, env)

	      val check_polymorphism =
		if !letrec_polymorphism_only then (fn [] => ()
	                                            | _ => die "LET.polymorphic let -- Polymorphism only allowed in FIX.")
		else (fn _ => ())

	      fun check_type_scheme(tyvars, tau, tau') =
		(eqType "LET" (tau,tau');
		 check_polymorphism tyvars;
		 tyvars_not_in_env(tyvars, env))

	      val ts_bind = unTypeList "LET.bind" (type_lexp env_bind bind)

              val _ = app (fn ((_,tyvars,tau), tau') =>
                              (valid_s env (tyvars,tau);
                               check_type_scheme(tyvars, tau, tau')))
	                  (ListPair.zipEq (pat,ts_bind))
                  handle ListPair.UnequalLengths =>
	                 die "LET.pattern and bind type differ in numbers of components"
	  in
	    type_lexp env_scope scope
	  end
         | LETREGION{regvars,scope} => type_lexp env scope
	 | FIX {functions, scope} =>
	  let (* env' is the environment for checking function bodies *)
              val env' = foldl (fn ({lvar,regvars,tyvars,Type,bind}, env) =>   (* memo:regvars *)
				     (valid_s env (tyvars,Type); add_lvar(lvar,([],Type),add_tyvars(tyvars,env)))) env functions
(*	      val _ =
		let type t = {lvar:lvar,tyvars:tyvar list, Type : Type, bind:LambdaExp}
		    fun ch_abs [] = ()
		      | ch_abs [f] = ()
		      | ch_abs (({tyvars,...} : t) :: (fs as {tyvars=tyvars',...} :: _)) =
		           (if tyvars = tyvars' then ()
			    else log "WARNING: FIX. abstracted tyvars not identical!";
			      ch_abs fs)
		in ch_abs functions
		end
*)
	      val type_pairs = map (fn {bind,Type,...} => (unTypeListOne "FIX" (type_lexp env' bind), Type)) functions

              val _ = app (eqType "FIX") type_pairs

	      val env_scope = foldl (fn ({lvar,regvars,tyvars,Type,bind}, env) =>
				      (tyvars_not_in_env(tyvars, env);
				       add_lvar(lvar,(tyvars,Type),env))) env functions
	  in
	    type_lexp env_scope scope
	  end
	 | APP (lexp1, lexp2, _) =>
	  (case type_lexp env lexp1
	     of Types [ARROWtype(ts_arg,ts_res)] =>
	       let val ts = unTypeList "APP" (type_lexp env lexp2)
	       in if eq_Types(ts,ts_arg) then Types ts_res
		  else
		    (log "types expected:\n"; log_st (layoutTypes ts_arg);
		     log "\ntypes:\n"; log_st (layoutTypes ts);
		     log "application:\n"; log_st (layoutLambdaExp lexp);
		     die "APP")
	       end
	      | _ => die "APP.argument type not arrow")
	 | EXCEPTION (excon, typeopt, lexp) =>
	      (case typeopt of SOME t => valid_t env t | NONE => ();
	       type_lexp (add_excon(excon,typeopt,env)) lexp)
	 | RAISE (lexp, tl) =>
	     ((* valid_ts env tl; *)   (* MEMO: Why is this commented out? mael 2007-11-05 *)
	      case type_lexp env lexp
		of Types [CONStype([],tyName_EXN)] => tl
		 | _ => die "RAISE.type not exn")
	 | HANDLE (lexp1, lexp2) =>
		(case type_lexp env lexp2
		   of Types [ARROWtype([CONStype([],tyName_EXN)], ts_res)] =>
		     let val ts = unTypeList "HANDLE" (type_lexp env lexp1)
		     in if eq_Types(ts,ts_res) then Types ts_res
			else die "HANDLE"
		     end
		    | _ => die "HANDLE.wrong handler type")
	 | SWITCH_I {switch, precision} =>
	   let val tn = case precision
			  of 31 => tyName_INT31
			   | 32 => tyName_INT32
                        (* | ~1 => tyName_INTINF *)  (* IntInf's have been compiled away at this point *)
			   | _ => die ("SWITCH_I.precision = " ^ Int.toString precision)
	   in type_switch (type_lexp env) (fn _ => tn) switch
	   end
	 | SWITCH_W {switch, precision} =>
	   let val tn = case precision
			  of 31 => tyName_WORD31  (* word8 type translated into default word type in CompileDec *)
			   | 32 => tyName_WORD32
			   | _ => die "SWITCH_I"
	   in type_switch (type_lexp env) (fn _ => tn) switch
	   end
	 | SWITCH_S sw => type_switch (type_lexp env) (fn (s:string) => tyName_STRING) sw
	 | SWITCH_C sw => type_switch (type_lexp env)
		   (fn (con:con,_) => case lookup_con env con
				      of (_, CONStype(_,tyname)) => tyname
				       | (_, ARROWtype(_,[CONStype(_,tyname)])) => tyname
				       | _ => die "SWITCH_C.Wrong con type") sw
	 | SWITCH_E sw => type_switch (type_lexp env)
		   (fn (excon:excon,_) => (lookup_excon env excon; tyName_EXN)) sw
	 | PRIM (prim, lexps) => Types (type_prim env prim lexps)
	 | FRAME {declared_lvars, declared_excons} =>
		   let
		     fun on_lvar (p as {lvar,tyvars,Type}) =
		       let val (tvs, tau) = lookup_lvar env lvar
(*
                         val _ = if LambdaBasics.eq_sigma((tvs,tau),(tyvars,Type)) then ()
                                 else die ("FRAME.lvars.sigmas not equal: env(" ^ Lvars.pr_lvar lvar ^ ") = "
                                           ^ prTypeScheme(tvs,tau) ^ "; sigma = " ^ prTypeScheme(tyvars,Type))
*)
		       in {lvar=lvar, tyvars=tvs,Type=tau}
		       end
		     fun on_excon (excon,opt) =
                         let val opt = lookup_excon env excon
                         in (excon,opt)
                         end
(*
                         case (opt, lookup_excon env excon) of
                           (NONE, NONE) => (excon,NONE)
                         | (SOME t, SOME t') =>
                           if eq_Type (t,t') then (excon, SOME t')
                           else die "FRAME.excons.type mismatch"
                         | _ => die "FRAME.excons.NONE vs SOME"
*)
		   in
		     Frame {declared_lvars = map on_lvar declared_lvars,
			    declared_excons = map on_excon declared_excons}
		   end
		 ) handle AbortExp => raise AbortExp
                        | ? => (log_st (layoutLambdaExp lexp) ;
                                log_st (layout_env env);
                                raise AbortExp)

  (* Analyse the datatype bindings and yield an environment which
   * maps all constructors to type schemes and all tynames to
   * constructor identifier lists.  *)
  fun analyse_datbinds (DATBINDS dbs) : env =
    let
      fun analyse_datbind (tyvars : tyvar list,tyname,conbind: (con * Type option) list) : env =
	let
	  fun gen_typescheme (SOME tau) = (tyvars, ARROWtype([tau],[CONStype (map TYVARtype tyvars, tyname)]))
	    | gen_typescheme NONE = (tyvars, CONStype (map TYVARtype tyvars, tyname))

	  val env = foldl (fn ((con, tauopt), env) =>
				add_con(con, gen_typescheme tauopt, env)) empty conbind

	in add_tyname(tyname, map #1 conbind, env)
	end
      val concat = foldl (op @) []
    in
      foldl (fn (datbind,env) => (env plus (analyse_datbind datbind)))
      empty (concat dbs)
    end


  (* Convert a frame into an environment. *)
  fun env_from_frame (Frame {declared_lvars, declared_excons}) =
    let val env' = foldl (fn ({lvar,tyvars,Type}, env') => add_lvar(lvar, (tyvars, Type), env'))
                   empty declared_lvars
    in foldl (fn ((excon,tyopt), env') => add_excon(excon, tyopt, env'))
       env' declared_excons
    end
    | env_from_frame _ = die "env_from_frame. No frame"


  (* Type checking of lambda programs *)
  fun type_check {env, pgm=PGM (datbinds,lexp), letrec_polymorphism_only=flag} : env =
    let
      val _ = letrec_polymorphism_only := flag
      val env' = analyse_datbinds datbinds
      val fr = type_lexp (env plus env') lexp
	handle ? => (log_st (layoutLambdaExp lexp) ; raise ?)
      val env'' = env_from_frame fr
    in env' plus env''
    end

  end
