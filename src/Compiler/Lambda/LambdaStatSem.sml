(*$LambdaStatSem: CRASH LAMBDA_EXP LAMBDA_BASICS EXCON CON TYNAME
                  LVARS FINMAP FINMAPEQ PRETTYPRINT FLAGS
                  LAMBDA_STAT_SEM KIT_MONO_SET*)

functor LambdaStatSem(structure LambdaExp : LAMBDA_EXP
		      structure LambdaBasics : LAMBDA_BASICS
			sharing type LambdaBasics.LambdaExp = LambdaExp.LambdaExp
			    and type LambdaBasics.excon = LambdaExp.excon
			    and type LambdaBasics.lvar = LambdaExp.lvar
			    and type LambdaBasics.tyvar = LambdaExp.tyvar
			    and type LambdaBasics.Type = LambdaExp.Type
		      structure Excon : EXCON
		        sharing type Excon.excon = LambdaExp.excon
		      structure Con : CON
		        sharing type Con.con = LambdaExp.con
		      structure TyName : TYNAME
		        sharing type TyName.TyName = LambdaExp.TyName
		      structure Lvars : LVARS 
		        sharing type Lvars.lvar = LambdaExp.lvar
		      structure Crash : CRASH
		      structure FinMap : FINMAP
		      structure FinMapEq : FINMAPEQ
		      structure NatSet : KIT_MONO_SET
			sharing type NatSet.elt = LambdaExp.tyvar
		      structure PP : PRETTYPRINT
			sharing type PP.StringTree =
			  LambdaExp.StringTree = 
			  FinMap.StringTree = FinMapEq.StringTree
		      structure Flags : FLAGS) : LAMBDA_STAT_SEM =
  struct


    open Edlib

(*
         	                 (* There is a bug in SML/NJ'93 which causes equality
				  * attributes of type names to propagate
				  * erroneous. Particularly, the type `Type' allows for
				  * equality though it should not (TyName does not admit
				  * equality.) - Martin. *)
    nonfix =
    val op = = 4.3
*)

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
      let fun f (TYVARtype tyvar) s = NatSet.insert tyvar s
	    | f (ARROWtype (tl1, tl2)) s = List.foldL f (List.foldL f s tl1) tl2
	    | f (CONStype (ts, _)) s = List.foldL f s ts
	    | f (RECORDtype ts) s = List.foldL f s ts
      in f Type NatSet.empty
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
		    val restrict : env * {lvars:lvar list,
					  tynames:TyName list,
					  cons: con list,
					  excons:excon list} -> env
		    val enrich : env * env -> bool
		    type StringTree
		    val layout_env : env -> StringTree
		    val layoutTypes : Type list -> StringTree
		  end =
      struct

	(* maintain the set of free type variables of an environment;
	 * operations are simple since we know that variables are
	 * unique. This makes ftv_env cheap. *)

	type env = {ftv : NatSet.Set, 
		    con_env : (con, TypeScheme) FinMapEq.map,
		    tyname_env : (TyName, con list) FinMapEq.map,   (* the con list is the domain of TE *)
		    lvar_env : (lvar, TypeScheme) FinMapEq.map,
		    excon_env : (excon, Type option) FinMapEq.map}	  
	  
	val empty_con_env = FinMapEq.empty
	val empty_tyname_env = FinMapEq.empty
	val empty_lvar_env = FinMapEq.empty
	val empty_excon_env = FinMapEq.empty
	  
	val empty : env = {ftv = NatSet.empty,
			   con_env = empty_con_env,
			   tyname_env = empty_tyname_env,
			   lvar_env = empty_lvar_env,
			   excon_env = empty_excon_env}
	  
	fun initMapeq eq = List.foldL (fn (v,r) => fn m => FinMapEq.add eq (v,r,m)) FinMapEq.empty
	  
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
	  in
	    initMapeq Con.eq [ (* (Con.con_REF, typescheme_REF), *)
			      (Con.con_TRUE, typescheme_TRUE),
			      (Con.con_FALSE, typescheme_FALSE),
			      (Con.con_NIL, typescheme_NIL),
			      (Con.con_CONS, typescheme_CONS)]
	  end

	val initial_tyname_env = 
	  initMapeq TyName.eq [(tyName_BOOL, [Con.con_TRUE, Con.con_FALSE]),
			       (tyName_INT, []),
			       (tyName_CHAR, []),
			       (tyName_WORD, []),
			       (tyName_REAL, []),
			       (tyName_STRING, []),
			       (tyName_LIST, [Con.con_NIL, Con.con_CONS]),
			       (tyName_BYTE_TABLE, []),
			       (tyName_WORD_TABLE, []),
			       (tyName_REF, [(*Con.con_REF*)]),
			       (tyName_EXN, [])]

	val initial_lvar_env = empty_lvar_env
	  
	val initial_excon_env =
	  initMapeq Excon.eq [(Excon.ex_ABS, (NONE : Type option)),
			      (Excon.ex_NEG, NONE),
			      (Excon.ex_SUM, NONE),
			      (Excon.ex_DIFF, NONE),
			      (Excon.ex_PROD, NONE),
			      (Excon.ex_DIV, NONE),
			      (Excon.ex_MOD, NONE),
			      (Excon.ex_MATCH, NONE),
			      (Excon.ex_BIND, NONE)]

	val ftv_initial =
	  FinMapEq.fold (fn (sigma,set) => NatSet.union (ftv_TypeScheme sigma) set)
	  (FinMapEq.fold (fn (sigma,set) => NatSet.union (ftv_TypeScheme sigma) set)
	   (FinMapEq.fold (fn (SOME Type,set) => NatSet.union (ftv_Type Type) set
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
	  let val op + = FinMap.plus
	  in {ftv=NatSet.union ftv ftv', 
	      con_env=FinMapEq.plus Con.eq (con_env,con_env'),
	      tyname_env=FinMapEq.plus TyName.eq (tyname_env,tyname_env'),
	      lvar_env=FinMapEq.plus Lvars.eq (lvar_env,lvar_env'),
	      excon_env=FinMapEq.plus Excon.eq (excon_env,excon_env')}
	  end

	fun add_con (con, TypeScheme, {ftv,con_env,tyname_env,lvar_env,excon_env}) =
	  {ftv=NatSet.union ftv (ftv_TypeScheme TypeScheme),
	   con_env=FinMapEq.add Con.eq (con,TypeScheme,con_env),
	   tyname_env=tyname_env,
	   lvar_env=lvar_env, 
	   excon_env=excon_env}

	fun add_tyname (tyname, conlist, {ftv,con_env,tyname_env,lvar_env,excon_env}) =
	  {ftv=ftv,
	   con_env=con_env,
	   tyname_env=FinMapEq.add TyName.eq (tyname, conlist, tyname_env),
	   lvar_env=lvar_env, 
	   excon_env=excon_env}

	fun add_lvar (lvar, TypeScheme, {ftv,con_env,tyname_env,lvar_env,excon_env}) =
	  {ftv=NatSet.union ftv (ftv_TypeScheme TypeScheme),
	   con_env=con_env,
	   tyname_env=tyname_env,
	   lvar_env=FinMapEq.add Lvars.eq (lvar,TypeScheme,lvar_env), 
	   excon_env=excon_env}

	fun add_excon (excon, TypeOpt, {ftv,con_env,tyname_env,lvar_env,excon_env}) =
	  let val ftv' = case TypeOpt
			   of SOME Type => NatSet.union ftv (ftv_Type Type)
			    | NONE => ftv
	  in {ftv=ftv', con_env=con_env,tyname_env=tyname_env,
	      lvar_env=lvar_env,excon_env=FinMapEq.add Excon.eq (excon,TypeOpt,excon_env)}
	  end

	fun lookup_con ({con_env,...} : env) con =
	  case FinMapEq.lookup Con.eq con_env con
	    of SOME r => r
	     | NONE => die ("lookup_con.Cannot find " ^ Con.pr_con con)

	fun lookup_tyname ({tyname_env,...} : env) tyname =
	  case FinMapEq.lookup TyName.eq tyname_env tyname
	    of SOME r => r
	     | NONE => die ("lookup_tyname.Cannot find " ^ pr_TyName tyname)

	fun lookup_lvar ({lvar_env,...} : env) lvar =
	  case FinMapEq.lookup Lvars.eq lvar_env lvar
	    of SOME r => r
	     | NONE => die ("lookup_lvar.Cannot find " ^ Lvars.pr_lvar lvar)

	fun lookup_excon ({excon_env,...} : env) excon =
	  case FinMapEq.lookup Excon.eq excon_env excon
	    of SOME r => r
	     | NONE => die ("lookup_excon.Cannot find " ^ Excon.pr_excon excon)

	fun ftv_env ({ftv,...} : env) = ftv


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

	fun layout_con_env con_env =
	  FinMapEq.layoutMap {start="ConEnv: {", eq=" -> ", sep=", ", finish="}"}
	  layout_con layoutTypeScheme con_env

	fun layout_tyname_env tyname_env =
	  FinMapEq.layoutMap {start="TyNameEnv: {", eq=" -> ", sep=", ", finish="}"}
	  layout_tyname layout_cons tyname_env

	fun layout_lvar_env lvar_env =
	  FinMapEq.layoutMap {start="LvarEnv: {", eq=" -> ", sep=", ", finish="}"}
	  layout_lvar layoutTypeScheme lvar_env

	fun layout_excon_env excon_env =
	  FinMapEq.layoutMap {start="ExConEnv: {", eq=" -> ", sep=", ", finish="}"}
	  layout_excon layoutTypeOpt excon_env

	fun layout_env ({con_env, tyname_env, lvar_env, excon_env,...} : env) = 
	  PP.NODE {start="LambdaStatEnv: [",finish="]",childsep=PP.RIGHT "; ",
		   indent=2, children=[layout_con_env con_env,
				       layout_tyname_env tyname_env,
				       layout_lvar_env lvar_env,
				       layout_excon_env excon_env]}

        exception FAIL_restrict of string
	fun restrict_finmapeq eq mkstring (m,l) = 
	  List.foldL (fn a => fn acc => FinMapEq.add eq
		      (a,case FinMapEq.lookup eq m a
			   of SOME r => r
			    | NONE => raise FAIL_restrict(mkstring a),acc)) FinMapEq.empty l

	fun restrict_con_env a = restrict_finmapeq Con.eq Con.pr_con a
	fun restrict_tyname_env a = restrict_finmapeq TyName.eq TyName.pr_TyName a
	fun restrict_lvar_env a = restrict_finmapeq Lvars.eq Lvars.pr_lvar a
	fun restrict_excon_env a = restrict_finmapeq Excon.eq Excon.pr_excon a

	fun restrict(env as {ftv,con_env,tyname_env,lvar_env,excon_env},{cons,tynames,lvars,excons}) =
	  let val _ = if NatSet.isEmpty ftv then () else die "restrict.ftvset not empty"
              fun say s = (TextIO.output(TextIO.stdOut, s^"\n"); TextIO.output(!Flags.log, s^"\n"))             
              fun sayenv() = PP.outputTree(say,layout_env env, !Flags.colwidth)
	      val con_env1 = restrict_con_env(con_env,cons)
                             handle FAIL_restrict dom => 
                               (say "problems with constructor environment"; 
                                say ("constructor: " ^ dom);
                                say ("is not in the domain of the environment:");
                                sayenv();
                                con_env(*die "LambdaStatSem.restrict"*))
	      val tyname_env1 = restrict_tyname_env(tyname_env,tynames)
                             handle FAIL_restrict dom => 
                               (say "problems with tyname environment"; 
                                say ("tyname: " ^ dom);
                                say ("is not in the domain of the environment:");
                                sayenv();
                                tyname_env(*die "LambdaStatSem.restrict"*))
	      val lvar_env1 = restrict_lvar_env(lvar_env,lvars)
                             handle FAIL_restrict dom => 
                               (say "problems with lvar environment"; 
                                say ("lvar: " ^ dom);
                                say ("is not in the domain of the environment:");
                                sayenv();
                                lvar_env(*die "LambdaStatSem.restrict"*))
	      val excon_env1 = restrict_excon_env(excon_env,excons)
                             handle FAIL_restrict dom => 
                               (say "problems with excon environment"; 
                                say ("excon: " ^ dom);
                                say ("is not in the domain of the environment:");
                                sayenv();
                                excon_env(*die "LambdaStatSem.restrict"*))
	  in {ftv=ftv,con_env=con_env1, tyname_env=tyname_env1,
	      lvar_env=lvar_env1, excon_env=excon_env1}
	  end

	fun enrich _ = true  (* Well, - this module is only here for
			      * the purpose of debugging!! *)



      end


    open E

    (* ---------------------------------------------------------
     *  Semantic Operations
     * --------------------------------------------------------- *)

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
	     else die "SWITCH.wrong tyname"
	  end
	  | check _ _ _ = die "check. error"  
      in
	check sel defopt NONE
      end


   exception AbortExp

    (* Type checking of primitives *)
    fun type_prim (env:env) (prim:Type prim) lexps : Type list = 
      let
	val type_e = type_lexp env
	fun check_list s ([], []) = ()
	  | check_list s (tn::tns, e::es) = let val t = CONStype([],tn)
						val ts = unTypeList s (type_e e)
					    in if eq_Types([t],ts) then check_list s (tns, es)
					       else die (s ^ ": "
							 ^ pp_list pr_Type [t]
							 ^ "<>"
							 ^ pp_list pr_Type ts)
					    end
	  | check_list s _ = die ("zip " ^ s)

	fun type_prim' argtynames restyname s = (check_list s (argtynames, lexps);
						 [CONStype([],restyname)])

	fun type_prim_unit argtynames s = (check_list s (argtynames, lexps);
					   [unit_Type])
      in
	case prim
	  of CONprim{con,instances} =>
	    (case lexps
	       of [] => [mk_instance(lookup_con env con, instances)]
		| [lexp] =>
		 (case mk_instance(lookup_con env con, instances)
		    of ARROWtype([t1],[t2]) =>
		      let val ts = unTypeList "CONprim" (type_e lexp)
		      in (eqTypes ("CONprim: " ^ Con.pr_con con)([t1],ts); [t2])
		      end
		     | _ => die "CONprim.Unary constructor does not have arrow type")
		| _ => die "CONprim.Wrong number of args")
	   | DECONprim{con,instances} => 
	       (case lexps
		  of [lexp] => (case mk_instance(lookup_con env con, instances)
				  of ARROWtype([t1],[t2]) =>
				    let val s = ("DECONprim: " (* ^ Con.pr_con con *))
				        val ts = unTypeList s (type_e lexp)
				    in if eq_Types([t2],ts) then [t1]
				       else die s
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
	   | RECORDprim => [RECORDtype(map ((unTypeListOne "RECORDprim") o type_e) lexps)]
	   | SELECTprim i =>
			(case lexps
			   of [lexp] =>
			     (case type_e lexp
				of Types [RECORDtype ts] => ([List.nth i ts]
							     handle _ => die "SELECTprim.Index out of range")
				 | _ => die "SELECTprim.Arg not of record type")
			    | _ => die "SELECTprim.Wrong number of args.")  
	   | UB_RECORDprim => map ((unTypeListOne "UB_RECORDprim") o type_e) lexps
	   | NEG_INTprim => type_prim' [tyName_INT] tyName_INT "NEG_INTprim"
	   | NEG_REALprim => type_prim' [tyName_REAL] tyName_REAL "NEG_REALprim"
	   | ABS_INTprim => type_prim' [tyName_INT] tyName_INT "ABS_INTprim"
	   | ABS_REALprim => type_prim' [tyName_REAL] tyName_REAL "ABS_REALprim"
	   | DEREFprim {instance} => (* instance: argument type of primitive *)
	       (case lexps
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
		  in case lexps
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
	       (case lexps
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
	   | MUL_REALprim => type_prim' [tyName_REAL,tyName_REAL] tyName_REAL "MUL_REALprim"
	   | MUL_INTprim => type_prim' [tyName_INT,tyName_INT] tyName_INT "MUL_INTprim"
	   | PLUS_REALprim => type_prim' [tyName_REAL,tyName_REAL] tyName_REAL "PLUS_REALprim"
	   | PLUS_INTprim => type_prim' [tyName_INT,tyName_INT] tyName_INT "PLUS_INTprim"
	   | MINUS_REALprim => type_prim' [tyName_REAL,tyName_REAL] tyName_REAL "MINUS_REALprim"
	   | MINUS_INTprim => type_prim' [tyName_INT,tyName_INT] tyName_INT "MINUS_INTprim"
	   | EQUALprim {instance} => (* instance: argument type of primitive *)
	       (case lexps
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
	   | LESS_REALprim => type_prim' [tyName_REAL,tyName_REAL] tyName_BOOL "LESS_REALprim"
	   | EQUAL_INTprim => type_prim' [tyName_INT,tyName_INT] tyName_BOOL "EQUAL_INTprim"
	   | LESS_INTprim => type_prim' [tyName_INT,tyName_INT] tyName_BOOL "LESS_INTprim"
	   | GREATER_REALprim => type_prim' [tyName_REAL,tyName_REAL] tyName_BOOL "GREATER_REALprim"
	   | GREATER_INTprim => type_prim' [tyName_INT,tyName_INT] tyName_BOOL "GREATER_INTprim"
	   | LESSEQ_REALprim => type_prim' [tyName_REAL,tyName_REAL] tyName_BOOL "LESSEQ_REALprim"
	   | LESSEQ_INTprim => type_prim' [tyName_INT,tyName_INT] tyName_BOOL "LESSEQ_INTprim"
	   | GREATEREQ_REALprim => type_prim' [tyName_REAL,tyName_REAL] tyName_BOOL "GREATEREQ_REALprim"
	   | GREATEREQ_INTprim => type_prim' [tyName_INT,tyName_INT] tyName_BOOL "GREATEREQ_INTprim"
	   | CCALLprim {name, instances, tyvars, Type} => 
	       (case mk_instance ((tyvars, Type), instances) of
		  ARROWtype (ts_arg, ts_res) =>
		    let val ts = map (unTypeListOne "CCALL" o type_e) lexps
		    in
		      if eq_Types (ts, ts_arg) then ts_res
		      else (log ("c function " ^ name ^ " expected types:\n");
			    log_st (layoutTypes ts_arg);
			    log "but found types:\n"; log_st (layoutTypes ts);
			    die "c function call")
		    end
		| _ => die ("c function " ^ name ^ " does not have arrow type"))

	   | RESET_REGIONSprim {instance} =>
	     (case lexps
		of [lexp] => 
		  let val ts = unTypeList "RESET_REGIONSprim1" (type_e lexp)
		  in if eq_Types(ts,[instance]) then [unit_Type]
		     else die "RESET_REGIONSprim2"
		  end
		 | _ => die "RESET_REGIONSprim.Wrong number of args")
	   | FORCE_RESET_REGIONSprim {instance} => 
	     (case lexps
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
	of VAR{lvar,instances} => Types [mk_instance(lookup_lvar env lvar, instances)] 
	 | INTEGER i => Types [CONStype([], tyName_INT)]
	 | STRING s => Types [CONStype([], tyName_STRING)]
	 | REAL s => Types [CONStype([], tyName_REAL)]
	 | FN {pat,body} =>
	  let val env' = List.foldL (fn (lvar,Type) => fn env => 
				     add_lvar(lvar,([],Type),env)) env pat
	      val ts_body = unTypeList "FN" (type_lexp env' body)
	      val ts_arg = map #2 pat
	  in Types [ARROWtype(ts_arg, ts_body)]
	  end
	 | LET {pat,bind,scope} =>
	  let val env' = List.foldL (fn (lvar,tyvars,Type) => fn env => 
				     add_lvar(lvar,(tyvars,Type),env)) env pat

	      val check_polymorphism = 
		if !letrec_polymorphism_only then (fn [] => ()
	                                            | _ => die "LET.polymorphic let -- Polymorphism only allowed in FIX.")
		else (fn _ => ())

	      fun check_type_scheme(tyvars, tau, tau') =
		(eqType "LET" (tau,tau');
		 check_polymorphism tyvars; 
		 tyvars_not_in_env(tyvars, env))


	      val ts_bind = unTypeList "LET.bind" (type_lexp env bind)
	  in 
	    (((List.apply (fn ((_,tyvars,tau), tau') => check_type_scheme(tyvars, tau, tau'))
	       (ListPair.zip (pat,ts_bind))) handle ListPair.Zip => 
	                 die "LET.pattern and bind type differ in numbers of components"));
	    type_lexp env' scope
	  end
	 | FIX {functions, scope} =>
	  let val env' = List.foldL (fn {lvar,tyvars,Type,bind} => fn env =>
				     add_lvar(lvar,([],Type),env)) env functions
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
	      val env'' = List.foldL (fn {lvar,tyvars,Type,bind} => fn env =>
				      (tyvars_not_in_env(tyvars, env);
				       add_lvar(lvar,(tyvars,Type),env))) env functions
	  in
	    List.apply (eqType "FIX") type_pairs;
	    type_lexp env'' scope
	  end
	 | APP (lexp1, lexp2) =>
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
	     type_lexp (add_excon(excon,typeopt,env)) lexp
	 | RAISE (lexp, tl) =>
	     (case type_lexp env lexp
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
	 | SWITCH_I sw => type_switch (type_lexp env) (fn (i:int) => tyName_INT) sw  
	 | SWITCH_S sw => type_switch (type_lexp env) (fn (s:string) => tyName_STRING) sw  
	 | SWITCH_C sw => type_switch (type_lexp env) 
		   (fn (con:con) => case lookup_con env con
				      of (_, CONStype(_,tyname)) => tyname
				       | (_, ARROWtype(_,[CONStype(_,tyname)])) => tyname
				       | _ => die "SWITCH_C.Wrong con type") sw  
	 | SWITCH_E sw => type_switch (type_lexp env) 
		   (fn (excon:excon) => (lookup_excon env excon; tyName_EXN)) sw
	 | PRIM (prim, lexps) => Types (type_prim env prim lexps)
	 | FRAME {declared_lvars, declared_excons} =>
		   let 
		     fun on_lvar {lvar,tyvars,Type} =
		       let val (tyvars, Type) = lookup_lvar env lvar
		       in {lvar=lvar, tyvars=tyvars,Type=Type}
		       end
		     fun on_excon (excon,_) = (excon, lookup_excon env excon)
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

	  val env = List.foldL (fn (con, tauopt) => fn env => 
				add_con(con, gen_typescheme tauopt, env)) empty conbind

	in add_tyname(tyname, map #1 conbind, env)
	end
      val concat = List.foldL (General.curry (op @)) []
    in
      List.foldL (fn datbind => fn env => (env plus (analyse_datbind datbind)))
      empty (concat dbs)
    end


  (* Convert a frame into an environment. *)
  fun env_from_frame (Frame {declared_lvars, declared_excons}) =    			  
    let val env' = List.foldL (fn {lvar,tyvars,Type} => fn env' => add_lvar(lvar, (tyvars, Type), env')) 
                   empty declared_lvars
    in List.foldL (fn (excon,tyopt) => fn env' => add_excon(excon, tyopt, env'))
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
