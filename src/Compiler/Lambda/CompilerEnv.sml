(*$CompilerEnv: IDENT TYNAME STRID CON EXCON ENVIRONMENTS LAMBDA_EXP
	LVARS FINMAP FINMAPEQ PRETTYPRINT CRASH COMPILER_ENV *)

functor CompilerEnv(structure Ident: IDENT
		    structure TyName : TYNAME
		    structure StrId : STRID
		      sharing type Ident.strid = StrId.strid
		    structure Con: CON
		    structure Excon: EXCON
		    structure Environments : ENVIRONMENTS
		      sharing type Environments.strid = StrId.strid
			  and type Environments.id = Ident.id
		    structure LambdaExp : LAMBDA_EXP
		      sharing type LambdaExp.TyName = TyName.TyName
		    structure LambdaBasics : LAMBDA_BASICS
		      sharing type LambdaBasics.Type = LambdaExp.Type
                          and type LambdaBasics.tyvar = LambdaExp.tyvar
		    structure Lvars: LVARS
		    structure FinMap: FINMAP
		    structure FinMapEq : FINMAPEQ
		    structure PP: PRETTYPRINT
		      sharing type FinMap.StringTree = PP.StringTree
		          and type LambdaExp.StringTree = PP.StringTree 
			           = FinMapEq.StringTree
		    structure Crash: CRASH
		   ): COMPILER_ENV =
  struct

    fun die s = Crash.impossible ("CompilerEnv."^s)

    type con = Con.con
    type excon = Excon.excon
    type Type = LambdaExp.Type
    type tyvar = LambdaExp.tyvar
    type lvar = Lvars.lvar
    type id = Ident.id
    type longid = Ident.longid
    type strid = StrId.strid
    type TyName = LambdaExp.TyName

    type instance_transformer = int list

    type subst = LambdaBasics.subst
    val mk_subst = LambdaBasics.mk_subst "CompilerEnv.mk_subst"
    fun on_il(S, il) = map (LambdaBasics.on_Type S) il



    datatype result = LVAR of lvar * tyvar list * Type * Type list
                    | CON of con * tyvar list * Type * Type list * instance_transformer
                    | REF       (* ref is *not* a constructor in the backend, but a primitive! *)
                    | EXCON of excon * Type
                    | ABS | NEG | PLUS | MINUS | MUL | LESS 
		    | GREATER | LESSEQ | GREATEREQ
		                (* ABS .. GREATEREQ are place-holders for 
				   the built-in overloaded operators 
				   The compiler must turn these into the
				   corresponding PRIM_APP(n, ...) in
				   the lambda language *)
                    | RESET_REGIONS | FORCE_RESET_REGIONS
                    | PRIM	(* `PRIM' is a place-holder for the built-in
				   prim function: int * 'a -> 'b. The compiler
				   must turn this, plus its constant argument,
				   into a PRIM_APP(n, ...) in the lambda
				   language. *)

    datatype CEnv = CENV of {StrEnv:StrEnv, VarEnv:VarEnv, LvarEnv:LvarEnv}
    and StrEnv    = STRENV of (strid,CEnv) FinMap.map
    and VarEnv    = VARENV of (id,result) FinMap.map
    and LvarEnv   = LVARENV of (lvar,Type list) FinMapEq.map

    val emptyStrEnv    = STRENV FinMap.empty
    and emptyVarEnv    = VARENV FinMap.empty
    and emptyLvarEnv   = LVARENV FinMapEq.empty
    val emptyCEnv      = CENV {StrEnv=emptyStrEnv, VarEnv=emptyVarEnv, LvarEnv=emptyLvarEnv}

    val initMap = List.foldL (fn (v,r) => fn m => FinMap.add(v,r,m)) 
                  FinMap.empty
    val initialStrEnv = emptyStrEnv

    val boolType = LambdaExp.boolType
    val exnType = LambdaExp.exnType
    val tyvar_list = LambdaExp.fresh_eqtyvar()
    val listType = LambdaExp.CONStype([LambdaExp.TYVARtype tyvar_list], TyName.tyName_LIST)

    val initialVarEnv = 
      VARENV(initMap [(Ident.id_PRIM, PRIM),
		      (Ident.id_ABS, ABS),
		      (Ident.id_NEG, NEG),
		      (Ident.id_PLUS, PLUS),
		      (Ident.id_MINUS, MINUS),
		      (Ident.id_MUL, MUL),
		      (Ident.id_LESS, LESS),
		      (Ident.id_GREATER, GREATER),
		      (Ident.id_LESSEQ, LESSEQ),
		      (Ident.id_GREATEREQ, GREATEREQ),
                      (Ident.resetRegions, RESET_REGIONS),
                      (Ident.forceResetting, FORCE_RESET_REGIONS),
		      (Ident.id_REF, REF),
		      (Ident.id_TRUE, CON(Con.con_TRUE,[],boolType,[],[])),
		      (Ident.id_FALSE, CON(Con.con_FALSE,[],boolType,[],[])),
		      (Ident.id_NIL, CON(Con.con_NIL,[tyvar_list],listType,
					 [LambdaExp.TYVARtype tyvar_list],[0])),
		      (Ident.id_CONS, CON(Con.con_CONS,[tyvar_list],listType,
					  [LambdaExp.TYVARtype tyvar_list],[0])),
		      (Ident.id_Match, EXCON(Excon.ex_MATCH, exnType)),
		      (Ident.id_Bind, EXCON(Excon.ex_BIND, exnType))
		      ])

    val initialCEnv = CENV{StrEnv=initialStrEnv,
			   VarEnv=initialVarEnv,
			   LvarEnv=emptyLvarEnv}

    fun declareVar(id, (lv, tyvars, tau), CENV{StrEnv,VarEnv=VARENV map,LvarEnv}) =
      let val il0 = List.map LambdaExp.TYVARtype tyvars
      in CENV{StrEnv=StrEnv, LvarEnv=LvarEnv,
	      VarEnv=VARENV (FinMap.add(id, LVAR (lv,tyvars,tau,il0), map))}
      end

    fun declareCon(id, (con,tyvars,tau,it), CENV{StrEnv,VarEnv=VARENV map,LvarEnv}) =
      let val il0 = List.map LambdaExp.TYVARtype tyvars
      in CENV{StrEnv=StrEnv, LvarEnv=LvarEnv,
	      VarEnv=VARENV(FinMap.add(id,CON (con,tyvars,tau,il0,it), map))}
      end

    fun declareExcon(id, excon, CENV{StrEnv,VarEnv=VARENV map,LvarEnv}) =
      CENV{StrEnv=StrEnv,VarEnv=VARENV(FinMap.add(id,EXCON excon,map)),LvarEnv=LvarEnv}

    fun declare_strid(strid, env, CENV{StrEnv=STRENV m,VarEnv,LvarEnv}) =
      CENV{StrEnv=STRENV(FinMap.add(strid,env,m)),VarEnv=VarEnv,LvarEnv=LvarEnv}

    fun declareLvar(lvar,taus, CENV{StrEnv,VarEnv,LvarEnv=LVARENV map}) = 
      CENV{StrEnv=StrEnv,VarEnv=VarEnv,LvarEnv=LVARENV(FinMapEq.add Lvars.eq (lvar,taus,map))}

    fun plus (CENV{StrEnv,VarEnv,LvarEnv}, CENV{StrEnv=StrEnv',VarEnv=VarEnv',LvarEnv=LvarEnv'}) =
      CENV{StrEnv=plusStrEnv(StrEnv,StrEnv'),
	   VarEnv=plusVarEnv(VarEnv,VarEnv'),
	   LvarEnv=plusLvarEnv(LvarEnv,LvarEnv')}

    and plusStrEnv    (STRENV m1,STRENV m2)       = STRENV (FinMap.plus(m1,m2))
    and plusVarEnv    (VARENV m1,VARENV m2)       = VARENV (FinMap.plus(m1,m2))
    and plusLvarEnv   (LVARENV m1,LVARENV m2)     = LVARENV (FinMapEq.plus Lvars.eq (m1,m2))

    fun lookupLvar (CENV{LvarEnv=LVARENV le,...}) lvar =
      case FinMapEq.lookup Lvars.eq le lvar of
	Some taus => taus
      | None => die("CompilerEnv.lookupLvar(" ^ Lvars.pr_lvar lvar ^ ")")

    fun lookupId (CENV{VarEnv=VARENV m,...}) id =
      case FinMap.lookup m id
	of Some res => res
	 | None => die ("lookupId(" ^ Ident.pr_id id ^ ")")

    fun lookup_strid (CENV{StrEnv=STRENV m,...}) strid =
      case FinMap.lookup m strid
	of Some res => res
	 | None => die ("lookup_strid(" ^ StrId.pr_StrId strid ^ ")")

    fun lookup_longid CEnv longid =
      let fun lookup CEnv ([],id) = lookupId CEnv id
	    | lookup CEnv (strid::strids,id) =
	          lookup (lookup_strid CEnv strid) (strids,id) 
      in lookup CEnv (Ident.decompose longid)
      end
 
    fun lvars_result (LVAR (lv,_,_,_), lvs) = lv :: lvs
      | lvars_result (_, lvs) = lvs
    fun primlvars_result (result, lvs) =
      case result
	of ABS => Lvars.absint_lvar :: Lvars.absfloat_lvar :: lvs
	 | NEG => Lvars.negint_lvar :: Lvars.negfloat_lvar :: lvs
	 | PLUS => Lvars.plus_int_lvar :: Lvars.plus_float_lvar :: lvs
	 | MINUS => Lvars.minus_int_lvar :: Lvars.minus_float_lvar :: lvs
	 | MUL => Lvars.mul_int_lvar :: Lvars.mul_float_lvar :: lvs
	 | LESS => Lvars.less_int_lvar :: Lvars.less_float_lvar :: lvs
	 | GREATER => Lvars.greater_int_lvar :: Lvars.greater_float_lvar :: lvs
	 | LESSEQ => Lvars.lesseq_int_lvar :: Lvars.lesseq_float_lvar :: lvs
	 | GREATEREQ => Lvars.greatereq_int_lvar :: Lvars.greatereq_float_lvar :: lvs
	 | _ => lvs
    fun cons_result (CON (c,_,_,_,_), cs) = c :: cs
      | cons_result (_, cs) = cs
    fun excons_result (EXCON (c,_), cs) = c :: cs
      | excons_result (_, cs) = cs

    fun tynames_tau (LambdaExp.CONStype(taus,t), tns) = tynames_taus(taus,t::tns)
      | tynames_tau (LambdaExp.ARROWtype(taus1,taus2),tns) = tynames_taus(taus1,tynames_taus(taus2,tns))
      | tynames_tau (LambdaExp.RECORDtype(taus), tns) = tynames_taus(taus,tns)
      | tynames_tau (LambdaExp.TYVARtype _, tns) = tns
    and tynames_taus ([],tns) = tns
      | tynames_taus (tau::taus,tns) = tynames_tau(tau,tynames_taus(taus,tns))

    fun tynames_result (LVAR (_,_,tau,il), tns) : TyName list = tynames_taus(il,tynames_tau(tau,tns))
      | tynames_result (CON(_,_,tau,il,_), tns) = tynames_taus(il,tynames_tau(tau,tns))
      | tynames_result (EXCON(_,tau), tns) = tynames_tau(tau,tns)
      | tynames_result (REF, tns) = TyName.tyName_REF :: tns
      | tynames_result (_, tns) = tns

    fun varsOfCEnv (vars_result : result * 'a list -> 'a list) ce : 'a list =
      let fun vars_ce (CENV{VarEnv=VARENV m,StrEnv,...}, vars) =
	    FinMap.fold vars_result (vars_se (StrEnv,vars)) m
	  and vars_se (STRENV m, vars) = FinMap.fold vars_ce vars m
      in vars_ce (ce, [])
      end

    val lvarsOfCEnv = varsOfCEnv lvars_result
    val consOfCEnv = varsOfCEnv cons_result
    val exconsOfCEnv = varsOfCEnv excons_result
    val tynamesOfCEnv = TyName.Set.list o TyName.Set.fromList o (varsOfCEnv tynames_result)
    val primlvarsOfCEnv = varsOfCEnv primlvars_result

(*
    fun primlvarsOfCEnv (CENV{VarEnv=VARENV m,...}) lvars =
      FinMap.fold (fn (ABS,rest) => Lvars.absint_lvar :: Lvars.absfloat_lvar :: rest
		    | (NEG,rest) => Lvars.negint_lvar :: Lvars.negfloat_lvar :: rest
		    | (PLUS,rest) => Lvars.plus_int_lvar :: Lvars.plus_float_lvar :: rest
		    | (MINUS,rest) => Lvars.minus_int_lvar :: Lvars.minus_float_lvar :: rest
		    | (MUL,rest) => Lvars.mul_int_lvar :: Lvars.mul_float_lvar :: rest
		    | (LESS,rest) => Lvars.less_int_lvar :: Lvars.less_float_lvar :: rest
		    | (GREATER,rest) => Lvars.greater_int_lvar :: Lvars.greater_float_lvar :: rest
		    | (LESSEQ,rest) => Lvars.lesseq_int_lvar :: Lvars.lesseq_float_lvar :: rest
		    | (GREATEREQ,rest) => Lvars.greatereq_int_lvar :: Lvars.greatereq_float_lvar :: rest
		    | (_, rest) => rest) lvars m

*)      
(*old
   fun mk_instancestransformer tyvars tyvars0 types =
     (* order types in the order tvs in tyvars appear in tyvars0;
      * tyvars and types are related as tyvars0 and result are related. *)
     let fun index _ _ [] = die "index"
	   | index n x' (x::xs) = if x=x' then n
				  else index (n+1) x' xs
	 fun f [] = []
	   | f (tv0::tyvars0) =
	   let val i = index 0 tv0 tyvars
	   in (List.nth i types :: f tyvars0) handle _ => die "mk_instancestransformer"
	   end
     in f tyvars0
     end
old*)

   fun mk_it tyvars tyvars0 =
     let  fun index _ _ [] = die "index"
	    | index n x' (x::xs) = if x=x' then n
				   else index (n+1) x' xs
          fun f [] a = rev a
	    | f (tv0::tvs0) a = f tvs0 (index 0 tv0 tyvars :: a)
     in f tyvars0 []
     end

   fun apply_it(it,types) =
     let fun sel 0 (x::xs) = x
	   | sel n (x::xs) = sel (n-1) xs
	   | sel _ _ = die "apply_it"
         fun f [] a = rev a
	   | f (n::ns) a = f ns (sel n types :: a)
     in f it []
     end

   fun restrictFinMap(error_str, env : (''a,'b) FinMap.map, dom : ''a list) =
     List.foldL (fn id => fn acc =>
		 let val res = case FinMap.lookup env id
				 of Some res => res
				  | None => die error_str
		 in FinMap.add(id,res,acc)
		 end) FinMap.empty dom

   fun restrictVarEnv(VARENV m, ids) = 
     VARENV(restrictFinMap("restrictCEnv.id not in env", m, ids))

   fun restrictStrEnv(STRENV m, strids) =
     STRENV(restrictFinMap("restrictCEnv.strid not in env", m, strids))

   fun restrictCEnv(CENV{StrEnv,VarEnv,LvarEnv=LvarEnv as LVARENV lvarenv},
		    strids : strid list, ids : id list) =
     if FinMapEq.isEmpty lvarenv then
       CENV{StrEnv=restrictStrEnv (StrEnv, strids),
	    VarEnv=restrictVarEnv (VarEnv, ids),
	    LvarEnv=LvarEnv}
     else die "restrictCEnv.lvarenv not empty"

   fun eq_res (LVAR (lv1,tvs1,tau1,il1), LVAR (lv2,tvs2,tau2,il2)) = 
       Lvars.eq(lv1,lv2) andalso
       LambdaBasics.eq_sigma_with_il((tvs1,tau1,il1),(tvs2,tau2,il2))
     | eq_res (CON(con1,tvs1,tau1,il1,it1), CON(con2,tvs2,tau2,il2,it2)) =
       Con.eq(con1, con2) andalso it1 = it2 andalso
       LambdaBasics.eq_sigma_with_il((tvs1,tau1,il1),(tvs2,tau2,il2))
     | eq_res (REF, REF) = true
     | eq_res (EXCON (excon1,tau1), EXCON (excon2,tau2)) = 
       Excon.eq(excon1,excon2) andalso LambdaBasics.eq_Type(tau1,tau2)
     | eq_res (ABS,ABS) = true
     | eq_res (NEG,NEG) = true
     | eq_res (PLUS,PLUS) = true
     | eq_res (MINUS,MINUS) = true
     | eq_res (MUL,MUL) = true
     | eq_res (LESS,LESS) = true
     | eq_res (GREATER,GREATER) = true
     | eq_res (LESSEQ,LESSEQ) = true
     | eq_res (GREATEREQ,GREATEREQ) = true
     | eq_res (RESET_REGIONS,RESET_REGIONS) = true
     | eq_res (FORCE_RESET_REGIONS,FORCE_RESET_REGIONS) = true
     | eq_res (PRIM,PRIM) = true
     | eq_res _ = false

   fun enrichVarEnv(env1,env2) =
     FinMap.Fold (fn ((id2,res2),b) => b andalso
		  case FinMap.lookup env1 id2
		    of Some res1 => eq_res(res1,res2)
		     | None => false) true env2
     
   fun enrichCEnv(CENV{StrEnv,VarEnv=VARENV env1,LvarEnv=LVARENV lenv1},
		  CENV{StrEnv=StrEnv',VarEnv=VARENV env2,LvarEnv=LVARENV lenv2}) =
     let
       val _ = if FinMapEq.isEmpty lenv1 then () else die "enrichCEnv.lvarenv1 not empty"
       val _ = if FinMapEq.isEmpty lenv2 then () else die "enrichCEnv.lvarenv2 not empty"
     in enrichVarEnv(env1,env2)
     end

   fun matchRes (LVAR (lv,_,_,_), LVAR (lv0,_,_,_)) = Lvars.match(lv,lv0)
     | matchRes (CON(con,_,_,_,it), CON(con0,_,_,_,it0)) = if it = it0 then Con.match(con,con0) else ()
     | matchRes (EXCON (excon,_), EXCON (excon0,_)) = Excon.match(excon,excon0)
     | matchRes _ = ()

   fun matchVarEnv(env, env0) =
     FinMap.Fold(fn ((id0,res0),_) =>
		 case FinMap.lookup env id0
		   of Some res => matchRes(res,res0)
		    | None => ()) () env0 

   fun match(CENV{StrEnv,VarEnv=VARENV env,LvarEnv=LVARENV lenv},
	     CENV{StrEnv=StrEnv',VarEnv=VARENV env0, LvarEnv=LVARENV lenv0}) =
     let
       val _ = if FinMapEq.isEmpty lenv then () else die "match.lvarenv not empty"
       val _ = if FinMapEq.isEmpty lenv0 then () else die "match.lvarenv0 not empty"
     in matchVarEnv(env,env0)
     end


   type TypeScheme = Environments.TypeScheme
   type ElabEnv = Environments.Env

   val compileTypeScheme_knot: (TypeScheme -> tyvar list * Type) Option ref = ref None   (* MEGA HACK *)
   fun set_compileTypeScheme c = compileTypeScheme_knot := (Some c)

   fun constrain (ce: CEnv, elabE : ElabEnv) =
     let open Environments

         val compileTypeScheme = case compileTypeScheme_knot 
				   of ref (Some compileTypeScheme) => compileTypeScheme
				    | ref None => die "compileTypeScheme_knot not set" 
         fun constr_ran (tr, er) =
	   case tr
	     of LVAR(lv,tvs,tau,il) =>
	       (case er
		  of VE.LONGVAR sigma => 
		    let val (tvs',tau') = compileTypeScheme sigma
		        val S = LambdaBasics.match_sigma((tvs,tau),tau')
			val il' = map (LambdaBasics.on_Type S) il
		    in LVAR(lv,tvs',tau',il')
		    end
		   | _ => die "constr_ran.LVAR.longvar expected") 
	      | CON(con,tvs,tau,il,it) =>
	       (case er
		  of VE.LONGVAR sigma =>
		    let val (tvs',tau') = compileTypeScheme sigma
		        val S = LambdaBasics.match_sigma((tvs,tau),tau')
			val il' = map (LambdaBasics.on_Type S) il
		    in CON(con,tvs',tau',il',it)
		    end
		   | VE.LONGCON sigma =>
		    let val (tvs',tau') = compileTypeScheme sigma
		        val S = LambdaBasics.match_sigma((tvs,tau),tau')
			val il' = map (LambdaBasics.on_Type S) il
		    in if LambdaBasics.eq_sigma((tvs,tau),(tvs',tau')) then CON(con,tvs',tau',il',it)
		       else die "constr_ran.CON.type schemes should be equal"
		    end
		   | _ => die "constr_ran.CON.longvar or longcon expected")
	      | EXCON(excon,tau) =>
	       (case er
		  of VE.LONGVAR sigma =>
		    let val (tvs',tau') = compileTypeScheme sigma
		    in if tvs' = [] andalso LambdaBasics.eq_Type(tau,tau') then EXCON(excon,tau)
		       else die "constr_ran.EXCON.LONGVAR"
		    end
		   | VE.LONGEXCON _ => EXCON(excon,tau)  (* we could check equality of types *)
		   | _ => die "constr_ran.EXCON.longvar or longexcon expected")
	      | _ => die "constr_ran.expecting LVAR, CON or EXCON"

         fun constr_ce(CENV{StrEnv, VarEnv, LvarEnv=LvarEnv as LVARENV lvarenv}, elabE) =
	   let val _ = if FinMapEq.isEmpty lvarenv then () else die "constrain.lvarenv not empty"
	       val (elabSE, _, elabVE) = E.un elabE
	   in CENV{StrEnv=constr_se(StrEnv,elabSE), VarEnv=constr_ve(VarEnv,elabVE),
		   LvarEnv=LvarEnv}
	   end

	 and constr_se(STRENV se, elabSE) =
	   STRENV(SE.Fold (fn (strid, elabE) => fn se' =>
			   case FinMap.lookup se strid
			     of Some ce => let val ce' = constr_ce(ce,elabE)
					   in FinMap.add(strid,ce',se')
					   end
			      | None => die "constr_se") FinMap.empty elabSE)
		  
	 and constr_ve(VARENV ve, elabVE) =
	   VARENV(VE.Fold (fn (id, elabRan) => fn ve' =>
			   case FinMap.lookup ve id
			     of Some transRan => let val transRan' = constr_ran(transRan, elabRan)
						 in FinMap.add(id,transRan', ve')
						 end
			      | None => die "constr_ve") FinMap.empty elabVE)
     in constr_ce(ce, elabE)
     end


   type StringTree = FinMap.StringTree

    fun layoutCEnv (CENV{StrEnv,VarEnv,LvarEnv}) =
      PP.NODE{start="CENV(",finish=")",indent=2,
	      children=[layoutStrEnv StrEnv,
			layoutVarEnv VarEnv,
			layoutLvarEnv LvarEnv],
	      childsep=PP.RIGHT ","}

    and layoutStrEnv (STRENV m) =
      PP.NODE{start="StrEnv = ",finish="",indent=2,
	      children= [FinMap.layoutMap {start="{", eq=" -> ", sep=", ", finish="}"}
			 (PP.layoutAtom StrId.pr_StrId)
			 (layoutCEnv) m],
	      childsep=PP.RIGHT ","}

    and layoutVarEnv (VARENV m) =
      PP.NODE{start="VarEnv = ",finish="",indent=2,
	      children= [FinMap.layoutMap {start="{", eq=" -> ", sep=", ", finish="}"}
			               (PP.layoutAtom Ident.pr_id)
				       (PP.layoutAtom 
					(fn LVAR (lv,_,_,_) => "LVAR(" ^ Lvars.pr_lvar lv ^ ")"
                                          | RESET_REGIONS => Ident.pr_id Ident.resetRegions
                                          | FORCE_RESET_REGIONS => Ident.pr_id Ident.forceResetting
					  | PRIM => "PRIM"
					  | ABS => "ABS"
					  | NEG => "NEG"
					  | PLUS => "PLUS"
					  | MINUS => "MINUS"
					  | MUL => "MUL"
					  | LESS => "LESS"
					  | GREATER => "GREATER"
					  | LESSEQ => "LESSEQ"
					  | GREATEREQ => "GREATEREQ"
					  | REF => "REF" 
					  | CON(con,_,_,_,it) => "CON(" ^ Con.pr_con con ^ ")"
					  | EXCON (excon,_) => "EXCON(" ^ Excon.pr_excon excon ^ ")"))
				       m],
	      childsep=PP.NONE}

    and layoutLvarEnv (LVARENV m) =
      let 
	fun f x = PP.NODE{start="[",finish="]",indent=0,
			  children=map LambdaExp.layoutType x,
			  childsep=PP.RIGHT ","}
      in
	PP.NODE{start="LvarEnv = ",finish="",indent=2,
		children= [FinMapEq.layoutMap 
			   {start="{", eq=" -> ", sep=", ", finish="}"}
			   (PP.layoutAtom Lvars.pr_lvar)
			   f
			   m],
		childsep=PP.NONE}
      end

  end;
