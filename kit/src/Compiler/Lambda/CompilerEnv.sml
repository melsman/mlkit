(*$CompilerEnv: IDENT CON EXCON LAMBDA_EXP LVARS FINMAP FINMAPEQ
	PRETTYPRINT CRASH COMPILER_ENV *)

functor CompilerEnv(structure Ident: IDENT
		    structure Con: CON
		    structure Excon: EXCON
		    structure LambdaExp : LAMBDA_EXP
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
    type lvar = Lvars.lvar
    type id = Ident.id

    type instance_transformer = int list

    datatype result = LVAR of lvar
                    | CON of con * instance_transformer
                    | REF       (* ref is *not* a constructor in the backend, but a primitive! *)
                    | EXCON of excon
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

    datatype CEnv = CENV of {VarEnv:VarEnv, LvarEnv:LvarEnv}
    and VarEnv    = VARENV of (id,result) FinMap.map
    and LvarEnv   = LVARENV of (lvar,Type list) FinMapEq.map

    val emptyVarEnv    = VARENV FinMap.empty
    and emptyLvarEnv   = LVARENV FinMapEq.empty
    val emptyCEnv      = CENV {VarEnv=emptyVarEnv, LvarEnv=emptyLvarEnv}

    val initMap = List.foldL (fn (v,r) => fn m => FinMap.add(v,r,m)) 
                  FinMap.empty
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
		      (Ident.id_TRUE, CON(Con.con_TRUE,[])),
		      (Ident.id_FALSE, CON(Con.con_FALSE,[])),
		      (Ident.id_NIL, CON(Con.con_NIL,[0])),
		      (Ident.id_CONS, CON(Con.con_CONS,[0])),
		      (Ident.id_Match, EXCON Excon.ex_MATCH),
		      (Ident.id_Bind, EXCON Excon.ex_BIND)
		      ])

    val initialCEnv = CENV{VarEnv=initialVarEnv,
			   LvarEnv=emptyLvarEnv}

    fun declareVar(id, lv, CENV{VarEnv=VARENV map,LvarEnv}) =
      CENV{VarEnv=VARENV (FinMap.add(id, LVAR lv, map)),LvarEnv=LvarEnv}

    fun declareCon(id, (con,it), CENV{VarEnv=VARENV map,LvarEnv}) =
      CENV{VarEnv=VARENV(FinMap.add(id,CON (con,it),map)),LvarEnv=LvarEnv}

    fun declareExcon(id, excon, CENV{VarEnv=VARENV map,LvarEnv}) =
      CENV{VarEnv=VARENV(FinMap.add(id,EXCON excon,map)),LvarEnv=LvarEnv}

    fun declareLvar(lvar,taus, CENV{VarEnv,LvarEnv=LVARENV map}) = 
      CENV{VarEnv=VarEnv,LvarEnv=LVARENV(FinMapEq.add Lvars.eq (lvar,taus,map))}

    fun plus (CENV{VarEnv,LvarEnv}, CENV{VarEnv=VarEnv',LvarEnv=LvarEnv'}) =
      CENV{VarEnv=plusVarEnv(VarEnv,VarEnv'),
	   LvarEnv=plusLvarEnv(LvarEnv,LvarEnv')}

    and plusVarEnv    (VARENV m1,VARENV m2)       = VARENV (FinMap.plus(m1,m2))
    and plusLvarEnv   (LVARENV m1,LVARENV m2)     = LVARENV (FinMapEq.plus Lvars.eq (m1,m2))

    fun lookupId (CENV{VarEnv=VARENV m,...}) id =
      case FinMap.lookup m id
	of Some res => res
	 | None => die ("lookupId(" ^ Ident.pr_id id ^ ")")

    fun lookupLvar (CENV{LvarEnv=LVARENV le,...}) lvar =
      case FinMapEq.lookup Lvars.eq le lvar of
	Some taus => taus
      | None => die("CompilerEnv.lookupLvar(" ^ Lvars.pr_lvar lvar ^ ")")

   (* lvarsOfCEnv is needed so that we can build the map from lvars to
      runtime objects (the dynamic env); see CompileAndRun. It's illegal
      to find an identifier mapping to a PRIM here. *)

    fun lvarsOfCEnv (CENV{VarEnv=VARENV m,...}) =
      (* Return the list of lvars which the declared ids in CEnv are mapped to *)
      FinMap.fold (fn (LVAR lv, rest) => lv :: rest
		    | (_, rest) => rest) nil m

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
      

    fun exconsOfCEnv (CENV{VarEnv=VARENV m,...}) =
      (* Return the list of excons which the declared ids in CEnv are mapped to *)
      FinMap.fold (fn (EXCON excon, rest) => excon :: rest
		    | (_, rest) => rest) nil m

    fun consOfCEnv (CENV{VarEnv=VARENV m,...}) =
      (* Return the list of cons which the declared ids in CEnv are mapped to *)
      FinMap.fold (fn (CON (con,it), rest) => con :: rest
		    | (_, rest) => rest) nil m


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


   fun restrictCEnv(CENV{VarEnv=VARENV env,LvarEnv=LvarEnv as LVARENV lvarenv},
		    {ids:id list}) =
     if FinMapEq.isEmpty lvarenv then
       CENV{VarEnv=VARENV(List.foldL (fn id => fn acc =>
				      let val res = case FinMap.lookup env id
						      of Some res => res
						       | None => die "restrictCEnv.not in env"
				      in FinMap.add(id,res,acc)
				      end) FinMap.empty ids),
	    LvarEnv=LvarEnv}
     else die "restrictCEnv.lvarenv not empty"


   fun eq_res (LVAR lv1, LVAR lv2) = Lvars.eq(lv1,lv2)
     | eq_res (CON(con1,it1), CON(con2,it2)) = Con.eq(con1, con2) andalso it1 = it2
     | eq_res (REF, REF) = true
     | eq_res (EXCON excon1, EXCON excon2) = Excon.eq(excon1,excon2)
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
     
   fun enrichCEnv(CENV{VarEnv=VARENV env1,LvarEnv=LVARENV lenv1},
		  CENV{VarEnv=VARENV env2,LvarEnv=LVARENV lenv2}) =
     let
       val _ = if FinMapEq.isEmpty lenv1 then () else die "enrichCEnv.lvarenv1 not empty"
       val _ = if FinMapEq.isEmpty lenv2 then () else die "enrichCEnv.lvarenv2 not empty"
     in enrichVarEnv(env1,env2)
     end

   fun matchRes (LVAR lv, LVAR lv0) = Lvars.match(lv,lv0)
     | matchRes (CON(con,it), CON(con0,it0)) = if it = it0 then Con.match(con,con0) else ()
     | matchRes (EXCON excon, EXCON excon0) = Excon.match(excon,excon0)
     | matchRes _ = ()

   fun matchVarEnv(env, env0) =
     FinMap.Fold(fn ((id0,res0),_) =>
		 case FinMap.lookup env id0
		   of Some res => matchRes(res,res0)
		    | None => ()) () env0 

   fun match(CENV{VarEnv=VARENV env,LvarEnv=LVARENV lenv},
	     CENV{VarEnv=VARENV env0, LvarEnv=LVARENV lenv0}) =
     let
       val _ = if FinMapEq.isEmpty lenv then () else die "match.lvarenv not empty"
       val _ = if FinMapEq.isEmpty lenv0 then () else die "match.lvarenv0 not empty"
     in matchVarEnv(env,env0)
     end


    type StringTree = FinMap.StringTree

    fun layoutCEnv (CENV{VarEnv,LvarEnv}) =
      PP.NODE{start="CENV(",finish=")",indent=2,
	      children=[layoutVarEnv VarEnv,
			layoutLvarEnv LvarEnv],
	      childsep=PP.RIGHT ","}

    and layoutVarEnv (VARENV m) =
      PP.NODE{start="VarEnv = ",finish="",indent=2,
	      children= [FinMap.layoutMap {start="{", eq=" -> ", sep=", ", finish="}"}
			               (PP.layoutAtom Ident.pr_id)
				       (PP.layoutAtom 
					(fn LVAR lv => "LVAR(" ^ Lvars.pr_lvar lv ^ ")"
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
					  | CON(con,it) => "CON(" ^ Con.pr_con con ^ ")"
					  | EXCON excon => "EXCON(" ^ Excon.pr_excon excon ^ ")"))
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
