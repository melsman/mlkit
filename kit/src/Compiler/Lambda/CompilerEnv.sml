
functor CompilerEnv(structure Ident: IDENT
		    structure TyCon : TYCON
		    structure TyName : TYNAME
		    structure StrId : STRID
		      sharing type Ident.strid = StrId.strid = TyCon.strid
		    structure Con: CON
		    structure Excon: EXCON
		    structure Environments : ENVIRONMENTS
		      sharing type Environments.strid = StrId.strid
		      sharing type Environments.id = Ident.id
		      sharing type Environments.longid = Ident.longid
		      sharing type Environments.tycon = TyCon.tycon
		      sharing type Environments.longtycon = TyCon.longtycon
		      sharing type Environments.longstrid = StrId.longstrid
		    structure LambdaExp : LAMBDA_EXP
		      sharing type LambdaExp.TyName = TyName.TyName
		    structure LambdaBasics : LAMBDA_BASICS
		      sharing type LambdaBasics.Type = LambdaExp.Type
                      sharing type LambdaBasics.tyvar = LambdaExp.tyvar
		    structure Lvars: LVARS
		    structure FinMap: FINMAP
		    structure FinMapEq : FINMAPEQ
		    structure PP: PRETTYPRINT
		      sharing type FinMap.StringTree = PP.StringTree
		      sharing type LambdaExp.StringTree = PP.StringTree 
			           = FinMapEq.StringTree = TyName.StringTree = Environments.StringTree
	            structure Flags : FLAGS
		    structure Crash: CRASH
		    structure Report : REPORT
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
    type longstrid = StrId.longstrid
    type tycon = TyCon.tycon
    type longtycon = TyCon.longtycon
    type TyName = LambdaExp.TyName

    (* layout functions *)
    fun layout_scheme(tvs,tau) =
      PP.NODE{start="",finish="",indent=0,childsep=PP.NOSEP,
	      children=[layout_abs tvs,LambdaExp.layoutType tau]}
    and layout_abs tvs = PP.NODE{start="all(",finish=").",indent=0, childsep=PP.RIGHT ",", 
				 children=map (PP.LEAF o LambdaExp.pr_tyvar)tvs}

    fun pr_scheme sigma = PP.flatten1 (layout_scheme sigma)

    fun pr_il il = "il"

    fun pr_st st = 
      (PP.outputTree (print, st, !Flags.colwidth);
       print "\n")


    datatype result = LVAR of lvar * tyvar list * Type * Type list
                    | CON of con * tyvar list * Type * Type list
                    | REF       (* ref is *not* a constructor in the backend, but a primitive! *)
                    | EXCON of excon * Type
                    | ABS | NEG | PLUS | MINUS | MUL | DIV | MOD | LESS 
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

    type spath = int list

    fun spath_lt (_ : spath, nil : spath) = false
      | spath_lt (nil,_) = true
      | spath_lt (x::xs,y::ys) = x < y orelse (x=y andalso spath_lt(xs,ys))

    structure PathEnv = 
	OrderFinMap(structure Order = struct
					  type T = spath
					  fun lt (a:T) b = spath_lt(a,b)
				      end
		    structure PP = PP
		    structure Report = Report
		    structure Crash = Crash)

    type PathEnv = (lvar*Type) PathEnv.map
    type VarEnv = (id,result) FinMap.map

    datatype CEnv = CENV of {StrEnv:StrEnv, VarEnv:VarEnv, TyEnv: TyEnv, PathEnv:PathEnv}
    and StrEnv  = STRENV of (strid,CEnv) FinMap.map
    and TyEnv = TYENV of (tycon,TyName list * CEnv) FinMap.map

    val emptyStrEnv   : StrEnv = STRENV FinMap.empty
    and emptyVarEnv   : VarEnv = FinMap.empty
    and emptyTyEnv    : TyEnv  = TYENV FinMap.empty
    and emptyPathEnv  : PathEnv = PathEnv.empty
    val emptyCEnv = CENV {StrEnv=emptyStrEnv, VarEnv=emptyVarEnv, 
			  TyEnv=emptyTyEnv, PathEnv=emptyPathEnv}

    fun initMap a = foldl (fn ((v,r), m) => FinMap.add(v,r,m)) FinMap.empty a
    val initialStrEnv = emptyStrEnv

    val boolType = LambdaExp.boolType
    val exnType = LambdaExp.exnType
    val tyvar_nil = LambdaExp.fresh_eqtyvar()
    val nilType = LambdaExp.CONStype([LambdaExp.TYVARtype tyvar_nil], TyName.tyName_LIST)
    val tyvar_cons = LambdaExp.fresh_eqtyvar()
    val consType = 
      let open LambdaExp
	  val t = CONStype([TYVARtype tyvar_cons], TyName.tyName_LIST)
      in ARROWtype([RECORDtype [TYVARtype tyvar_cons, t]],[t])
      end

    val tyvar_quote = LambdaExp.fresh_eqtyvar()
    val quoteType = 
      let open LambdaExp
	  val t = CONStype([TYVARtype tyvar_quote], TyName.tyName_FRAG)
      in ARROWtype([CONStype([],TyName.tyName_STRING)],[t])
      end

    val tyvar_antiquote = LambdaExp.fresh_eqtyvar()
    val antiquoteType = 
      let open LambdaExp
	  val t = CONStype([TYVARtype tyvar_antiquote], TyName.tyName_FRAG)
      in ARROWtype([TYVARtype tyvar_antiquote],[t])
      end

    val initialVarEnv : VarEnv = 
      initMap [(Ident.id_PRIM, PRIM),
	       (Ident.id_ABS, ABS),
	       (Ident.id_NEG, NEG),
	       (Ident.id_PLUS, PLUS),
	       (Ident.id_MINUS, MINUS),
	       (Ident.id_MUL, MUL),
	       (Ident.id_DIV, DIV),
	       (Ident.id_MOD, MOD),
	       (Ident.id_LESS, LESS),
	       (Ident.id_GREATER, GREATER),
	       (Ident.id_LESSEQ, LESSEQ),
	       (Ident.id_GREATEREQ, GREATEREQ),
	       (Ident.resetRegions, RESET_REGIONS),
	       (Ident.forceResetting, FORCE_RESET_REGIONS),
	       (Ident.id_REF, REF),
	       (Ident.id_TRUE, CON(Con.con_TRUE,[],boolType,[])),
	       (Ident.id_FALSE, CON(Con.con_FALSE,[],boolType,[])),
	       (Ident.id_NIL, CON(Con.con_NIL,[tyvar_nil],nilType,
				  [LambdaExp.TYVARtype tyvar_nil])),
	       (Ident.id_CONS, CON(Con.con_CONS,[tyvar_cons],consType,
				   [LambdaExp.TYVARtype tyvar_cons])),
	       (Ident.id_QUOTE, CON(Con.con_QUOTE,[tyvar_quote],quoteType,
				    [LambdaExp.TYVARtype tyvar_quote])),
	       (Ident.id_ANTIQUOTE, CON(Con.con_ANTIQUOTE,[tyvar_antiquote],antiquoteType,
					[LambdaExp.TYVARtype tyvar_antiquote])),		      
	       (Ident.id_Div, EXCON(Excon.ex_DIV, exnType)),
	       (Ident.id_Match, EXCON(Excon.ex_MATCH, exnType)),
	       (Ident.id_Bind, EXCON(Excon.ex_BIND, exnType)),
	       (Ident.id_Overflow, EXCON(Excon.ex_OVERFLOW, exnType)),
	       (Ident.id_Interrupt, EXCON(Excon.ex_INTERRUPT, exnType))
	       ]
    local 
      open TyCon TyName
      fun initialTyEnv() : TyEnv = 	
	  TYENV (initMap [(tycon_INT31, ([tyName_INT31], emptyCEnv)),
			  (tycon_INT32, ([tyName_INT32], emptyCEnv)),
			  (tycon_INT, ([tyName_IntDefault()], emptyCEnv)),
			  (tycon_WORD8, ([tyName_WORD8], emptyCEnv)),
			  (tycon_WORD31, ([tyName_WORD31], emptyCEnv)),
			  (tycon_WORD32, ([tyName_WORD32], emptyCEnv)),
			  (tycon_WORD, ([tyName_WordDefault()], emptyCEnv)),
			  (tycon_REAL, ([tyName_REAL], emptyCEnv)),
			  (tycon_STRING, ([tyName_STRING], emptyCEnv)),
			  (tycon_ARRAY, ([tyName_ARRAY], emptyCEnv)),
			  (tycon_VECTOR, ([tyName_VECTOR], emptyCEnv)),
			  (tycon_CHARARRAY, ([tyName_CHARARRAY], emptyCEnv)),
			  (tycon_FOREIGNPTR, ([tyName_FOREIGNPTR], emptyCEnv)),
			  (tycon_CHAR, ([tyName_WORD8], emptyCEnv)),
			  (tycon_EXN, ([tyName_EXN], emptyCEnv)),
			  (tycon_REF, ([tyName_REF], emptyCEnv)),
			  (tycon_BOOL, ([tyName_BOOL], emptyCEnv)),
			  (tycon_LIST, ([tyName_LIST], emptyCEnv)),
			  (tycon_FRAG, ([tyName_FRAG], emptyCEnv)),
			  (*		       (tycon_WORD_TABLE, ([tyName_WORD_TABLE], emptyCEnv)), *)
			  (tycon_UNIT, ([], emptyCEnv))
			  ])
    in
      fun initialCEnv() = CENV{StrEnv=initialStrEnv,
			       VarEnv=initialVarEnv,
			       TyEnv=initialTyEnv(),
			       PathEnv=emptyPathEnv}

    end (*local*)

    fun declarePath (spath:spath,lv: lvar, tau, CENV{StrEnv,VarEnv,TyEnv,PathEnv}) : CEnv =
	CENV{StrEnv=StrEnv,VarEnv=VarEnv,TyEnv=TyEnv,
	     PathEnv=PathEnv.add(spath,(lv,tau),PathEnv)}
	
    fun lookupPath (CENV{PathEnv,...}) (spath:spath) : (lvar * Type) option =
	PathEnv.lookup PathEnv spath

    fun clearPathEnv (CENV{StrEnv,VarEnv,TyEnv,PathEnv}) =
	CENV{StrEnv=StrEnv,VarEnv=VarEnv,TyEnv=TyEnv,PathEnv=emptyPathEnv}

    fun declareVar(id, (lv, tyvars, tau), CENV{StrEnv,VarEnv=m,TyEnv,PathEnv}) =
      let val il0 = map LambdaExp.TYVARtype tyvars
      in CENV{StrEnv=StrEnv, TyEnv=TyEnv,
	      VarEnv=FinMap.add(id, LVAR (lv,tyvars,tau,il0), m),
	      PathEnv=PathEnv}
      end

    fun declareCon(id, (con,tyvars,tau), CENV{StrEnv,VarEnv=m,TyEnv,PathEnv}) =
      let val il0 = map LambdaExp.TYVARtype tyvars
      in CENV{StrEnv=StrEnv, TyEnv=TyEnv,
	      VarEnv=FinMap.add(id,CON (con,tyvars,tau,il0), m),
	      PathEnv=PathEnv}
      end

    fun declareExcon(id, excon, CENV{StrEnv,VarEnv=map,TyEnv,PathEnv}) =
      CENV{StrEnv=StrEnv,VarEnv=FinMap.add(id,EXCON excon,map),TyEnv=TyEnv,
	   PathEnv=PathEnv}

    fun declare_strid(strid, env, CENV{StrEnv=STRENV m,VarEnv,TyEnv,PathEnv}) =
      CENV{StrEnv=STRENV (FinMap.add(strid,env,m)),VarEnv=VarEnv,TyEnv=TyEnv,
	   PathEnv=PathEnv}

    fun declare_tycon(tycon, a, CENV{StrEnv,VarEnv,TyEnv=TYENV m,PathEnv}) =
      CENV{StrEnv=StrEnv,VarEnv=VarEnv,TyEnv=TYENV(FinMap.add(tycon,a,m)),PathEnv=PathEnv}

    fun plus (CENV{StrEnv,VarEnv,TyEnv,PathEnv}, 
	      CENV{StrEnv=StrEnv',VarEnv=VarEnv',TyEnv=TyEnv',PathEnv=PathEnv'}) =
      CENV{StrEnv=plusStrEnv(StrEnv,StrEnv'),
	   VarEnv=plusVarEnv(VarEnv,VarEnv'),
	   TyEnv=plusTyEnv(TyEnv,TyEnv'),
	   PathEnv=PathEnv.plus(PathEnv,PathEnv')}

    and plusStrEnv (STRENV m1, STRENV m2) : StrEnv = STRENV(FinMap.plus(m1,m2))
    and plusVarEnv (m1: VarEnv, m2: VarEnv) : VarEnv = FinMap.plus(m1,m2)
    and plusTyEnv  (TYENV m1,  TYENV m2)  : TyEnv  = TYENV(FinMap.plus(m1,m2))

    exception LOOKUP_ID
    fun lookupId (CENV{VarEnv=m,...}) id =
      case FinMap.lookup m id
	of SOME res => res
	 | NONE => raise LOOKUP_ID

    fun lookup_strid (CENV{StrEnv=STRENV m,...}) strid =
      case FinMap.lookup m strid
	of SOME res => res
	 | NONE => die ("lookup_strid(" ^ StrId.pr_StrId strid ^ ")")

    fun lookup_longid CEnv longid =
      let fun lookup CEnv ([],id) = lookupId CEnv id
	    | lookup CEnv (strid::strids,id) =
	          lookup (lookup_strid CEnv strid) (strids,id) 
      in SOME(lookup CEnv (Ident.decompose longid))
         handle _ => NONE
      end

    fun lookup_longstrid ce longstrid =
      let val (strids,strid) = StrId.explode_longstrid longstrid
	  fun lookup (ce, []) = lookup_strid ce strid
	    | lookup (ce, strid::strids) = lookup(lookup_strid ce strid, strids)
      in lookup(ce, strids)
      end

    fun lookup_tycon (CENV{TyEnv=TYENV m, ...}) tycon =
      case FinMap.lookup m tycon
	of SOME res => res
	 | NONE => die ("lookup_tycon(" ^ TyCon.pr_TyCon tycon ^ ")")

    fun lookup_longtycon ce longtycon =
      let val (strids,tycon) = TyCon.explode_LongTyCon longtycon
	  fun lookup (ce, []) = lookup_tycon ce tycon
	    | lookup (ce, strid::strids) = lookup(lookup_strid ce strid, strids)
      in lookup(ce, strids)
      end

 
    fun lvars_result (LVAR (lv,_,_,_), lvs) = lv :: lvs
      | lvars_result (_, lvs) = lvs
    fun cons_result (CON (c,_,_,_), cs) = c :: cs
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
      | tynames_result (CON(_,_,tau,il), tns) = tynames_taus(il,tynames_tau(tau,tns))
      | tynames_result (EXCON(_,tau), tns) = tynames_tau(tau,tns)
      | tynames_result (REF, tns) = TyName.tyName_REF :: tns
      | tynames_result (_, tns) = tns

    fun varsOfCEnv (vars_result : result * 'a list -> 'a list) ce : 'a list =
      let fun vars_ce (CENV{VarEnv=m,StrEnv,...}, vars) =
	    FinMap.fold vars_result (vars_se (StrEnv,vars)) m
	  and vars_se (STRENV m, vars) = FinMap.fold vars_ce vars m
      in vars_ce (ce, [])
      end

    val lvarsOfCEnv = varsOfCEnv lvars_result
    val consOfCEnv = varsOfCEnv cons_result
    val exconsOfCEnv = varsOfCEnv excons_result

    fun tynamesOfCEnv ce : TyName list =
      let fun tynames_TEentry((tns,ce),acc) = tynames_E(ce,tns@acc) 
          and tynames_TE(TYENV m, acc) = FinMap.fold tynames_TEentry acc m 
          and tynames_E(CENV{VarEnv=ve, StrEnv, TyEnv, ...}, acc) =
	    let val acc = tynames_SE (StrEnv,acc)
	        val acc = tynames_TE (TyEnv,acc)
	    in FinMap.fold tynames_result acc ve
	    end
	  and tynames_SE(STRENV m, acc) = FinMap.fold tynames_E acc m 
      in (TyName.Set.list o TyName.Set.fromList) (tynames_E(ce,[]))
      end

   (* -------------
    * Restriction
    * ------------- *)

   fun restrictFinMap(error_str, env : (''a,'b) FinMap.map, dom : ''a list) =
     foldl (fn (id, acc) =>
	    let val res = case FinMap.lookup env id
			    of SOME res => res
			     | NONE => die (error_str id)
	    in FinMap.add(id,res,acc)
	    end) FinMap.empty dom

   fun restrictVarEnv(m: VarEnv, ids) : VarEnv = 
     restrictFinMap(fn _ => "restrictCEnv.id not in env", m, ids)

   fun restrictTyEnv(TYENV m, tycons) : TyEnv =
       TYENV (restrictFinMap(fn tc => ("restrictCEnv.tycon " ^ TyCon.pr_TyCon tc ^ " not in env"), m, tycons))

   fun restrictStrEnv(STRENV m, strid_restrs) : StrEnv =
       STRENV (foldl (fn ((strid,restr:Environments.restricter), acc) =>
		      let val res = case FinMap.lookup m strid of 
			  SOME res => restrictCEnv(res,restr)
			| NONE => die "restrictStrEnv.strid not in env"
		      in FinMap.add(strid,res,acc)
		      end) FinMap.empty strid_restrs)

   and restrictCEnv(ce,Environments.Whole) = ce
     | restrictCEnv(CENV{StrEnv,VarEnv,TyEnv,PathEnv}, Environments.Restr{strids,vids,tycons}) =
     CENV{StrEnv=restrictStrEnv(StrEnv,strids),
	  VarEnv=restrictVarEnv(VarEnv,vids),
	  TyEnv=restrictTyEnv(TyEnv,tycons),
	  PathEnv=emptyPathEnv}

   val restrictCEnv = fn (ce, longids) => restrictCEnv(ce, Environments.create_restricter longids)
     

   (* -------------
    * Enrichment
    * ------------- *)

   local

    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

     fun log s = TextIO.output(TextIO.stdOut,s)
     fun debug(s, b) = if !debug_man_enrich then
                         (if b then log("\n" ^ s ^ ": enrich succeeded.")
			  else log("\n" ^ s ^ ": enrich failed."); b)
		       else b


     fun eq_res (LVAR (lv1,tvs1,tau1,il1), LVAR (lv2,tvs2,tau2,il2)) = 
       Lvars.eq(lv1,lv2) andalso
       LambdaBasics.eq_sigma_with_il((tvs1,tau1,il1),(tvs2,tau2,il2))
       | eq_res (CON(con1,tvs1,tau1,il1), CON(con2,tvs2,tau2,il2)) =
       Con.eq(con1, con2) andalso
       LambdaBasics.eq_sigma_with_il((tvs1,tau1,il1),(tvs2,tau2,il2))
       | eq_res (REF, REF) = true
       | eq_res (EXCON (excon1,tau1), EXCON (excon2,tau2)) = 
       Excon.eq(excon1,excon2) andalso LambdaBasics.eq_Type(tau1,tau2)
       | eq_res (ABS,ABS) = true
       | eq_res (NEG,NEG) = true
       | eq_res (PLUS,PLUS) = true
       | eq_res (MINUS,MINUS) = true
       | eq_res (MUL,MUL) = true
       | eq_res (DIV,DIV) = true
       | eq_res (MOD,MOD) = true
       | eq_res (LESS,LESS) = true
       | eq_res (GREATER,GREATER) = true
       | eq_res (LESSEQ,LESSEQ) = true
       | eq_res (GREATEREQ,GREATEREQ) = true
       | eq_res (RESET_REGIONS,RESET_REGIONS) = true
       | eq_res (FORCE_RESET_REGIONS,FORCE_RESET_REGIONS) = true
       | eq_res (PRIM,PRIM) = true
       | eq_res _ = false
       
     fun enrichVarEnv(env1: VarEnv, env2: VarEnv) : bool =
       FinMap.Fold (fn ((id2,res2),b) => b andalso
		    case FinMap.lookup env1 id2
		      of SOME res1 => eq_res(res1,res2)
		       | NONE => false) true env2

     fun eq_tynames(res1,res2) = TyName.Set.eq (TyName.Set.fromList res1) (TyName.Set.fromList res2)
       
     fun enrichTyEnv(TYENV m1, TYENV m2) : bool =
       FinMap.Fold (fn ((id2,(res2,ce2)),b) => b andalso
		    case FinMap.lookup m1 id2
		      of SOME (res1,ce1) => eq_tynames(res1,res2) andalso eqCEnv(ce1,ce2)
		       | NONE => false) true m2
       
     and enrichCEnv(CENV{StrEnv,VarEnv,TyEnv,PathEnv},
		    CENV{StrEnv=StrEnv',VarEnv=VarEnv',TyEnv=TyEnv',
			 PathEnv=PathEnv'}) =
       debug("StrEnv", enrichStrEnv(StrEnv,StrEnv')) andalso 
       debug("VarEnv", enrichVarEnv(VarEnv,VarEnv')) andalso
       debug("TyEnv", enrichTyEnv(TyEnv,TyEnv')) andalso
       PathEnv.isEmpty PathEnv andalso PathEnv.isEmpty PathEnv'
       
     and eqCEnv(ce1,ce2) = enrichCEnv(ce1,ce2) andalso enrichCEnv(ce2,ce1)

     and enrichStrEnv(STRENV se1, STRENV se2) =
       FinMap.Fold (fn ((strid,env2),b) => b andalso
		    case FinMap.lookup se1 strid
		      of SOME env1 => enrichCEnv(env1,env2)
		       | NONE => false) true se2
   in

     val enrichCEnv = enrichCEnv

   end
       

   (* -------------
    * Matching
    * ------------- *)

   local  
     fun matchRes (LVAR (lv,_,_,_), LVAR (lv0,_,_,_)) = Lvars.match(lv,lv0)
       | matchRes (CON(con,_,_,_), CON(con0,_,_,_)) = Con.match(con,con0)
       | matchRes (EXCON (excon,_), EXCON (excon0,_)) = Excon.match(excon,excon0)
       | matchRes _ = ()

     fun matchVarEnv(env: VarEnv, env0: VarEnv) =
       FinMap.Fold(fn ((id,res),_) =>
		   case FinMap.lookup env0 id
		     of SOME res0 => matchRes(res,res0)
		      | NONE => ()) () env 

     fun matchEnv(CENV{StrEnv,VarEnv, ...},
		  CENV{StrEnv=StrEnv0,VarEnv=VarEnv0, ...}) =
       (matchStrEnv(StrEnv,StrEnv0); matchVarEnv(VarEnv,VarEnv0))

     and matchStrEnv(STRENV se, STRENV se0) =
       FinMap.Fold(fn ((strid,env),_) =>
		   case FinMap.lookup se0 strid
		     of SOME env0 => matchEnv(env,env0)
		      | NONE => ()) () se
   in

     val match = matchEnv

   end

   type TypeScheme = Environments.TypeScheme
   type ElabEnv = Environments.Env

   val compileTypeScheme_knot: (TypeScheme -> tyvar list * Type) option ref = ref NONE   (* MEGA HACK *)
   fun set_compileTypeScheme c = compileTypeScheme_knot := (SOME c)

   fun constrain (ce: CEnv, elabE : ElabEnv) =
     let
	 open Environments

         val compileTypeScheme = case compileTypeScheme_knot 
				   of ref (SOME compileTypeScheme) => compileTypeScheme
				    | ref NONE => die "compileTypeScheme_knot not set" 
         fun constr_ran (tr, er) =
	   case tr
	     of LVAR(lv,tvs,tau,il) =>
	       (case er
		  of VE.LONGVAR sigma => 
		    let val (tvs',tau') = compileTypeScheme sigma
		          handle ? => (print ("constr_ran. lvar = " ^ Lvars.pr_lvar lv ^ "\n"); raise ?)
		        val S = LambdaBasics.match_sigma((tvs,tau),tau')
			  handle X => (print ("\nMatch failed for var matching var " ^ Lvars.pr_lvar lv ^ "\n");
				       print "Type scheme in translation env:\n";
				       pr_st (layout_scheme(tvs,tau));
				       print "\nType scheme in signature:\n";
				       pr_st (layout_scheme(tvs',tau'));
				       raise X)
			val il' = map (LambdaBasics.on_Type S) il
		    in LVAR(lv,tvs',tau',il')
		    end
		   | _ => die "constr_ran.LVAR.longvar expected") 
	      | CON(con,tvs,tau,il) =>
	       (case er
		  of VE.LONGVAR sigma =>
		    let val (tvs',tau') = compileTypeScheme sigma
		          handle ? => (print ("constr_ran.LONGVAR: con = " ^ Con.pr_con con ^ "\n"); raise ?)
		        val S = LambdaBasics.match_sigma((tvs,tau),tau')
			  handle X => (print ("\nMatch failed for var matching con " ^ Con.pr_con con ^ "\n");
				       raise X)			
			val il' = map (LambdaBasics.on_Type S) il
		    in CON(con,tvs',tau',il')
		    end
		   | VE.LONGCON sigma =>
		    let val (tvs',tau') = compileTypeScheme sigma
		          handle ? => (print ("constr_ran.LONGCON: con = " ^ Con.pr_con con ^ "\n"); raise ?)
		        val S = LambdaBasics.match_sigma((tvs,tau),tau')
			  handle X => (print ("\nMatch failed for con matching con " ^ Con.pr_con con ^ "\n");
				       raise X)			
			val il' = map (LambdaBasics.on_Type S) il
		    in if LambdaBasics.eq_sigma((tvs,tau),(tvs',tau')) then CON(con,tvs',tau',il')
		       else (print "\nconstr_ran.CON.type schemes should be equal.\n";
                             print "Elaboration environment:\n\n";
                             pr_st (Environments.E.layout elabE);
                             print ("constructor: " ^ Con.pr_con con ^ "\n");
                             print ("sigma1  = ");
                             pr_st (layout_scheme(tvs,tau)); print "\n";
                             print ("sigma2  = ");
                             pr_st (layout_scheme(tvs',tau')); print "\n";
                             die  ("constr_ran.CON.type schemes should be equal")
                            )
		    end
		   | _ => die "constr_ran.CON.longvar or longcon expected")
	      | EXCON(excon,tau) =>
	       (case er
		  of VE.LONGVAR sigma =>
		    let val (tvs',tau') = compileTypeScheme sigma
		          handle ? => (print ("constr_ran.EXCON: excon = " ^ Excon.pr_excon excon ^ "\n"); raise ?)
		    in if tvs' = [] andalso LambdaBasics.eq_Type(tau,tau') then EXCON(excon,tau)
		       else die "constr_ran.EXCON.LONGVAR"
		    end
		   | VE.LONGEXCON _ => EXCON(excon,tau)  (* we could check equality of types *)
		   | _ => die "constr_ran.EXCON.longvar or longexcon expected")
	      | _ => die "constr_ran.expecting LVAR, CON or EXCON"

         fun constr_ce(CENV{StrEnv, VarEnv, TyEnv, PathEnv}, elabE) =
	   let val (elabSE, elabTE, elabVE) = E.un elabE
	   in CENV{StrEnv=constr_se(StrEnv,elabSE), VarEnv=constr_ve(VarEnv,elabVE),
		   TyEnv=constr_te(TyEnv,elabTE),
		   PathEnv=emptyPathEnv}
	   end

	 and constr_se(STRENV se, elabSE) =
	     STRENV(SE.Fold (fn (strid, elabE) => fn se' =>
			     case FinMap.lookup se strid
				 of SOME ce => let val ce' = constr_ce(ce,elabE)
					       in FinMap.add(strid,ce',se')
					       end
			       | NONE => die "constr_se") FinMap.empty elabSE)
		  
	 and constr_ve(ve, elabVE) =
	   VE.Fold (fn (id, elabRan) => fn ve' =>
		    case FinMap.lookup ve id
		      of SOME transRan => let val transRan' = constr_ran(transRan, elabRan)
					  in FinMap.add(id,transRan', ve')
					  end
		       | NONE => die "constr_ve") FinMap.empty elabVE

	 and constr_te(TYENV te, elabTE) =
	     TYENV(TE.Fold(fn (tycon, _) => fn te' =>
			   case FinMap.lookup te tycon
			       of SOME tynames => FinMap.add(tycon,tynames,te')
			     | NONE => die "constr_te") FinMap.empty elabTE)
		   
(*	 val _ = print "\n[Constraining ...\n" *)
	 val res = constr_ce(ce, elabE)
(*	 val _ = print " Done]\n" *)
     in res
     end


   fun pp_tynames l =
     let fun pp [] = ""
	   | pp [t] = TyName.pr_TyName t 
	   | pp (t::ts) = TyName.pr_TyName t ^ "," ^ pp ts
     in "{" ^ pp l ^ "}"
     end

   type StringTree = FinMap.StringTree

   fun layout_spath (l:spath) = PP.HNODE{start="{",finish="}",childsep=PP.RIGHT",",
					 children=map (fn i => PP.LEAF(Int.toString i)) l}

    fun layoutCEnv (CENV{StrEnv,VarEnv,TyEnv, PathEnv}) =
      PP.NODE{start="CENV(",finish=")",indent=2,
	      children=[layoutStrEnv StrEnv,
			layoutVarEnv VarEnv,
			layoutTyEnv TyEnv,
			layoutPathEnv PathEnv],
	      childsep=PP.RIGHT ","}

    and layoutLvar lv = PP.LEAF (Lvars.pr_lvar lv)
    and layoutLvarTypePair (lv,t) = PP.NODE{start="(",finish=")",indent=1,childsep=PP.RIGHT ",",
					    children=[layoutLvar lv,LambdaExp.layoutType t]}
    and layoutPathEnv e = PathEnv.layoutMap {start="PathEnv = ",finish="",eq=" -> ", sep = ", "}
	layout_spath layoutLvarTypePair e

    and layoutStrEnv (STRENV m) =
      PP.NODE{start="StrEnv = ",finish="",indent=2,
	      children= [FinMap.layoutMap {start="{", eq=" -> ", sep=", ", finish="}"}
			 (PP.layoutAtom StrId.pr_StrId)
			 (layoutCEnv) m],
	      childsep=PP.RIGHT ","}

    and layoutVarEnv (m : VarEnv) =
      PP.NODE{start="VarEnv = ",finish="",indent=2,
	      children= [FinMap.layoutMap {start="{", eq=" -> ", sep=", ", finish="}"}
			               (PP.layoutAtom Ident.pr_id)
				       (PP.layoutAtom
					(fn LVAR (lv,tyvars,Type,il) => ("LVAR(" ^ Lvars.pr_lvar lv ^ ", " ^ 
					                                 pr_scheme (tyvars,Type) ^ ", " ^ 
									 pr_il il ^ ")")
                                          | RESET_REGIONS => Ident.pr_id Ident.resetRegions
                                          | FORCE_RESET_REGIONS => Ident.pr_id Ident.forceResetting
					  | PRIM => "PRIM"
					  | ABS => "ABS"
					  | NEG => "NEG"
					  | PLUS => "PLUS"
					  | MINUS => "MINUS"
					  | MUL => "MUL"
					  | DIV => "DIV"
					  | MOD => "MOD"
					  | LESS => "LESS"
					  | GREATER => "GREATER"
					  | LESSEQ => "LESSEQ"
					  | GREATEREQ => "GREATEREQ"
					  | REF => "REF" 
					  | CON(con,_,_,_) => "CON(" ^ Con.pr_con con ^ ")"
					  | EXCON (excon,_) => "EXCON(" ^ Excon.pr_excon excon ^ ")"))
				       m],
	      childsep=PP.NOSEP}

    and layoutTyEnv (TYENV m) = 
      let fun layout_res (tynames,ce) = 
	    PP.NODE {start="([",finish="], ce)",indent=0, children=map TyName.layout tynames,
		     childsep=PP.RIGHT ","}
      in FinMap.layoutMap {start="TyEnv = {", eq=" -> ", sep = ", ", finish="}"} (PP.LEAF o TyCon.pr_TyCon)
	 layout_res m
      end

    val pu_result =
	let open Pickle
	    fun toInt (LVAR _) = 0
	      | toInt (CON _) = 1
	      | toInt REF = 2
	      | toInt (EXCON _) = 3
	      | toInt ABS = 4
	      | toInt NEG = 5
	      | toInt PLUS = 6
	      | toInt MINUS = 7
	      | toInt MUL = 8
	      | toInt DIV = 9
	      | toInt MOD = 10
	      | toInt LESS = 11
	      | toInt GREATER = 12
	      | toInt LESSEQ = 13
	      | toInt GREATEREQ = 14
	      | toInt RESET_REGIONS = 15
	      | toInt FORCE_RESET_REGIONS = 16
	      | toInt PRIM = 17          
	    fun eq (LVAR (lv1,tvs1,t1,ts1), LVAR (lv2,tvs2,t2,ts2)) = 
		#4 Lvars.pu(lv1,lv2) andalso
		#4 LambdaExp.pu_tyvars(tvs1,tvs2) andalso
		#4 LambdaExp.pu_Type(t1,t2) andalso
		#4 LambdaExp.pu_Types(ts1,ts2)
	      | eq (CON (c1,tvs1,t1,ts1), CON (c2,tvs2,t2,ts2)) =
		#4 Con.pu(c1,c2) andalso
		#4 LambdaExp.pu_tyvars(tvs1,tvs2) andalso
		#4 LambdaExp.pu_Type(t1,t2) andalso
		#4 LambdaExp.pu_Types(ts1,ts2)
	      | eq (REF,REF) = true
	      | eq (EXCON (e1,t1), EXCON (e2,t2)) =
		#4 Excon.pu(e1,e2) andalso
		#4 LambdaExp.pu_Type(t1,t2)
	      | eq (ABS,ABS) = true
	      | eq (NEG,NEG) = true
	      | eq (PLUS,PLUS) = true
	      | eq (MINUS,MINUS) = true
	      | eq (MUL,MUL) = true
	      | eq (DIV,DIV) = true
	      | eq (MOD,MOD) = true
	      | eq (LESS,LESS) = true
	      | eq (GREATER,GREATER) = true
	      | eq (LESSEQ,LESSEQ) = true
	      | eq (GREATEREQ,GREATEREQ) = true
	      | eq (RESET_REGIONS,RESET_REGIONS) = true
	      | eq (FORCE_RESET_REGIONS,FORCE_RESET_REGIONS) = true
	      | eq (PRIM,PRIM) = true
	      | eq _ = false
	    fun fun_LVAR _ = 
		    con1 eq LVAR (fn LVAR lv => lv | _ => die "pu.LVAR")
		    (tup4Gen(Lvars.pu, LambdaExp.pu_tyvars,
			     LambdaExp.pu_Type, LambdaExp.pu_Types))
	    fun fun_CON _ = 
		    con1 eq CON (fn CON lv => lv | _ => die "pu.CON")
		    (tup4Gen(Con.pu, LambdaExp.pu_tyvars,
			     LambdaExp.pu_Type, LambdaExp.pu_Types))
	    fun fun_EXCON _ = 
		    con1 eq EXCON (fn EXCON lv => lv | _ => die "pu.CON")
		    (pairGen(Excon.pu,LambdaExp.pu_Type))
	in dataGen(toInt,eq,
		   [fun_LVAR,
		    fun_CON,
		    con0 eq REF,
		    fun_EXCON,
		    con0 eq ABS,
		    con0 eq NEG,
		    con0 eq PLUS,
		    con0 eq MINUS,
		    con0 eq MUL,
		    con0 eq DIV,
		    con0 eq MOD,
		    con0 eq LESS,
		    con0 eq GREATER,
		    con0 eq LESSEQ,
		    con0 eq GREATEREQ,
		    con0 eq RESET_REGIONS,
		    con0 eq FORCE_RESET_REGIONS,
		    con0 eq PRIM])
	end

    val pu_PathEnv = 
	let open Pickle
	in PathEnv.pu (listGen int) (pairGen(Lvars.pu,LambdaExp.pu_Type))
	end
    val pu_VarEnv =
	let open Pickle
	in FinMap.pu(Ident.pu,pu_result)
	end
    val (pu,_,_) =
	let open Pickle
	    val pu_TyNames = listGen TyName.pu
	    fun CEnvToInt _ = 0
	    fun StrEnvToInt _ = 0
	    fun TyEnvToInt _ = 0
	    fun CEnvEq(CENV{StrEnv=se1,VarEnv=ve1,TyEnv=te1,PathEnv=pe1},
		       CENV{StrEnv=se2,VarEnv=ve2,TyEnv=te2,PathEnv=pe2}) =
		#4 pu_PathEnv (pe1,pe2) andalso #4 pu_VarEnv (ve1,ve2)
		andalso StrEnvEq(se1,se2) andalso TyEnvEq(te1,te2)
	    and StrEnvEq(STRENV m1, STRENV m2) = FinMap.eq CEnvEq (m1,m2)
	    and TyEnvEq(TYENV m1, TYENV m2) = FinMap.eq (fn ((tns1,e1),(tns2,e2)) => #4 pu_TyNames (tns1,tns2)
							 andalso CEnvEq(e1,e2)) (m1,m2)
	    fun fun_CENV (pu_CEnv,pu_StrEnv,pu_TyEnv) =
		convert (fn ((se,ve),(te,pe)) => CENV{StrEnv=se,VarEnv=ve,TyEnv=te,PathEnv=pe},
			 fn CENV{StrEnv=se,VarEnv=ve,TyEnv=te,PathEnv=pe} => ((se,ve),(te,pe)))
		(pairGen(pairGen(pu_StrEnv,pu_VarEnv),pairGen(pu_TyEnv,pu_PathEnv)))
	    fun fun_STRENV (pu_CEnv,pu_StrEnv,pu_TyEnv) =
		convert (STRENV,fn STRENV v => v)
		(FinMap.pu(StrId.pu,pu_CEnv))
	    fun fun_TYENV (pu_CEnv,pu_StrEnv,pu_TyEnv) =
		convert (TYENV,fn TYENV v => v)
		(FinMap.pu(TyCon.pu,pairGen(pu_TyNames,pu_CEnv)))
	in data3Gen(CEnvToInt,CEnvEq,[fun_CENV],
		    StrEnvToInt,StrEnvEq,[fun_STRENV],
		    TyEnvToInt,TyEnvEq,[fun_TYENV])
	end

  end

