structure CompilerEnv: COMPILER_ENV =
  struct
    structure PP = PrettyPrint
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

		    | EXPORT    (* Support for exporting ML functions to be used in
				 * C programs. The function _export has type
				 * \/'a,'b. string * ('a -> 'b) -> unit. In a call
				 * _export("myFun",fn a:int => a+1)
				 * the string "myFun" is the name of the assembled
				 * function (following C calling conventions), and the
				 * function (fn a:int => a+1) is the ML function
				 * called when the C function "myFun" is called from C
				 * code. *)

    type spath = int list

    fun spath_lt (_ : spath, nil : spath) = false
      | spath_lt (nil,_) = true
      | spath_lt (x::xs,y::ys) = x < y orelse (x=y andalso spath_lt(xs,ys))

    structure PathEnv =
	OrderFinMap(struct
			type t = spath
			val lt = spath_lt
		    end)

    type PathEnv = (lvar*Type) PathEnv.map

    type VarEnv = result Ident.Map.map

    datatype CEnv = CENV of {StrEnv:StrEnv, VarEnv:VarEnv, TyEnv: TyEnv, PathEnv:PathEnv}
    and StrEnv  = STRENV of CEnv StrId.Map.map
    and TyEnv = TYENV of (TyName list * CEnv) TyCon.Map.map

    val emptyStrEnv   : StrEnv = STRENV StrId.Map.empty
    and emptyVarEnv   : VarEnv = Ident.Map.empty
    and emptyTyEnv    : TyEnv  = TYENV TyCon.Map.empty
    and emptyPathEnv  : PathEnv = PathEnv.empty
    val emptyCEnv = CENV {StrEnv=emptyStrEnv, VarEnv=emptyVarEnv,
			  TyEnv=emptyTyEnv, PathEnv=emptyPathEnv}

    fun initMap a = foldl (fn ((v,r), m) => TyCon.Map.add(v,r,m)) TyCon.Map.empty a
    fun initVE  a = foldl (fn ((v,r), m) => Ident.Map.add(v,r,m)) Ident.Map.empty a
    val initialStrEnv = STRENV(StrId.Map.singleton(StrId.mk_StrId "IntInfRep", emptyCEnv))

    val boolType = LambdaExp.boolType
    val exnType = LambdaExp.exnType
    val tyvar_nil = LambdaExp.fresh_eqtyvar()
    val nilType = LambdaExp.CONStype([LambdaExp.TYVARtype tyvar_nil], TyName.tyName_LIST, NONE)
    val tyvar_cons = LambdaExp.fresh_eqtyvar()
    val consType =
      let open LambdaExp
	  val t = CONStype([TYVARtype tyvar_cons], TyName.tyName_LIST, NONE)
      in ARROWtype([RECORDtype ([TYVARtype tyvar_cons, t],NONE)],NONE,[t],NONE)
      end

    val tyvar_quote = LambdaExp.fresh_eqtyvar()
    val quoteType =
      let open LambdaExp
	  val t = CONStype([TYVARtype tyvar_quote], TyName.tyName_FRAG, NONE)
      in ARROWtype([CONStype([],TyName.tyName_STRING,NONE)],NONE,[t],NONE)
      end

    val tyvar_antiquote = LambdaExp.fresh_eqtyvar()
    val antiquoteType =
      let open LambdaExp
	  val t = CONStype([TYVARtype tyvar_antiquote], TyName.tyName_FRAG, NONE)
      in ARROWtype([TYVARtype tyvar_antiquote],NONE,[t],NONE)
      end

    val intinfType =
      let open LambdaExp
	  val t = CONStype([], TyName.tyName_INTINF, NONE)
	  val int31 = CONStype([],TyName.tyName_INT31, NONE)
	  val int31list = CONStype([int31],TyName.tyName_LIST,NONE)
	  val bool = CONStype([],TyName.tyName_BOOL,NONE)
	  val record = RECORDtype ([int31list,bool],NONE)
      in ARROWtype([record],NONE,[t],NONE)
      end

    val boolVE =
        initVE [(Ident.id_TRUE, CON(Con.con_TRUE,[],boolType,[])),
	        (Ident.id_FALSE, CON(Con.con_FALSE,[],boolType,[]))]

    val listVE =
        initVE [(Ident.id_NIL, CON(Con.con_NIL,[tyvar_nil],nilType,
				   [LambdaExp.TYVARtype tyvar_nil])),
	        (Ident.id_CONS, CON(Con.con_CONS,[tyvar_cons],consType,
				    [LambdaExp.TYVARtype tyvar_cons]))]

    val fragVE =
        initVE [(Ident.id_QUOTE, CON(Con.con_QUOTE,[tyvar_quote],quoteType,
				     [LambdaExp.TYVARtype tyvar_quote])),
	        (Ident.id_ANTIQUOTE, CON(Con.con_ANTIQUOTE,[tyvar_antiquote],antiquoteType,
					 [LambdaExp.TYVARtype tyvar_antiquote]))]

    val initialVarEnv : VarEnv =
        Ident.Map.plus(Ident.Map.plus(Ident.Map.plus(boolVE,listVE),fragVE),
         initVE [(Ident.id_PRIM, PRIM),
	         (Ident.id_EXPORT, EXPORT),
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
(*
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
*)
	         (Ident.id_INTINF, CON(Con.con_INTINF,[],intinfType, [])),
	         (Ident.id_Div, EXCON(Excon.ex_DIV, exnType)),
	         (Ident.id_Match, EXCON(Excon.ex_MATCH, exnType)),
	         (Ident.id_Bind, EXCON(Excon.ex_BIND, exnType)),
	         (Ident.id_Overflow, EXCON(Excon.ex_OVERFLOW, exnType)),
	         (Ident.id_Interrupt, EXCON(Excon.ex_INTERRUPT, exnType)),
	         (Ident.id_Subscript, EXCON(Excon.ex_SUBSCRIPT, exnType)),
	         (Ident.id_Size, EXCON(Excon.ex_SIZE, exnType))
	        ])
    local
      fun fromVarEnv ve =
          CENV {StrEnv=emptyStrEnv, VarEnv=ve, TyEnv=emptyTyEnv, PathEnv=emptyPathEnv}

      open TyCon TyName
      fun initialTyEnv() : TyEnv =
	  TYENV (initMap [(tycon_INT31, ([tyName_INT31], emptyCEnv)),
			  (tycon_INT32, ([tyName_INT32], emptyCEnv)),
                          (tycon_INT63, ([tyName_INT63], emptyCEnv)),
			  (tycon_INT64, ([tyName_INT64], emptyCEnv)),
			  (tycon_INT, ([tyName_IntDefault()], emptyCEnv)),
			  (tycon_WORD8, ([tyName_WORD8], emptyCEnv)),
			  (tycon_WORD31, ([tyName_WORD31], emptyCEnv)),
			  (tycon_WORD32, ([tyName_WORD32], emptyCEnv)),
			  (tycon_WORD63, ([tyName_WORD63], emptyCEnv)),
			  (tycon_WORD64, ([tyName_WORD64], emptyCEnv)),
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
			  (tycon_BOOL, ([tyName_BOOL], fromVarEnv boolVE)),
			  (tycon_LIST, ([tyName_LIST], fromVarEnv listVE)),
			  (tycon_FRAG, ([tyName_FRAG], fromVarEnv fragVE)),
			  (tycon_INTINF, ([tyName_INTINF], emptyCEnv)),
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

    fun declareVar (id, (lv, tyvars, tau), CENV{StrEnv,VarEnv=m,TyEnv,PathEnv}) =
      let val il0 = map LambdaExp.TYVARtype tyvars
      in CENV{StrEnv=StrEnv, TyEnv=TyEnv,
	      VarEnv=Ident.Map.add(id, LVAR (lv,tyvars,tau,il0), m),
	      PathEnv=PathEnv}
      end

    fun declareCon (id, (con,tyvars,tau), CENV{StrEnv,VarEnv=m,TyEnv,PathEnv}) =
      let val il0 = map LambdaExp.TYVARtype tyvars
      in CENV{StrEnv=StrEnv, TyEnv=TyEnv,
	      VarEnv=Ident.Map.add(id,CON (con,tyvars,tau,il0), m),
	      PathEnv=PathEnv}
      end

    fun declareExcon (id, excon, CENV{StrEnv,VarEnv=map,TyEnv,PathEnv}) =
      CENV{StrEnv=StrEnv,VarEnv=Ident.Map.add(id,EXCON excon,map),TyEnv=TyEnv,
	   PathEnv=PathEnv}

    fun declare_strid (strid, env, CENV{StrEnv=STRENV m,VarEnv,TyEnv,PathEnv}) =
      CENV{StrEnv=STRENV (StrId.Map.add(strid,env,m)),VarEnv=VarEnv,TyEnv=TyEnv,
	   PathEnv=PathEnv}

    fun declare_tycon (tycon, a, CENV{StrEnv,VarEnv,TyEnv=TYENV m,PathEnv}) =
      CENV{StrEnv=StrEnv,VarEnv=VarEnv,TyEnv=TYENV(TyCon.Map.add(tycon,a,m)),PathEnv=PathEnv}

    fun plus (CENV{StrEnv,VarEnv,TyEnv,PathEnv},
	      CENV{StrEnv=StrEnv',VarEnv=VarEnv',TyEnv=TyEnv',PathEnv=PathEnv'}) =
      CENV{StrEnv=plusStrEnv(StrEnv,StrEnv'),
	   VarEnv=plusVarEnv(VarEnv,VarEnv'),
	   TyEnv=plusTyEnv(TyEnv,TyEnv'),
	   PathEnv=PathEnv.plus(PathEnv,PathEnv')}

    and plusStrEnv (STRENV m1, STRENV m2) : StrEnv = STRENV(StrId.Map.plus(m1,m2))
    and plusVarEnv (m1: VarEnv, m2: VarEnv) : VarEnv =
       ((*if Compiler.Profile.getTimingMode() then
             let fun size m = List.length(Ident.Map.list m)
             in
                 TextIO.output(TextIO.stdOut, concat["plus(",Int.toString(size m1), ", ",
                                                             Int.toString(size m2),")\n"])
             end
        else ();*)
        Ident.Map.plus(m1,m2)
       )
    and plusTyEnv  (TYENV m1,  TYENV m2)  : TyEnv  = TYENV(TyCon.Map.plus(m1,m2))

    exception LOOKUP_ID
    fun lookupId (CENV{VarEnv=m,...}) id =
      case Ident.Map.lookup m id
	of SOME res => res
	 | NONE => raise LOOKUP_ID

    fun lookup_strid (CENV{StrEnv=STRENV m,...}) strid =
      case StrId.Map.lookup m strid
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
      case TyCon.Map.lookup m tycon
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

    fun tynames_tau (LambdaExp.CONStype(taus,t,_), tns) = tynames_taus(taus,t::tns)
      | tynames_tau (LambdaExp.ARROWtype(taus1,_,taus2,_),tns) = tynames_taus(taus1,tynames_taus(taus2,tns))
      | tynames_tau (LambdaExp.RECORDtype(taus,_), tns) = tynames_taus(taus,tns)
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
	    Ident.Map.fold vars_result (vars_se (StrEnv,vars)) m
	  and vars_se (STRENV m, vars) = StrId.Map.fold vars_ce vars m
      in vars_ce (ce, [])
      end

    val lvarsOfCEnv = varsOfCEnv lvars_result
    val consOfCEnv = varsOfCEnv cons_result
    val exconsOfCEnv = varsOfCEnv excons_result

    fun tynamesOfCEnv ce : TyName list =
      let fun tynames_TEentry((tns,ce),acc) = tynames_E(ce,tns@acc)
          and tynames_TE(TYENV m, acc) = TyCon.Map.fold tynames_TEentry acc m
          and tynames_E(CENV{VarEnv=ve, StrEnv, TyEnv, ...}, acc) =
	    let val acc = tynames_SE (StrEnv,acc)
	        val acc = tynames_TE (TyEnv,acc)
	    in Ident.Map.fold tynames_result acc ve
	    end
	  and tynames_SE(STRENV m, acc) = StrId.Map.fold tynames_E acc m
      in (TyName.Set.list o TyName.Set.fromList) (tynames_E(ce,[]))
      end

   (* -------------
    * Restriction
    * ------------- *)

   fun restrictTyConMap (error_str, env : 'b TyCon.Map.map, dom) =
     foldl (fn (id, acc) =>
	    let val res = case TyCon.Map.lookup env id
			    of SOME res => res
			     | NONE => die (error_str id)
	    in TyCon.Map.add(id,res,acc)
	    end) TyCon.Map.empty dom

   fun restrictStrIdMap (error_str, env : 'b StrId.Map.map, dom) =
     foldl (fn (id, acc) =>
	    let val res = case StrId.Map.lookup env id
			    of SOME res => res
			     | NONE => die (error_str id)
	    in StrId.Map.add(id,res,acc)
	    end) StrId.Map.empty dom

   fun restrictVarEnv (m: VarEnv, ids) : VarEnv =
     foldl (fn (id, acc) =>
	    let val res = case Ident.Map.lookup m id
			    of SOME res => res
			     | NONE => die ("restrictCEnv.id not in env" (*  ^ id*))
	    in Ident.Map.add(id,res,acc)
	    end) Ident.Map.empty ids


   fun restrictTyEnv (TYENV m, tycons) : TyEnv =
       TYENV (restrictTyConMap(fn tc => ("restrictCEnv.tycon " ^ TyCon.pr_TyCon tc ^ " not in env"), m, tycons))

   fun restrictStrEnv (STRENV m, strid_restrs) : StrEnv =
       STRENV (foldl (fn ((strid,restr:Environments.restrictor), acc) =>
		      let val res = case StrId.Map.lookup m strid of
			  SOME res => restrictCEnv(res,restr)
			| NONE => die "restrictStrEnv.strid not in env"
		      in StrId.Map.add(strid,res,acc)
		      end) StrId.Map.empty strid_restrs)

   and restrictCEnv (ce,Environments.Whole) = ce
     | restrictCEnv (CENV{StrEnv,VarEnv,TyEnv,PathEnv}, Environments.Restr{strids,vids,tycons}) =
     CENV{StrEnv=restrictStrEnv(StrEnv,strids),
	  VarEnv=restrictVarEnv(VarEnv,vids),
	  TyEnv=restrictTyEnv(TyEnv,tycons),
	  PathEnv=emptyPathEnv}

   val restrictCEnv = fn (ce, longids) => restrictCEnv(ce, Environments.create_restrictor longids)


   (* -------------
    * Enrichment
    * ------------- *)

   local

    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

     fun log s = TextIO.output(TextIO.stdOut,s)
     fun debug (s, b) = if !debug_man_enrich then
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
       | eq_res (EXPORT,EXPORT) = true
       | eq_res _ = false

     fun enrichVarEnv (env1: VarEnv, env2: VarEnv) : bool =
       Ident.Map.Fold (fn ((id2,res2),b) => b andalso
		    case Ident.Map.lookup env1 id2
		      of SOME res1 => eq_res(res1,res2)
		       | NONE => false) true env2

     fun eq_tynames (res1,res2) = TyName.Set.eq (TyName.Set.fromList res1) (TyName.Set.fromList res2)

     fun enrichTyEnv (TYENV m1, TYENV m2) : bool =
       TyCon.Map.Fold (fn ((id2,(res2,ce2)),b) => b andalso
		    case TyCon.Map.lookup m1 id2
		      of SOME (res1,ce1) => eq_tynames(res1,res2) andalso eqCEnv(ce1,ce2)
		       | NONE => false) true m2

     and enrichCEnv (CENV{StrEnv,VarEnv,TyEnv,PathEnv},
		     CENV{StrEnv=StrEnv',VarEnv=VarEnv',TyEnv=TyEnv',
			  PathEnv=PathEnv'}) =
       debug("StrEnv", enrichStrEnv(StrEnv,StrEnv')) andalso
       debug("VarEnv", enrichVarEnv(VarEnv,VarEnv')) andalso
       debug("TyEnv", enrichTyEnv(TyEnv,TyEnv')) andalso
       PathEnv.isEmpty PathEnv andalso PathEnv.isEmpty PathEnv'

     and eqCEnv (ce1,ce2) = enrichCEnv(ce1,ce2) andalso enrichCEnv(ce2,ce1)

     and enrichStrEnv (STRENV se1, STRENV se2) =
       StrId.Map.Fold (fn ((strid,env2),b) => b andalso
		    case StrId.Map.lookup se1 strid
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

     fun matchVarEnv (env: VarEnv, env0: VarEnv) =
       Ident.Map.Fold(fn ((id,res),_) =>
		   case Ident.Map.lookup env0 id
		     of SOME res0 => matchRes(res,res0)
		      | NONE => ()) () env

     fun matchEnv (CENV{StrEnv,VarEnv, ...},
		   CENV{StrEnv=StrEnv0,VarEnv=VarEnv0, ...}) =
         (matchStrEnv(StrEnv,StrEnv0); matchVarEnv(VarEnv,VarEnv0))

     and matchStrEnv (STRENV se, STRENV se0) =
       StrId.Map.Fold(fn ((strid,env),_) =>
		   case StrId.Map.lookup se0 strid
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
	   let val (elabSE, elabTE, elabVE, elabR) = E.un elabE
	   in CENV{StrEnv=constr_se(StrEnv,elabSE), VarEnv=constr_ve(VarEnv,elabVE),
		   TyEnv=constr_te(TyEnv,elabTE),
		   PathEnv=emptyPathEnv}
	   end

	 and constr_se(STRENV se, elabSE) =
	     STRENV(SE.Fold (fn (strid, elabE) => fn se' =>
			     case StrId.Map.lookup se strid
				 of SOME ce => let val ce' = constr_ce(ce,elabE)
					       in StrId.Map.add(strid,ce',se')
					       end
			       | NONE => die "constr_se") StrId.Map.empty elabSE)

	 and constr_ve(ve, elabVE) =
	   VE.Fold (fn (id, elabRan) => fn ve' =>
		    case Ident.Map.lookup ve id
		      of SOME transRan => let val transRan' = constr_ran(transRan, elabRan)
					  in Ident.Map.add(id,transRan', ve')
					  end
		       | NONE => die ("constr_ve: no " ^ Ident.pr_id id)) Ident.Map.empty elabVE

	 and constr_te(TYENV te, elabTE) =
	     TYENV(TE.Fold(fn (tycon, _) => fn te' =>
			   case TyCon.Map.lookup te tycon
			       of SOME tynames => TyCon.Map.add(tycon,tynames,te')
			     | NONE => die "constr_te") TyCon.Map.empty elabTE)

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

   type StringTree = PP.StringTree

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
	      children= [StrId.Map.layoutMap {start="{", eq=" -> ", sep=", ", finish="}"}
			 (PP.layoutAtom StrId.pr_StrId)
			 (layoutCEnv) m],
	      childsep=PP.RIGHT ","}

    and layoutVarEnv (m : VarEnv) =
      PP.NODE{start="VarEnv = ",finish="",indent=2,
	      children= [Ident.Map.layoutMap {start="{", eq=" -> ", sep=", ", finish="}"}
			               (PP.layoutAtom Ident.pr_id)
				       (PP.layoutAtom
					(fn LVAR (lv,tyvars,Type,il) => ("LVAR(" ^ Lvars.pr_lvar lv ^ ", " ^
					                                 pr_scheme (tyvars,Type) ^ ", " ^
									 pr_il il ^ ")")
                                          | RESET_REGIONS => Ident.pr_id Ident.resetRegions
                                          | FORCE_RESET_REGIONS => Ident.pr_id Ident.forceResetting
					  | PRIM => "PRIM"
					  | EXPORT => "EXPORT"
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
      in TyCon.Map.layoutMap {start="TyEnv = {", eq=" -> ", sep = ", ", finish="}"} (PP.LEAF o TyCon.pr_TyCon)
	 layout_res m
      end

    val pu_result =
	let fun toInt (LVAR _) = 0
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
	      | toInt EXPORT = 18

	    fun fun_LVAR _ =
		Pickle.con1 LVAR (fn LVAR lv => lv | _ => die "pu.LVAR")
		(Pickle.tup4Gen(Lvars.pu, LambdaExp.pu_tyvars,
				LambdaExp.pu_Type, LambdaExp.pu_Types))
	    fun fun_CON _ =
		Pickle.con1 CON (fn CON lv => lv | _ => die "pu.CON")
		(Pickle.tup4Gen(Con.pu, LambdaExp.pu_tyvars,
				LambdaExp.pu_Type, LambdaExp.pu_Types))
	    fun fun_EXCON _ =
		Pickle.con1 EXCON (fn EXCON lv => lv | _ => die "pu.CON")
		(Pickle.pairGen(Excon.pu,LambdaExp.pu_Type))
	in Pickle.dataGen("CompilerEnv.result",toInt,
			  [fun_LVAR,
			   fun_CON,
			   Pickle.con0 REF,
			   fun_EXCON,
			   Pickle.con0 ABS,
			   Pickle.con0 NEG,
			   Pickle.con0 PLUS,
			   Pickle.con0 MINUS,
			   Pickle.con0 MUL,
			   Pickle.con0 DIV,
			   Pickle.con0 MOD,
			   Pickle.con0 LESS,
			   Pickle.con0 GREATER,
			   Pickle.con0 LESSEQ,
			   Pickle.con0 GREATEREQ,
			   Pickle.con0 RESET_REGIONS,
			   Pickle.con0 FORCE_RESET_REGIONS,
			   Pickle.con0 PRIM,
			   Pickle.con0 EXPORT])
	end

    val pu_PathEnv =
	PathEnv.pu (Pickle.listGen Pickle.int) (Pickle.pairGen(Lvars.pu,LambdaExp.pu_Type))
    val pu_VarEnv= Ident.Map.pu Ident.pu pu_result
    val (pu,_,_) =
	let val pu_TyNames = Pickle.listGen TyName.pu
	    fun CEnvToInt _ = 0
	    fun StrEnvToInt _ = 0
	    fun TyEnvToInt _ = 0
	    fun fun_CENV (pu_CEnv,pu_StrEnv,pu_TyEnv) =
		Pickle.con1 (fn ((se,ve),(te,pe)) => CENV{StrEnv=se,VarEnv=ve,TyEnv=te,PathEnv=pe})
		(fn CENV{StrEnv=se,VarEnv=ve,TyEnv=te,PathEnv=pe} => ((se,ve),(te,pe)))
		(Pickle.pairGen0(Pickle.pairGen0(pu_StrEnv,pu_VarEnv),Pickle.pairGen0(pu_TyEnv,pu_PathEnv)))
	    fun fun_STRENV (pu_CEnv,pu_StrEnv,pu_TyEnv) =
		Pickle.con1 STRENV (fn STRENV v => v)
		(StrId.Map.pu StrId.pu pu_CEnv)
	    fun fun_TYENV (pu_CEnv,pu_StrEnv,pu_TyEnv) =
		Pickle.con1 TYENV (fn TYENV v => v)
		(TyCon.Map.pu TyCon.pu (Pickle.pairGen(pu_TyNames,pu_CEnv)))
	in Pickle.data3Gen("CompilerEnv.CEnv",CEnvToInt,[fun_CENV],
			   "CompilerEnv.StrEnv",StrEnvToInt,[fun_STRENV],
			   "CompilerEnv.TyEnv",TyEnvToInt,[fun_TYENV])
	end

  end
