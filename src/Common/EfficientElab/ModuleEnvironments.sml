
structure ModuleEnvironments: MODULE_ENVIRONMENTS =
  struct
    structure PP = PrettyPrint
    fun die s = Crash.impossible ("ModuleEnvironments."^s)

    (*import from StatObject:*)
    type TyName            = StatObject.TyName
    type TyVar             = StatObject.TyVar
    structure TyVar        = StatObject.TyVar
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    type realisation       = StatObject.realisation

    (*import from Environments:*)
    type VarEnv            = Environments.VarEnv
    type TyStr             = Environments.TyStr
    type TyEnv             = Environments.TyEnv
    type StrEnv            = Environments.StrEnv
    type Env               = Environments.Env
    type Context           = Environments.Context
    type constructor_map   = Environments.constructor_map
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C
    structure Realisation  = Environments.Realisation

    (*import from ModuleStatObject:*)
    type Sig               = ModuleStatObject.Sig
    type FunSig            = ModuleStatObject.FunSig
    exception No_match     = ModuleStatObject.No_match
    (*may be raised by Sigma.match and Phi.match*)
    structure Sigma        = ModuleStatObject.Sigma
    structure Phi          = ModuleStatObject.Phi

    (*import from other modules:*)
    type sigid             = SigId.sigid
    type funid             = FunId.funid
    type longstrid         = StrId.longstrid
    type longtycon         = TyCon.longtycon
    type tycon             = TyCon.tycon
    type id                = Ident.id
    type longid            = Ident.longid
    type strid             = StrId.strid
    type Report            = Report.Report
    type StringTree        = PP.StringTree

    type longids = {longvids : longid list, longtycons : longtycon list, longstrids : longstrid list,
		    funids : funid list, sigids : sigid list}

    (*G, signature environments*)

    datatype SigEnv = SIGENV of (sigid, Sig) FinMap.map

    structure G = struct
      val empty = SIGENV FinMap.empty
      val singleton = SIGENV o FinMap.singleton
      fun plus (SIGENV G, SIGENV G') = SIGENV (FinMap.plus (G, G'))
      fun lookup (SIGENV G) = FinMap.lookup G
      fun tynames (SIGENV G) =
            FinMap.fold
	      (fn (Sig, T) =>
	            TyName.Set.union T (Sigma.tynames Sig))
	        TyName.Set.empty  G
      fun dom (SIGENV G) = FinMap.dom G
      fun layout (SIGENV m) =
	    let val l = FinMap.Fold (op ::) nil m

	    fun format_id sigid =
	          concat ["signature ", SigId.pr_SigId sigid, " : "]

	    fun layoutPair (sigid, Sig) =
	          PP.NODE {start=format_id sigid, finish="", indent=3,
			   children=[Sigma.layout Sig],
			   childsep=PP.NOSEP}
	    in
	      (case l of
		[] => PP.LEAF ""		(* No signatures => no printout *)
	      | _ =>
		  PP.NODE {start="", finish="", indent=0,
			   children=map layoutPair l, childsep=PP.RIGHT " "})
	    end
      fun report (report_sigid_Sigma : sigid * Sig -> Report, SIGENV m) =
	    FinMap.reportMapSORTED (SigId.<) report_sigid_Sigma m

      fun on rea (G as SIGENV m) =
	if Realisation.is_Id rea then G
	else SIGENV (FinMap.composemap (fn Sigma => Sigma.on (rea,Sigma)) m)

      val pu = Pickle.convert (SIGENV, fn SIGENV m => m)
	  (FinMap.pu (SigId.pu,Sigma.pu))

    end (*G*)



    (*F, functor environments*)

    type absprjid = string
    fun lt_absprjid (a,b:string) = a < b

    datatype FunEnv = FUNENV of (funid, absprjid*FunSig) FinMap.map
    fun absprjid_to_string(absprjid) = absprjid
    fun mk_absprjid x = x

    fun is_absprjid_basislib absprjid =
      OS.Path.file absprjid = "basis.pm"

    fun strip_install_dir absprjid =
      if is_absprjid_basislib absprjid then OS.Path.mkRelative{path=absprjid, relativeTo= !Flags.install_dir}
      else absprjid

    fun strip_install_dir' (p as (absprjid, funid)) =
      if is_absprjid_basislib absprjid then
	(OS.Path.mkRelative{path=absprjid, relativeTo= !Flags.install_dir},
	 FunId.mk_FunId (OS.Path.mkRelative{path=FunId.pr_FunId funid, relativeTo= !Flags.install_dir}))
      else p

    val pu_absprjid = Pickle.string


    structure F = struct
      val empty = FUNENV FinMap.empty
      val singleton = FUNENV o FinMap.singleton
      fun plus (FUNENV F, FUNENV F') = FUNENV (FinMap.plus (F, F'))
      fun lookup (FUNENV F) = FinMap.lookup F
      fun tynames (FUNENV F) =
	    FinMap.fold
	      (fn ((_,FunSig),T) =>
	       TyName.Set.union T (Phi.tynames FunSig))
		TyName.Set.empty  F
      fun tyvars (FUNENV F) =
	    FinMap.fold
	      (fn ((_,Phi), tyvars) => TyVar.unionTyVarSet (Phi.tyvars Phi, tyvars))
	        [] F
      fun tyvars' (FUNENV F) =
	    FinMap.fold
	      (fn ((_,Phi), criminals) => Phi.tyvars' Phi @ criminals) [] F
      fun dom (FUNENV F) = FinMap.dom F
      fun layout (FUNENV m) =
	    let val l = FinMap.Fold op :: nil m
	    fun format_id funid = concat ["functor ", FunId.pr_FunId funid, " : "]
	    fun layoutPair (funid, (_,FunSig)) =
	          PP.NODE {start=format_id funid, finish="", indent=3,
			   children=[Phi.layout FunSig],
			   childsep=PP.NOSEP}
	    in
	      case l of
		[] => PP.LEAF ""		(* No functors => no printout *)
	      | _ =>
		  PP.NODE {start="", finish="", indent=0,
			   children=map layoutPair l, childsep=PP.RIGHT " "}
	    end
      fun report (report_funid_Phi : funid * FunSig -> Report, FUNENV m) =
	let fun report_funid_Phi' (funid,(_,FunSig)) = report_funid_Phi(funid,FunSig)
	in FinMap.reportMapSORTED (FunId.<) report_funid_Phi' m
	end

      fun on rea (F as FUNENV m) =
	if Realisation.is_Id rea then F
	else FUNENV (FinMap.composemap (fn (absprjid, Phi) => (absprjid, Phi.on (rea, Phi))) m)

      val pu = Pickle.convert (FUNENV, fn FUNENV m => m)
	  (FinMap.pu (FunId.pu, Pickle.pairGen(Pickle.string,Phi.pu)))

    end (*F*)



    (*B, static basis*)

    datatype Basis = BASIS of {F : FunEnv, G : SigEnv, E : Env}

    structure B = struct
      val empty = BASIS {F = F.empty, G = G.empty, E = E.empty}
      fun initial() = BASIS {F = F.empty, G = G.empty, E = E.initial()}
      val bogus = empty
      fun plus (BASIS {F, G, E}, BASIS {F=F', G=G', E=E'}) =
	BASIS {F=F.plus (F, F'), G=G.plus (G, G'), E=E.plus (E, E')}

      fun on phi (B as (BASIS {F, G, E})) : Basis =
	    if Realisation.is_Id phi then B
	    else BASIS {F = F.on phi F, G = G.on phi G, E = Realisation.on_Env phi E}

      fun tyvars (BASIS{F, G, E}) : TyVar list =
	    TyVar.unionTyVarSet (F.tyvars F, E.tyvars E)    (* no tyvars in G *)
      fun tyvars' (BASIS{F,G,E}) : (id * TyVar list) list = F.tyvars' F @ E.tyvars' E

      fun tynames (BASIS{F, G, E}) =
            TyName.Set.union (F.tynames F)
	    (TyName.Set.union (G.tynames G) (E.tynames E))
      fun to_C (BASIS {F, G, E}) = C.from_E E
      fun layout (BASIS {F, G, E}) =
	    PP.NODE {start="", finish="", indent = 0,
		     children = [F.layout F, G.layout G, E.layout E],
		     childsep = PP.RIGHT " "}

  (*E component*)
      fun plus_E (BASIS {F, G, E}, E') = BASIS {F=F, G=G, E=E.plus (E, E')}
      fun from_E E = BASIS {F = F.empty, G = G.empty, E = E}
      fun to_E (BASIS {E, ...}) = E
      val lookup_strid = E.lookup_strid o to_E
      val lookup_longstrid = E.lookup_longstrid o to_E
      val lookup_longtycon = E.lookup_longtycon o to_E

  (*G component*)
      fun from_G G = BASIS {F = F.empty, G = G, E = E.empty}
      fun to_G (BASIS {G, ...}) = G
      fun plus_G (BASIS {F, G, E}, G') = BASIS {F = F, G = G.plus (G, G'), E = E}
      val lookup_sigid = G.lookup o to_G

  (*F component*)
      fun from_F F = BASIS {F = F, G = G.empty, E = E.empty}
      fun to_F (BASIS{F, ...}) = F
      fun plus_F (BASIS {F, G, E}, F') = BASIS {F = F.plus (F, F'), G = G, E = E}
      val lookup_funid = F.lookup o to_F


              (*Enrichment relation for compilation manager*)

      fun enrich_Env a = E.enrich a

      fun enrich_SigEnv(SIGENV G1,SIGENV G2) =
	FinMap.Fold (fn ((sigid2,Sig2), b) => b andalso
		     case FinMap.lookup G1 sigid2
		       of SOME Sig1 => Sigma.eq(Sig1,Sig2)
			| NONE => false) true G2

      fun enrich_FunEnv(FUNENV F1,FUNENV F2) =
	FinMap.Fold (fn ((funid2,(absprjid2,FunSig2)),b) => b andalso
		     case FinMap.lookup F1 funid2
		       of SOME (absprjid1,FunSig1) => absprjid1 = absprjid2 andalso Phi.eq(FunSig1,FunSig2)
			| NONE => false) true F2


      fun enrichB(BASIS{F=F1,G=G1,E=E1}, BASIS{F=F2,G=G2,E=E2}) =
	enrich_FunEnv(F1,F2) andalso enrich_SigEnv(G1,G2) andalso
	enrich_Env(E1,E2)
      val enrich = enrichB


              (*Restriction relation for compilation manager*)

	(* MEMO: use operations on F and G instead *)
      fun restrictB restrictE (BASIS {F=FUNENV F,G=SIGENV G,E},
		     {longvids : longid list, longtycons : longtycon list, longstrids : longstrid list,
		      funids : funid list, sigids : sigid list}:longids) =
	let val F' = foldl
	               (fn (funid, Fnew) =>
			let val FunSig = (case FinMap.lookup F funid of
					    SOME FunSig => FunSig
					  | NONE => die ("restrictB.funid " ^ FunId.pr_FunId funid ^ " not in basis."))
			in FinMap.add(funid,FunSig,Fnew)
			end) FinMap.empty funids
	    val G' = foldl
	               (fn (sigid, Gnew) =>
			let val Sig = (case FinMap.lookup G sigid of
					 SOME Sig => Sig
				       | NONE => die ("restrictB.sigid " ^ SigId.pr_SigId sigid ^ " not in basis."))
			in FinMap.add(sigid,Sig,Gnew)
			end) FinMap.empty sigids
	    val E' = restrictE (E, {longvids=longvids, longtycons=longtycons, longstrids=longstrids})
	in BASIS {F=FUNENV F', G=SIGENV G', E=E'}
	end
      val enrich = enrichB
      fun restrict p = restrictB E.restrict p

      fun domain(BASIS{F=FUNENV F,G=SIGENV G,E}) : longids =
	  let val (SE,TE,VE,_) = E.un E
	      val strids = EqSet.list (SE.dom SE)
	      val longstrids = map StrId.longStrIdOfStrId strids
	      val vids = EqSet.list (VE.dom VE)
	      val longvids = map Ident.idToLongId vids
	      val tycons = EqSet.list (TE.dom TE)
	      val longtycons = map (fn t => TyCon.implode_LongTyCon([],t)) tycons
	  in {longvids=longvids,longtycons=longtycons,longstrids=longstrids,
	      funids=EqSet.list(FinMap.dom F), sigids=EqSet.list(FinMap.dom G)}
	  end

      (* Structure agreement *)
      fun agree([],_,_) = true
	| agree(longstrid::longstrids,B1,B2) =
	case (lookup_longstrid B1 longstrid,
	      lookup_longstrid B2 longstrid)
	  of (SOME E1, SOME E2) => Environments.E.eq(E1,E2) andalso agree(longstrids,B1,B2)
	   | _ => false

      (*Matching function for compilation manager*)

      fun match (BASIS {F,G,E}, BASIS {F=F0,G=G0,E=E0}) = E.match (E,E0)

      val pu =
	  let fun to (F,G,E) = BASIS{F=F,G=G,E=E}
	      fun from (BASIS{F=F,G=G,E=E}) = (F,G,E)
	  in Pickle.convert (to,from)
	      (Pickle.tup3Gen0(F.pu,G.pu,E.pu))
	  end

    end (*B*)

  end; (*functor ModuleEnvironments*)
