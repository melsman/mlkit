(*$ModuleEnvironments:
	STRID SIGID FUNID TYCON STATOBJECT ENVIRONMENTS
	MODULE_STATOBJECT PRETTYPRINT REPORT FLAGS LIST_HACKS FINMAP 
        FINMAPEQ CRASH MODULE_ENVIRONMENTS IDENT*)

functor ModuleEnvironments(
	  structure StrId  : STRID
	  structure SigId  : SIGID
	  structure FunId  : FUNID
	  structure TyCon  : TYCON
	  structure Ident  : IDENT

	  structure StatObject : STATOBJECT

	  structure Environments : ENVIRONMENTS 
	    sharing type Environments.longstrid = StrId.longstrid
	    sharing type Environments.strid      = StrId.strid = TyCon.strid
		and type Environments.tycon      = TyCon.tycon
		and type Environments.longtycon      = TyCon.longtycon
		and type Environments.TypeScheme = StatObject.TypeScheme
		and type Environments.TyVar = StatObject.TyVar
	        and type Environments.Type = StatObject.Type
	        and type Environments.realisation = StatObject.realisation
		and type Environments.id = Ident.id
	    sharing Environments.TyName = StatObject.TyName

	  structure ModuleStatObject : MODULE_STATOBJECT
	    sharing type Environments.TypeFcn = StatObject.TypeFcn
	    sharing type ModuleStatObject.id = Environments.id
		and type ModuleStatObject.Env = Environments.Env
		and type ModuleStatObject.realisation = StatObject.realisation
		and type ModuleStatObject.TyVar = StatObject.TyVar
	    sharing Environments.TyName = ModuleStatObject.TyName


	  structure PP : PRETTYPRINT
	    sharing type ModuleStatObject.StringTree = PP.StringTree
	        and type Environments.StringTree = PP.StringTree

	  structure Report: REPORT
	  structure Flags : FLAGS
	  structure ListHacks: LIST_HACKS

	  structure FinMap : FINMAP
	    sharing type FinMap.StringTree = PP.StringTree
	        and type FinMap.Report = Report.Report

	  structure FinMapEq : FINMAPEQ
	    sharing type FinMapEq.StringTree = PP.StringTree
	        and type FinMapEq.Report = Report.Report

	  structure Crash : CRASH
          ) : MODULE_ENVIRONMENTS =
  struct
    fun die s = Crash.impossible ("ModuleEnvironments."^s)

    (*import from StatObject:*)
    type TyName            = StatObject.TyName
    type TyVar             = StatObject.TyVar
    structure TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
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
    type strid             = StrId.strid
    type Report            = Report.Report
    type StringTree        = PP.StringTree

    

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
	          implode ["signature ", SigId.pr_SigId sigid, " : "]

	    fun layoutPair (sigid, Sig) = 
	          PP.NODE {start=format_id sigid, finish="", indent=3,
			   children=[Sigma.layout Sig],
			   childsep=PP.NONE}
	    in 
	      (case l of
		[] => PP.LEAF ""		(* No signatures => no printout *)
	      | _ =>
		  PP.NODE {start="", finish="", indent=0, 
			   children=map layoutPair l, childsep=PP.RIGHT " "})
	    end
      fun report (report_sigid_Sigma : sigid * Sig -> Report, SIGENV m) =
	    FinMap.reportMapSORTED (SigId.<) report_sigid_Sigma m
    end (*G*)



    (*F, functor environments*)

    datatype FunEnv = FUNENV of (funid, FunSig) FinMap.map

    structure F = struct
      val empty = FUNENV FinMap.empty
      val singleton = FUNENV o FinMap.singleton
      fun plus (FUNENV F, FUNENV F') = FUNENV (FinMap.plus (F, F'))
      fun lookup (FUNENV F) = FinMap.lookup F
      fun tynames (FUNENV F) =
	    FinMap.fold
	      (fn (FunSig,T) =>
	       TyName.Set.union T (Phi.tynames FunSig))
		TyName.Set.empty  F
      fun tyvars (FUNENV F) =
	    FinMap.fold
	      (fn (Phi, tyvars) => TyVar.unionTyVarSet (Phi.tyvars Phi, tyvars))
	        [] F
      fun tyvars' (FUNENV F) =
	    FinMap.fold
	      (fn (Phi, criminals) => Phi.tyvars' Phi @ criminals) [] F
      fun dom (FUNENV F) = FinMap.dom F
      fun layout (FUNENV m) =
	    let val l = FinMap.Fold op :: nil m
	    fun format_id funid = implode ["functor ", FunId.pr_FunId funid, " : "]
	    fun layoutPair (funid, FunSig) = 
	          PP.NODE {start=format_id funid, finish="", indent=3,
			   children=[Phi.layout FunSig],
			   childsep=PP.NONE}
	    in
	      case l of
		[] => PP.LEAF ""		(* No functors => no printout *)
	      | _ =>
		  PP.NODE {start="", finish="", indent=0, 
			   children=map layoutPair l, childsep=PP.RIGHT " "}
	    end
      fun report (report_funid_Phi : funid * FunSig -> Report, FUNENV m) =
	    FinMap.reportMapSORTED (FunId.<) report_funid_Phi m
    end (*F*)



    (*B, static basis*)

    datatype Basis = BASIS of {F : FunEnv, G : SigEnv, E : Env}
	
    structure B = struct
      val empty = BASIS {F = F.empty, G = G.empty, E = E.empty}
      val initial = BASIS {F = F.empty, G = G.empty, E = E.initial}
      val bogus = empty
      fun plus (BASIS {F, G, E}, BASIS {F=F', G=G', E=E'}) =
	BASIS {F=F.plus (F, F'), G=G.plus (G, G'), E=E.plus (E, E')}

      (*It is assumed that phi does not affect F and G. This holds as an
       invariant throughout elaboration.*)
      fun on phi (B as (BASIS {F, G, E})) : Basis =
	    if Realisation.is_Id phi then B
	    else
	      BASIS {F = F, G = G, E = Realisation.on_Env phi E}
      fun tyvars (BASIS{F, G, E}) : TyVar list =
	    TyVar.unionTyVarSet (F.tyvars F, E.tyvars E)    (* no tyvars in G *)
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
		       of Some Sig1 => Sigma.eq(Sig1,Sig2)
			| None => false) true G2

      fun enrich_FunEnv(FUNENV F1,FUNENV F2) =
	FinMap.Fold (fn ((funid2,FunSig2),b) => b andalso
		     case FinMap.lookup F1 funid2 
		       of Some FunSig1 => Phi.eq(FunSig1,FunSig2)
			| None => false) true F2


      fun enrichB(BASIS{F=F1,G=G1,E=E1}, BASIS{F=F2,G=G2,E=E2}) = 
	enrich_FunEnv(F1,F2) andalso enrich_SigEnv(G1,G2) andalso
	enrich_Env(E1,E2)
      val enrich = enrichB


              (*Restriction relation for compilation manager*)


      (*TODO 27/01/1997 22:53. tho.  brug operationerne på F og G
       i s. f. at pille direkte ved deres repræsentation:*)
      fun restrictB (BASIS {F=FUNENV F,G=SIGENV G,E},
		     {ids : id list, tycons : tycon list, strids : strid list,
		      funids : funid list, sigids : sigid list}) =
	let val F' = List.foldL
	               (fn funid => fn Fnew =>
			let val FunSig = (case FinMap.lookup F funid of
					    Some FunSig => FunSig
					  | None => die "restrictB.funid not in basis.")
			in FinMap.add(funid,FunSig,Fnew)
			end) FinMap.empty funids 
	    val G' = List.foldL
	               (fn sigid => fn Gnew =>
			let val Sig = (case FinMap.lookup G sigid of
					 Some Sig => Sig
				       | None => die "restrictB.sigid not in basis.")
			in FinMap.add(sigid,Sig,Gnew)
			end) FinMap.empty sigids
	    val E' = E.restrict (E, (ids, tycons, strids))
	in BASIS {F=FUNENV F', G=SIGENV G', E=E'}
	end
      val enrich = enrichB
      val restrict = restrictB

      (*Matching function for compilation manager*)

      fun match (BASIS {F,G,E}, BASIS {F=F0,G=G0,E=E0}) = E.match (E,E0)
    end (*B*)

  end; (*functor ModuleEnvironments*)

