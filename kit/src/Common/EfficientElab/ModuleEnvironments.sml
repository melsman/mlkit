
functor ModuleEnvironments(
	  structure StrId  : STRID
	  structure SigId  : SIGID
	  structure FunId  : FUNID
	  structure TyCon  : TYCON
	  structure Ident  : IDENT

	  structure StatObject : STATOBJECT

	  structure Environments : ENVIRONMENTS 
	    sharing Environments.TyName = StatObject.TyName
	    sharing type Environments.longstrid = StrId.longstrid
	    sharing type Environments.strid      = StrId.strid = TyCon.strid
	    sharing type Environments.tycon      = TyCon.tycon
	    sharing type Environments.longtycon      = TyCon.longtycon
	    sharing type Environments.TypeScheme = StatObject.TypeScheme
	    sharing type Environments.TyVar = StatObject.TyVar
	    sharing type Environments.Type = StatObject.Type
	    sharing type Environments.realisation = StatObject.realisation
	    sharing type Environments.id = Ident.id
	    sharing type Environments.longid = Ident.longid

	  structure ModuleStatObject : MODULE_STATOBJECT
	    sharing Environments.TyName = ModuleStatObject.TyName
	    sharing type Environments.TypeFcn = StatObject.TypeFcn
	    sharing type ModuleStatObject.id = Environments.id
	    sharing type ModuleStatObject.Env = Environments.Env
	    sharing type ModuleStatObject.realisation = StatObject.realisation
	    sharing type ModuleStatObject.TyVar = StatObject.TyVar

	  structure PP : PRETTYPRINT
	    sharing type ModuleStatObject.StringTree = PP.StringTree

	  structure Report: REPORT
	  structure Flags : FLAGS

	  structure FinMap : FINMAP
	    sharing type FinMap.StringTree = PP.StringTree
	    sharing type FinMap.Report = Report.Report

	  structure FinMapEq : FINMAPEQ
	    sharing type FinMapEq.StringTree = PP.StringTree
	    sharing type FinMapEq.Report = Report.Report

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
    type longid            = Ident.longid
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

    end (*G*)



    (*F, functor environments*)

    type prjid = string

    datatype FunEnv = FUNENV of (funid, prjid*FunSig) FinMap.map
    type prjid = string
    fun prjid_to_string(prjid) = prjid
    fun mk_prjid x = x 

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
	else FUNENV (FinMap.composemap (fn (prjid, Phi) => (prjid, Phi.on (rea, Phi))) m)

    end (*F*)



    (*B, static basis*)

    datatype Basis = BASIS of {F : FunEnv, G : SigEnv, E : Env}
	
    structure B = struct
      val empty = BASIS {F = F.empty, G = G.empty, E = E.empty}
      val initial = BASIS {F = F.empty, G = G.empty, E = E.initial}
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
	FinMap.Fold (fn ((funid2,(prjid2,FunSig2)),b) => b andalso
		     case FinMap.lookup F1 funid2 
		       of SOME (prjid1,FunSig1) => prjid1 = prjid2 andalso Phi.eq(FunSig1,FunSig2)
			| NONE => false) true F2


      fun enrichB(BASIS{F=F1,G=G1,E=E1}, BASIS{F=F2,G=G2,E=E2}) = 
	enrich_FunEnv(F1,F2) andalso enrich_SigEnv(G1,G2) andalso
	enrich_Env(E1,E2)
      val enrich = enrichB


              (*Restriction relation for compilation manager*)


      (*TODO 27/01/1997 22:53. tho.  brug operationerne på F og G
       i s. f. at pille direkte ved deres repræsentation:*)
      fun restrictB (BASIS {F=FUNENV F,G=SIGENV G,E},
		     {longvids : longid list, longtycons : longtycon list, longstrids : longstrid list,
		      funids : funid list, sigids : sigid list}) =
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
	    val E' = E.restrict (E, {longvids=longvids, longtycons=longtycons, longstrids=longstrids})
	in BASIS {F=FUNENV F', G=SIGENV G', E=E'}
	end
      val enrich = enrichB
      val restrict = restrictB

      (* Structure agreement *)
      fun agree([],_,_) = true
	| agree(longstrid::longstrids,B1,B2) = 
	case (lookup_longstrid B1 longstrid, 
	      lookup_longstrid B2 longstrid)
	  of (SOME E1, SOME E2) => Environments.E.eq(E1,E2) andalso agree(longstrids,B1,B2)
	   | _ => false

      (*Matching function for compilation manager*)

      fun match (BASIS {F,G,E}, BASIS {F=F0,G=G0,E=E0}) = E.match (E,E0)
    end (*B*)

  end; (*functor ModuleEnvironments*)

