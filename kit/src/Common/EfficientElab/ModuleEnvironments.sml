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
      fun tyvars (SIGENV G) =
	    FinMap.fold
	      (fn (Sig, tyvars) => TyVar.unionTyVarSet (Sigma.tyvars Sig, tyvars))
	         [] G
      fun tyvars' (SIGENV G) =
	    FinMap.fold
	      (fn (Sig, criminals) => Sigma.tyvars' Sig @ criminals)
	         [] G
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

    datatype Basis = BASIS of {T : TyName.Set.Set, F : FunEnv, G : SigEnv,
			       E : Env}
	
    structure B = struct
      val empty = BASIS {T = TyName.Set.empty , F = F.empty, G = G.empty,
			 E = E.empty}
      val initial = BASIS {T = E.tynames E.initial, F = F.empty, G = G.empty,
			   E = E.initial}
      val bogus = empty
      fun plus (BASIS {T, F, G, E}, 
		BASIS {T=T', F=F', G=G', E=E'}) =
	    BASIS {T=TyName.Set.union T T', F=F.plus (F, F'), G=G.plus (G, G'),
		   E=E.plus (E, E')}

      (*It is assumed that phi does not affect F and G. This holds as an
       invariant throughout elaboration.*)
      fun on phi (B as (BASIS {T, F, G, E})) : Basis =
	    if Realisation.is_Id phi then B
	    else
	      BASIS {T = T, F = F, G = G, E = Realisation.on_Env phi E}
      fun tyvars (BASIS{T, F, G, E}) : TyVar list =
	    TyVar.unionTyVarSet (F.tyvars F,
				 TyVar.unionTyVarSet (G.tyvars G, E.tyvars E))
      fun tyvars' (BASIS{T, F, G, E}) =
	    F.tyvars' F @ G.tyvars' G @ E.tyvars' E
      fun tynames (BASIS{T, F, G, E}) =
            TyName.Set.union (F.tynames F)
	    (TyName.Set.union (G.tynames G) (E.tynames E))
      fun to_C (BASIS {T, F, G, E}) = C.from_T_and_E (T, E)
      fun layout (BASIS {T, F, G, E}) =
	    PP.NODE {start="", finish="", indent = 0,
		     children = (if !Flags.DEBUG_ENVIRONMENTS then
				   [TyName.Set.layoutSet
				      {start="{", sep=",", finish="}"}
					TyName.layout T]
				 else [])
		                @ [F.layout F, G.layout G, E.layout E],
		     childsep = PP.RIGHT " "}

                            (*E component*)
      fun plus_E (BASIS {T, F, G, E}, E') =
	    BASIS {T=T, F=F, G=G, E=E.plus (E, E')}
      fun cplus_E (BASIS {T, F, G, E}, E') =
	    BASIS {T = TyName.Set.union T (E.tynames E'), F = F, G = G,
		   E = E.plus (E, E')}
      fun from_T_and_E (T, E) =
	    BASIS {T = T, F = F.empty, G = G.empty, E = E}
      fun to_E (BASIS {E, ...}) = E 
      val lookup_strid = E.lookup_strid o to_E
      val lookup_longstrid = E.lookup_longstrid o to_E
      val lookup_longtycon = E.lookup_longtycon o to_E

                            (*T component*)
      fun plus_T (BASIS {T, F, G, E}, T') = 
	    BASIS {T = TyName.Set.union T T', F = F, G = G, E = E}
      fun to_T (BASIS {T, ...}) = T

      (*G component*)
      fun from_T_and_G (T, G) =
	    BASIS {T = T, F = F.empty, G = G, E = E.empty}
      fun to_G (BASIS {G, ...}) = G
      fun plus_G (BASIS {T, F, G, E}, G') =
	    BASIS {T = T, F = F, G = G.plus (G, G'), E = E}
      fun cplus_G (BASIS {T, F, G, E}, G') =
	    BASIS {T = TyName.Set.union T (G.tynames G'), F = F,
		   G = G.plus (G, G'), E = E}
      val lookup_sigid = G.lookup o to_G

                            (*F component*)
      fun from_T_and_F (T, F) =
	    BASIS {T = T, F = F, G = G.empty, E = E.empty}
      fun to_F (BASIS{F, ...}) = F
      fun plus_F (BASIS {T, F, G, E}, F') =
	    BASIS {T = T, F = F.plus (F, F'), G = G, E = E}
      fun cplus_F (BASIS {T, F, G, E}, F') =
	    BASIS {T = TyName.Set.union T (F.tynames F'),
		   F = F.plus (F, F'), G = G, E = E}
      val lookup_funid = F.lookup o to_F


              (*Enrichment relation for compilation manager*)

  (*TODO 27/01/1997 22:49. tho.  bør funktionerne nedenfor ikke
   være i modulerne hhv. TE, VE og SE?*)

      fun equal_VarEnvRan (VE.LONGVAR s1, VE.LONGVAR s2) =
	    StatObject.TypeScheme.eq (s1,s2)
	| equal_VarEnvRan (VE.LONGCON s1, VE.LONGCON s2) =
	    StatObject.TypeScheme.eq (s1,s2)
	| equal_VarEnvRan (VE.LONGEXCON t1, VE.LONGEXCON t2) =
	    StatObject.Type.eq (t1,t2)
	| equal_VarEnvRan _ = false

      fun enrich_TyEnv (TE1,TE2) =
	    TE.Fold (fn (tycon2, TyStr2) => fn b => b andalso
		     case TE.lookup TE1 tycon2 of
		       Some TyStr1 => TyStr.eq (TyStr1,TyStr2)
		     | None => false) true TE2

      fun equal_TyEnv(TE1,TE2) =
	let fun sorter a b = TyCon.< (a,b)
	    val dom1 = ListSort.sort sorter (EqSet.list (TE.dom TE1))
	    val dom2 = ListSort.sort sorter (EqSet.list (TE.dom TE2))
	in dom1 = dom2 andalso enrich_TyEnv (TE1,TE2)
	end

      fun enrich_VarEnv(VE1,VE2) =
	    VE.Fold (fn (id2,r2) => fn b => b andalso
			  (case VE.lookup VE1 id2 of
			     Some r1 => equal_VarEnvRan(r1,r2)
			   | None => false)) true VE2

      fun equal_VarEnv(VE1,VE2) =
	let fun sorter a b = Ident.< (a,b)
	    val dom1 = ListSort.sort sorter (EqSet.list (VE.dom VE1))
	    val dom2 = ListSort.sort sorter (EqSet.list (VE.dom VE2))
	in dom1 = dom2 andalso enrich_VarEnv(VE1,VE2)
	end

      fun equal_Env(E1,E2) =
	let val (SE1,TE1,VE1) = E.un E1
	    val (SE2,TE2,VE2) = E.un E2
	in equal_StrEnv(SE1,SE2)
	  andalso equal_TyEnv(TE1,TE2)
	  andalso equal_VarEnv(VE1,VE2)
	end

      and enrich_StrEnv(SE1,SE2) = SE.Fold (fn (strid2,S2) => fn b => b andalso
					     case SE.lookup SE1 strid2 of
					       Some S1 => equal_Env(S1,S2)
					     | None => false) true SE2

      and equal_StrEnv(SE1,SE2) =
	let fun sorter a b = StrId.< (a,b)
	    val dom1 = ListSort.sort sorter (EqSet.list (SE.dom SE1))
	    val dom2 = ListSort.sort sorter (EqSet.list (SE.dom SE2))
	in dom1 = dom2 andalso enrich_StrEnv (SE1,SE2)
	end

      fun enrich_FunEnv(F1,F2) = EqSet.isEmpty (F.dom F2)
      fun enrich_SigEnv(G1,G2) = EqSet.isEmpty (G.dom G2)

      fun enrich_Env(E1,E2) =
	let val (SE1,TE1,VE1) = E.un E1
	    val (SE2,TE2,VE2) = E.un E2
	in enrich_StrEnv(SE1,SE2)
	  andalso enrich_TyEnv(TE1,TE2)
	  andalso enrich_VarEnv(VE1,VE2)
	end


      fun enrichB(BASIS{T=T1,F=F1,G=G1,E=E1}, BASIS{T=T2,F=F2,G=G2,E=E2}) = 
	enrich_FunEnv(F1,F2) andalso enrich_SigEnv(G1,G2) andalso
	enrich_Env(E1,E2)
      val enrich = enrichB


              (*Restriction relation for compilation manager*)

      fun restrictE(E,{ids,tycons,strids}) =
	let val (SE,TE,VE) = E.un E
	    val SE' = List.foldL
	                (fn strid => fn SEnew =>
			 let val E = (case SE.lookup SE strid of
					Some E => E
				      | None => die "restrictE: strid not in env.")
			 in SE.plus (SEnew, SE.singleton (strid,E))
			 end) SE.empty strids
	    val TE' = List.foldL
	                (fn tycon => fn TEnew =>
			 let val TyStr = (case TE.lookup TE tycon of
					    Some TyStr => TyStr
					  | None => die "restrictE: tycon not in env.")
			 in TE.plus (TEnew, TE.singleton (tycon,TyStr))
			 end) TE.empty tycons
	    val VE' = VE.restrict (VE,ids)
	in E.mk (SE',TE',VE')
	end


      (*TODO 27/01/1997 22:53. tho.  brug operationerne på F og G
       i s. f. at pille direkte ved deres repræsentation:*)
      fun restrictB (BASIS {T,F=FUNENV F,G=SIGENV G,E},
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
	    val E' = restrictE (E, {ids=ids, tycons=tycons, strids=strids})
	in BASIS {T=T, F=FUNENV F', G=SIGENV G', E=E'}
	end
      val enrich = enrichB
      val restrict = restrictB

      (*Matching function for compilation manager*)

      local
	fun match_VE_range (VE.LONGVAR sigma1, VE.LONGVAR sigma2) =
	      StatObject.TypeScheme.match (sigma1, sigma2)
	  | match_VE_range (VE.LONGCON sigma1, VE.LONGCON sigma2) =
	      StatObject.TypeScheme.match (sigma1, sigma2)
	  | match_VE_range (VE.LONGEXCON tau1, VE.LONGEXCON tau2) =
	      StatObject.Type.match (tau1, tau2)
	  | match_VE_range _ = ()

	fun matchVE (VE,VE0) = VE.Fold (fn (id,r) => fn () =>
					   (case VE.lookup VE0 id of
					      Some r0 => match_VE_range (r,r0)
					    | None => ())) () VE

	fun matchTE (TE,TE0) = TE.Fold (fn (tycon, TyStr) => fn () =>
					   (case TE.lookup TE0 tycon of
					      Some TyStr0 => ()
					    (*C.match_TyStr(TyStr,TyStr0) *) (* MEMO *)
					    | None => ())) () TE

	fun matchE (E,E0) =
	      let val (SE, TE, VE)  = E.un E
		  val (SE0,TE0,VE0) = E.un E0
	      in
		matchSE (SE, SE0) ; 
		matchTE (TE, TE0) ;
		matchVE (VE, VE0)
	      end

	and matchSE (SE,SE0) = SE.Fold (fn (strid,E) => fn () =>
					   (case SE.lookup SE0 strid of
					      Some E0 => matchE (E,E0)
					    | None => ())) () SE
      in
	fun match (BASIS {T,F,G,E}, BASIS {T=T0,F=F0,G=G0,E=E0}) = matchE (E,E0)
      end (*local*)
    end (*B*)

  end; (*functor ModuleEnvironments*)
