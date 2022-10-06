
structure ModuleStatObject: MODULE_STATOBJECT =
  struct
    structure PP = PrettyPrint
    (*import from StatObject:*)
    type realisation       = StatObject.realisation
    type TyName            = TyName.TyName
    type TyVar             = StatObject.TyVar
    type Type              = StatObject.Type
    type TypeScheme        = StatObject.TypeScheme
    type TypeFcn           = StatObject.TypeFcn
    structure TyVar        = StatObject.TyVar
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    structure TypeFcn      = StatObject.TypeFcn

    (*import from Environments:*)
    type id                = Environments.id
    type Env               = Environments.Env
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C
    structure Realisation  = Environments.Realisation

    (*import from other modules:*)
    type tycon             = TyCon.tycon
    type StringTree        = PP.StringTree
    type strid             = StrId.strid
    type longstrid         = StrId.longstrid
    type longtycon         = TyCon.longtycon

    fun E_eq a = E.eq a         (* for profiling *)
    fun E_match a = E.match a
    fun mark_names T = app (Name.mark_gen o TyName.name) T
    fun unmark_names T = app (Name.unmark_gen o TyName.name) T


    (*Plan of this code: first two functions that are used for matching
     both signatures and functor signatures, viz. sigMatchRea and
     check_enrichment.  Then the structure Sigma and Phi.
     (*TODO 02/02/1997 23:17. tho.   well, sigMatchRea ought'a be inside
     structure Sigma, as it uses datatype Sig.*)*)

    (***** signature matching definition page 35 sec. 5.12 *****)
    (*sigMatchRea is used by both Sigma.match and Phi.match below.*)

    datatype Sig = SIGMA of {T : TyName.Set.Set, E : Env}

    datatype SigMatchError =
      MISSINGSTR  of longstrid
    | MISSINGTYPE of longtycon
    | S_CONFLICTINGARITY of longtycon * (TyName * TypeFcn)
    | CONFLICTINGEQUALITY of longtycon * (TyName * TypeFcn)
    | MISSINGVAR of strid list * id
    | MISSINGEXC of strid list * id
    | S_RIGIDTYCLASH of longtycon
    | S_CONFLICTING_DOMCE of longtycon
    | NOTYENRICHMENT of {qualid: strid list * id,
			 str_sigma : TypeScheme, str_vce: string,
			 sig_sigma : TypeScheme, sig_vce: string}
    | EXCNOTEQUAL of strid list * id * (Type * Type)

    exception No_match of SigMatchError
    fun fail reason = raise No_match reason

    fun sigMatchRea (SIGMA{T, E}, E') : realisation =
      (*... but, sigMatchRea will raise exception No_match of SigMatchError,
       if no realisation can be found.*)
      (*The path passed around is for error messages.*)
	let
	  fun implies(a,b) = (* a --> b *) not (a andalso (not b))
	  fun matchEnv (E, E', path) : realisation =
	    let val (SE, TE, _, _) = E.un E
		val (SE', TE', _, _) = E.un E'
		val phi = matchSE (SE, SE', path)
		val TEnew = Realisation.on_TyEnv phi TE
	    in
	      Realisation.oo (matchTE (TEnew, TE', path), phi)
	    end

	  and matchSE(SE, SE', path): realisation =
	    let
	      fun f (strid, E) phi : realisation =
		(case SE.lookup SE' strid of
		   SOME E' =>
		     let val Enew = Realisation.on_Env phi E
		     in Realisation.oo (matchEnv (Enew, E', strid::path), phi) end
		 | NONE =>
		       fail (MISSINGSTR (StrId.implode_longstrid(rev path, strid))))
	    in
	      SE.Fold f Realisation.Id SE
	    end

	  and matchTE (TE, TE', path) : realisation =
	      let
	       fun f (tycon, tystr) phi : realisation=
	      (case TE.lookup TE' tycon of

	       SOME tystr' =>
	      (*** As Sigma is type explicit, Definition sec. 5.8
	       *** we know that for all flexible type names t (type names bound
	       *** by T of Sigma) there exist a tycon, such that
	       *** TE(tycon) = (t, VE) for some VE.
	       *** However, it is possible that
	       *** there exists a t' and tycon', s.t. TE(tycon') =
	       *** (t', VE') for some VE' _and_ t' is not flexible (not in
	       *** T of Sigma).
	       ***)
	       let val theta = TyStr.to_theta tystr
	       in
		 (case TypeFcn.to_TyName theta of
		    SOME t =>
		      let val theta' = TyStr.to_theta tystr'
			fun err f = fail (f (TyCon.implode_LongTyCon (rev path, tycon),
						   (t,theta')))
		      in
			if TyName.Set.member t T then
			  (*** definition page 33 sec 5.6 Type Realisation ***)
			  if TyName.arity t = TypeFcn.arity theta'
			    then
			      if implies (TyName.equality t, TypeFcn.admits_equality theta')
				then Realisation.oo (phi, Realisation.singleton (t,theta'))
			      else err CONFLICTINGEQUALITY
			  else   err S_CONFLICTINGARITY
			else (* t is rigid *)
			  phi
		      end
		  | NONE => phi)
	       end

		 | NONE =>
		     fail (MISSINGTYPE
			   (TyCon.implode_LongTyCon(rev path, tycon))))
	     in
	       TE.Fold f Realisation.Id TE
	     end

	  fun elim_VEs_in_TE TE =
	    TE.Fold (fn (tycon, tystr) => fn te =>
		     let val tystr' = TyStr.from_theta_and_VE(TyStr.to_theta tystr,VE.empty)
		     in TE.plus(TE.singleton(tycon,tystr'),te)
		     end) TE.empty TE
	  fun elim_VEs_in_E E =
	    let val (SE, TE, _, R) = E.un E
	    in E.mk(SE.map elim_VEs_in_E SE, elim_VEs_in_TE TE, VE.empty, R)
	    end

	in (* the VE's are not needed for matching *)

	  matchEnv (elim_VEs_in_E E, elim_VEs_in_E E', [])

	end (* sigMatchRea *)


    (**** Enrichment : definition page 34 sec. 5.11 *)
    (*check_enrichment is used by both Sigma.match and Phi.match below.*)

    local
      (*The path passed around is for error messages.*)

          fun kind (VE.LONGVAR _)   = "var  "
            | kind (VE.LONGCON _)   = "con  "
            | kind (VE.LONGEXCON _) = "excon"

          fun sigma (VE.LONGVAR sigm)  = sigm
            | sigma (VE.LONGCON sigm)  = sigm
            | sigma (VE.LONGEXCON tau) = StatObject.TypeScheme.from_Type tau

          fun enrich_R (R1,R2) =
              List.foldl (fn (r,acc) => acc andalso List.exists (fn r' => RegVar.eq(r,r')) R1) true R2
          and equal_R (R1,R2) =
              length R1 = length R2 andalso enrich_R(R1,R2)

          fun enrichesE (E, E', path) : unit =
	      let val (SE,TE,VE,R) = E.un E
		  val (SE',TE',VE',R') = E.un E'
	      in enrichesSE (SE,SE',path) ;
		 enrichesTE (TE,TE',path) ;
		 enrichesVE (VE,VE',path) ;
                 if equal_R (R,R') then ()
                 else raise Fail "ModuleStatObject.enrichesE: different region sets"
	      end

	  and enrichesSE (SE, SE', path) : unit =
	        SE.apply
		  (fn (strid, S') =>
		         (case SE.lookup SE strid of
			    SOME S => enrichesE (S, S', strid::path)
			  | NONE => fail (MISSINGSTR
					  (StrId.implode_longstrid(rev path, strid)))))
		    SE'

	  and enrichesTE (TE, TE', path) : unit =
	        TE.apply
		  (fn (tycon, tystr') =>
		         (case TE.lookup TE tycon of
			    SOME tystr => enrichesTyStr (tystr, tystr', path, tycon)
			  | NONE => fail (MISSINGTYPE
					  (TyCon.implode_LongTyCon (rev path, tycon)))))
		     TE'

	  and enrichesTyStr (tystr, tystr', path, tycon) : unit =
	         (*the tycon, like the path, is for a potential error message*)
	        let val (theta, VE)  = TyStr.to_theta_and_VE tystr
		    val (theta',VE') = TyStr.to_theta_and_VE tystr'
		in
		  if not (TypeFcn.eq (theta, theta')) then
		    fail (S_RIGIDTYCLASH
			  (TyCon.implode_LongTyCon (rev path, tycon)))
		  else if not (VE.is_empty VE' orelse VE.eq (VE, VE'))
			 then fail (S_CONFLICTING_DOMCE
				    (TyCon.implode_LongTyCon (rev path, tycon)))
		       else ()
		end

	  and enrichesVE (VE, VE', path) : unit =
	        VE.apply
		  (fn (id, varenvrng') =>
		        (case VE.lookup VE id of
			   SOME varenvrng  =>
			     if enriches_sigma_is (varenvrng, varenvrng') then ()
			     else fail (NOTYENRICHMENT
                                        {qualid = (rev path, id),
                                         str_sigma = sigma varenvrng,
                                         str_vce  = kind  varenvrng,
                                         sig_sigma = sigma varenvrng',
                                         sig_vce  = kind  varenvrng'})
			 | NONE => fail (MISSINGVAR (rev path, id))))
		      VE'

          (*enriches_sigma_is: (sigma1,is1) enriches (sigma2,is2),
	   iff sigma1 generalises sigma2 and (is1 = is2 or is2 = v), �5.5, sml'96.
	   First the case that is1 = is2:*)

	  and enriches_sigma_is (VE.LONGVAR sigma, VE.LONGVAR sigma') =
	        TypeScheme.generalises_TypeScheme (sigma, sigma')
	    | enriches_sigma_is (VE.LONGCON sigma, VE.LONGCON sigma') =
		TypeScheme.generalises_TypeScheme (sigma, sigma')
	    | enriches_sigma_is (VE.LONGEXCON tau, VE.LONGEXCON tau') =
		Type.eq (tau, tau')
	                  (*Then the case that is2 = v:*)
	    | enriches_sigma_is (VE.LONGCON sigma, VE.LONGVAR sigma') =
	        TypeScheme.generalises_TypeScheme (sigma, sigma')
	    | enriches_sigma_is (VE.LONGEXCON tau, VE.LONGVAR sigma') =
		TypeScheme.generalises_TypeScheme
		      (TypeScheme.from_Type tau, sigma')
	    | enriches_sigma_is _ = false
      in
	fun check_enrichment (S,S') : unit = enrichesE (S, S', [])
      end (*local*)


			    (*signature*)

    structure Sigma = struct
      val triv = SIGMA {T = TyName.Set.empty, E = E.empty}
      val bogus = triv
      fun to_T_and_E (SIGMA {T, E}) = (T, E)
      fun from_T_and_E (T,E) = SIGMA {T=T, E=E}
      val to_E = #2 o to_T_and_E
      val tyvars = E.tyvars o to_E
      val tyvars' = E.tyvars' o to_E
      fun tynames (SIGMA {T, E}) = TyName.Set.difference (E.tynames E) T
      fun instance (SIGMA {T, E}) : Env =
	    Realisation.on_Env (Realisation.renaming T) E
      fun instance' (SIGMA {T, E}) : TyName.Set.Set * Env =
	let val (T', phi) = Realisation.renaming' T
	in (T', Realisation.on_Env phi E)
	end
      fun on' (phi : realisation, Sigma as SIGMA {T, E}) : Sig * realisation =
	    (*        ^ renaming of bound names *)
	    let
	      (*only clashing bound names need to be renamed to preserve
	       T intersection ( Supp phi union Yield phi) = EmptySet
	       realisations, Definition v4, page 33 sec. 5.7*)
	      val T_free = tynames Sigma
	      val T_free' = Realisation.on_TyName_set phi T_free
	      val T_clashes = TyName.Set.intersect T T_free'
	      val phi_rename = Realisation.renaming T_clashes
	      val phi_restricted = Realisation.restrict T_free phi
	      val T' = Realisation.on_TyName_set phi_rename T
	      val E' = Realisation.on_Env (Realisation.oo (phi_restricted, phi_rename)) E
	    in
	      (SIGMA {T = T', E = E'}, phi_rename)
	    end

      fun on (phi, Sigma : Sig) : Sig =
	    if Realisation.is_Id phi then Sigma else
	      #1 (on' (phi, Sigma))

      fun rename_Sig (SIGMA{T,E}) =
	let val phi = Realisation.renaming T
	in SIGMA{T=Realisation.on_TyName_set phi T,
		 E=Realisation.on_Env phi E}
	end

      fun match (Sig as SIGMA {T, E}, E') : Env =
	(* E' matches Sigma if there exists E'' s.t. Sigma >= E'' and
	 * E' enriches E''. match will raise No_match if there is no match.
	 * otherwise match will return the match E''.*)
	    let val phi = sigMatchRea (Sig,E')  (*sigMatchRea will raise No_match,
						 if no realisation can be found*)
	        val E'' = Realisation.on_Env phi E
	    in
	      check_enrichment (E',E'') ;       (*check_enrichment will raise No_match,
						 if E' does not enrich E''*)
	      E''
	    end

      (* match' is used to implement opaque signature matching. We
       * return the `transparent env', E_t, the `opaque env', E_o, and
       * a realisation, phi, mapping abstract type names of the opaque
       * env into type functions, such that phi(E_o) = E_t. *)

      fun match' (Sig, E') : Env * TyName.Set.Set * Env * realisation =
	let val (Sig as SIGMA{T,E}) = rename_Sig Sig

	    val phi = sigMatchRea (Sig,E')  (* sigMatchRea will raise No_match,
					     * if no realisation can be found. *)
	    val E'' = Realisation.on_Env phi E
	in
	  check_enrichment (E',E'') ;       (* check_enrichment will raise No_match,
					     * if E' does not enrich E''*)
	  (E'',T,E,phi)
	end

      fun eq (SIGMA{T=T1,E=E1}, Sig as SIGMA{T,...}) =
	if TyName.Set.size T1 <> TyName.Set.size T then false
	else let val Sig as SIGMA{T,E} = rename_Sig Sig
                 val phi = sigMatchRea(Sig,E1)
	     in TyName.Set.eq T1 (Realisation.on_TyName_set phi T)
		 andalso E.eq(Realisation.on_Env phi E, E1)
	     end handle _ => false
(*
      (* assumption: NO tynames in Sig1 and Sig2 may be marked generative. *)
      fun eq (SIGMA{T=T1,E=E1}, Sig2 as SIGMA{T,...}) =
	if TyName.Set.size T1 <> TyName.Set.size T then false
	else (* First rename bound names of Sig2 and then match E2 against E1 and
	      * then check for equality of E1 and E2. *)
	  let val SIGMA{T=T2,E=E2} = rename_Sig Sig2
	      val T1 = TyName.Set.list T1
	      val T2 = TyName.Set.list T2
	      val _ = mark_names T1
	      val _ = mark_names T2
	      val _ = E_match(E2,E1)
	      val _ = unmark_names T1
	      val _ = unmark_names T2
	  in E_eq(E1,E2)
	  end
*)

      fun layout (SIGMA {T, E}) =
(*	    if !Flags.DEBUG_STATOBJECTS then *)
	      let val Ttree = TyName.Set.layoutSet {start="(", sep=",", finish=")"} TyName.layout T
	      in PP.NODE {start="", finish="", indent=0,
			  children=[Ttree, E.layout E], childsep=PP.NOSEP}
	      end
(*	    else
	      E.layout E *)

      (* Pickler *)
      val pu =
	  let fun to(T,E) = SIGMA {T=T,E=E}
	      fun from (SIGMA{T,E}) = (T,E)
	  in Pickle.convert (to,from)
	      (Pickle.pairGen0(TyName.Set.pu TyName.pu, E.pu))
	  end

    end (*Sigma*)




                         (*functor signature*)

    datatype FunSig = FUNSIG of {T : TyName.Set.Set, E : Env, T'E' : Sig}

    structure Phi = struct
      fun from_T_and_E_and_Sigma (T, E, T'E') = FUNSIG {T = T, E = E, T'E' = T'E'}
      fun to_T_and_E_and_Sigma (FUNSIG {T, E, T'E'}) = (T, E, T'E')
      fun tynames (FUNSIG {T, E, T'E' (*= SIGMA {T = T', E = E'} *)}) =
(*	    TyName.Set.union
	      (TyName.Set.union (E.tynames E) T)
	      (TyName.Set.difference (E.tynames E') (TyName.Set.union T T'))
*)
	  TyName.Set.difference
	  (TyName.Set.union (Sigma.tynames T'E') (E.tynames E))
	  T

      fun tyvars Phi =
	    let val (_, E, Sig) = to_T_and_E_and_Sigma Phi
	    in
	      TyVar.unionTyVarSet (E.tyvars E, Sigma.tyvars Sig)
	    end
      fun tyvars' Phi =
	    let val (_, E, Sig) = to_T_and_E_and_Sigma Phi
	    in
	      E.tyvars' E @ Sigma.tyvars' Sig
	    end
      fun on (phi, funsig' as FUNSIG {T, E, T'E'}) : FunSig =
	    if Realisation.is_Id phi then funsig' else
	      let val (SIGMA {T = T1, E = E1}, phi_rename) =
		        Sigma.on' (phi, SIGMA {T = T, E = E})
		  val Sigma1' = Sigma.on (Realisation.oo (phi, phi_rename), T'E')
	      in
		FUNSIG {T = T1, E = E1, T'E' = Sigma1'}
	      end

      (*take care, match will raise No_match if there is no match*)
      fun match (FUNSIG {T=T, E=E, T'E'=Sig' as SIGMA {T=T',E=E'}}, E0) : Sig  =
	    let val phi = sigMatchRea(SIGMA{T=T,E=E}, E0)
	                    (*sigMatchRea will raise No_match,
			     if no realisation can be found*)
	        val E'' = Realisation.on_Env phi E
	    in
	      check_enrichment (E0,E'') ;       (*check_enrichment will raise No_match,
						 if E' does not enrich E''*)
	      let val phi_rename = Realisation.renaming T'
	      in
		Sigma.on (Realisation.oo (phi_rename, phi), Sig')
	      end
	    end

      (*take care, match_via will raise No_match if there is no match*)
      fun match_via (FUNSIG {T=T, E=E, T'E'=Sig' as SIGMA {T=T',E=E'}}, E0) : Sig * realisation * realisation =
	    let val phi = sigMatchRea(SIGMA{T=T,E=E}, E0)
	                    (*sigMatchRea will raise No_match,
			     if no realisation can be found*)
	        val E'' = Realisation.on_Env phi E
	    in
	      check_enrichment (E0,E'') ;       (*check_enrichment will raise No_match,
						 if E' does not enrich E''*)
	      let val phi_rename = Realisation.renaming T'
		  val phi' = Realisation.oo (phi_rename, phi)
		  fun Sigma_on(rea,SIGMA{T,E}) =              (* Avoid the mysterious Sigma.on; the interpreter *)
		    SIGMA{T=Realisation.on_TyName_set rea T,  (* is very picky on type name rebinding in functor *)
			  E=Realisation.on_Env rea E}         (* signatures. - Martin *)
	      in
		(Sigma_on (phi', Sig'), phi, phi_rename)      (* The elaborator must maintain the invariant that *)
	      end                                             (* (Supp(phi') U Yield(phi')) \cap (T of Sig') = 0 *)
	    end                                               (* -- I believe it does -- Martin *)

      local
	fun rename_FunSig (FUNSIG{T,E,T'E'}) =
	  let val phi = Realisation.renaming T
	  in FUNSIG{T=Realisation.on_TyName_set phi T,
		    E=Realisation.on_Env phi E,
		    T'E'=Sigma.on(phi,T'E')}
	  end
      in
	fun eq (FUNSIG{T=T1,E=E1,T'E'=Sig1'}, funsig2 as FUNSIG{T,...}) =
	  if TyName.Set.size T1 <> TyName.Set.size T then false
	  else let val FUNSIG{T=T2,E=E2,T'E'=Sig2'} = rename_FunSig funsig2
		   val Sig2 = SIGMA{T=T2,E=E2}
		   val phi = sigMatchRea(Sig2,E1)
	       in TyName.Set.eq T1 (Realisation.on_TyName_set phi T2)
		   andalso E.eq(Realisation.on_Env phi E2,E1)
		   andalso Sigma.eq(Sigma.on(phi,Sig2'),Sig1')
	       end handle _ => false
(*
	fun eq(FUNSIG{T=T1,E=E1,T'E'=Sig1'}, funsig2 as FUNSIG{T,...}) =
	  if TyName.Set.size T1 <> TyName.Set.size T then false
	  else
	    let val FUNSIG{T=T2,E=E2,T'E'=Sig2'} = rename_FunSig funsig2
	        val T1 = TyName.Set.list T1
		val T2 = TyName.Set.list T2
		val _ = mark_names T1
		val _ = mark_names T2
		val _ = E_match(E2,E1)
		val _ = unmark_names T1
		val _ = unmark_names T2
	    in E_eq(E1,E2) andalso Sigma.eq(Sig1',Sig2')
	    end
*)
      end

      fun layout (FUNSIG{T, E, T'E'}) =
	    let
	      val Ttree = TyName.Set.layoutSet {start="(", sep=",", finish=")"} TyName.layout T
	      val body = PP.NODE {start="(", finish=")", indent=1,
				  children=[E.layout E, Sigma.layout T'E'],
				  childsep=PP.RIGHT ", "}
	    in
	      PP.NODE {start="", finish="", indent=1,
		       children=[Ttree,body],
		       childsep=PP.NOSEP}
	    end

      val pu =
	  let fun to (T,E,T'E') = FUNSIG{T=T,E=E,T'E'=T'E'}
	      fun from (FUNSIG{T=T,E=E,T'E'=T'E'}) = (T,E,T'E')
	  in Pickle.convert (to,from)
	      (Pickle.tup3Gen0(TyName.Set.pu TyName.pu, E.pu, Sigma.pu))
	  end

    end (*Phi*)

  end (*ModuleStatObject*)
