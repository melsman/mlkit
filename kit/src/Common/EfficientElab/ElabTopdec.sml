(*************************************************************)
(* Elaboration of top-level declarations, modules included,  *)
(*   Section 5 of the Definition, v3                         *)
(*************************************************************)

(*$ElabTopdec: TOPDEC_GRAMMAR ELABDEC ENVIRONMENTS STATOBJECT
	MODULE_STATOBJECT MODULE_ENVIRONMENTS STRID SIGID
	PARSE_INFO ELAB_INFO BASIC_IO PRETTYPRINT REPORT CRASH
	FLAGS FINMAP ELABTOPDEC*)

functor ElabTopdec
  (structure PrettyPrint : PRETTYPRINT
   structure StrId : STRID
   structure SigId : SIGID
   structure StatObject : STATOBJECT
   structure ParseInfo : PARSE_INFO
   structure ElabInfo : ELAB_INFO
   sharing ElabInfo.ParseInfo = ParseInfo
   sharing type ElabInfo.ErrorInfo.TyName = StatObject.TyName
   sharing type ElabInfo.ErrorInfo.TyVar = StatObject.TyVar
   sharing type ElabInfo.ErrorInfo.TypeFcn = StatObject.TypeFcn
   sharing type ElabInfo.ErrorInfo.Type = StatObject.Type
   sharing type ElabInfo.ErrorInfo.strid = StrId.strid
   sharing type ElabInfo.ErrorInfo.sigid = SigId.sigid
   sharing type ElabInfo.ErrorInfo.longstrid = StrId.longstrid
   sharing type ElabInfo.TypeInfo.realisation = StatObject.realisation

  structure IG : TOPDEC_GRAMMAR
     sharing type IG.strid = StrId.strid
     sharing type IG.longstrid = StrId.longstrid 
     sharing type IG.sigid = SigId.sigid
     sharing type IG.info = ParseInfo.ParseInfo
     sharing type IG.tyvar = StatObject.ExplicitTyVar
     sharing type ElabInfo.ErrorInfo.id = IG.id
     sharing type ElabInfo.ErrorInfo.tycon = IG.tycon
     sharing type ElabInfo.ErrorInfo.funid = IG.funid
     sharing type ElabInfo.ErrorInfo.longtycon = IG.longtycon
     sharing type IG.StringTree = PrettyPrint.StringTree

  structure OG : TOPDEC_GRAMMAR
     sharing type OG.funid = IG.funid
     sharing type OG.strid = IG.strid
     sharing type OG.sigid = SigId.sigid
     sharing type OG.longstrid = StrId.longstrid
     sharing type OG.longtycon = IG.longtycon
     sharing type OG.tyvar = IG.tyvar
     sharing type OG.tycon = IG.tycon
     sharing type OG.id = IG.id
     sharing type OG.info = ElabInfo.ElabInfo
     sharing type OG.StringTree = PrettyPrint.StringTree

   structure Environments : ENVIRONMENTS
     sharing Environments.TyName = StatObject.TyName
     sharing type Environments.TyVar = StatObject.TyVar
     sharing type Environments.Type = StatObject.Type
     sharing type Environments.TypeScheme = StatObject.TypeScheme
     sharing type Environments.Substitution = StatObject.Substitution
     sharing type Environments.TypeFcn = StatObject.TypeFcn
     sharing type Environments.level = StatObject.level
     sharing type Environments.realisation = StatObject.realisation
     sharing type Environments.id = IG.id
     sharing type Environments.strid = IG.strid
     sharing type Environments.tycon = IG.tycon
     sharing type Environments.ExplicitTyVar = StatObject.ExplicitTyVar
     sharing type Environments.longtycon = IG.longtycon
     sharing type Environments.longstrid = IG.longstrid
     sharing type Environments.ty = IG.ty
         and type Environments.Env = ElabInfo.TypeInfo.Env

   structure ModuleStatObject: MODULE_STATOBJECT
     sharing ModuleStatObject.TyName = StatObject.TyName
     sharing type ModuleStatObject.Env = Environments.Env
     sharing type ModuleStatObject.realisation = StatObject.realisation
     sharing type ModuleStatObject.ErrorInfo = ElabInfo.ErrorInfo.ErrorInfo
     sharing type ModuleStatObject.StringTree = PrettyPrint.StringTree

   structure ModuleEnvironments : MODULE_ENVIRONMENTS
     sharing ModuleEnvironments.TyName = StatObject.TyName
     sharing type ModuleEnvironments.TyVar = StatObject.TyVar
     sharing type ModuleEnvironments.TyStr = Environments.TyStr
     sharing type ModuleEnvironments.Env = Environments.Env
     sharing type ModuleEnvironments.Sig = ModuleStatObject.Sig
     sharing type ModuleEnvironments.FunSig = ModuleStatObject.FunSig
     sharing type ModuleEnvironments.Context = Environments.Context
     sharing type ModuleEnvironments.realisation = StatObject.realisation
     sharing type ModuleEnvironments.id = IG.id
     sharing type ModuleEnvironments.strid = IG.strid
     sharing type ModuleEnvironments.tycon = IG.tycon
     sharing type ModuleEnvironments.longstrid = IG.longstrid
     sharing type ModuleEnvironments.longtycon = IG.longtycon
     sharing type ModuleEnvironments.sigid = IG.sigid
     sharing type ModuleEnvironments.funid = IG.funid

   structure ElabDec : ELABDEC
     sharing type ElabDec.PreElabDec = IG.dec
     sharing type ElabDec.PostElabDec = OG.dec
     sharing type ElabDec.PreElabTy = IG.ty
     sharing type ElabDec.PostElabTy = OG.ty
     sharing type ElabDec.Env = Environments.Env
     sharing type ElabDec.Context = Environments.Context
     sharing type ElabDec.Type = StatObject.Type

   structure FinMap : FINMAP
   structure Report : REPORT
     sharing type Report.Report = PrettyPrint.Report
   structure Flags : FLAGS
   structure Crash : CRASH
     ) : ELABTOPDEC  =
  struct
    fun impossible s = Crash.impossible ("ElabTopdec." ^ s)
    fun noSome None s = impossible s
      | noSome (Some x) s = x
    fun is_Some None = false
      | is_Some (Some x) = true
    val StringTree_to_string = PrettyPrint.flatten1
    fun quote s = "`" ^ s ^ "'"
    fun pp_list pp_x [] = ""
      | pp_list pp_x [x] = pp_x x
      | pp_list pp_x [x,x'] = pp_x x ^ " and " ^ pp_x x'
      | pp_list pp_x (x::xs) = pp_x x ^ ", " ^ pp_list pp_x xs

    (*import from StatObject:*)
    structure Level        = StatObject.Level
    structure TyVar        = StatObject.TyVar
         type TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
         type Type         = StatObject.Type
    structure Type         = StatObject.Type
    structure TypeScheme   = StatObject.TypeScheme
    structure TypeFcn      = StatObject.TypeFcn
	    
    (*import from Environments:*)
    type VarEnv            = Environments.VarEnv
    type TyStr             = Environments.TyStr
    type TyEnv             = Environments.TyEnv
    type StrEnv            = Environments.StrEnv
    type Env               = Environments.Env
    type Context           = Environments.Context
    type constructor_map   = Environments.constructor_map
    type realisation       = Environments.realisation
    structure VE           = Environments.VE
    structure TyStr        = Environments.TyStr
    structure TE           = Environments.TE
    structure SE           = Environments.SE
    structure E            = Environments.E
    structure C            = Environments.C
    structure constructor_map = Environments.constructor_map
    structure Realisation  = Environments.Realisation

    (*import from ModuleEnvironments:*)
    type Basis             = ModuleEnvironments.Basis
    type FunEnv            = ModuleEnvironments.FunEnv
    type SigEnv            = ModuleEnvironments.SigEnv
    structure G            = ModuleEnvironments.G
    structure F            = ModuleEnvironments.F
    structure B            = ModuleEnvironments.B

    (*import from ModuleStatObject:*)
    type Sig               = ModuleStatObject.Sig
    type FunSig            = ModuleStatObject.FunSig
    exception No_match     = ModuleStatObject.No_match
    (*may be raised by Sigma.match and Phi.match*)
    structure Sigma        = ModuleStatObject.Sigma
    structure Phi          = ModuleStatObject.Phi
    
    (*import from other modules:*)
    type StringTree        = PrettyPrint.StringTree
    type tycon             = IG.tycon
    type strid             = IG.strid
    structure TyCon        = IG.DecGrammar.TyCon
    structure ErrorInfo    = ElabInfo.ErrorInfo
    structure TypeInfo     = ElabInfo.TypeInfo

    infixr onE           val op onE = Realisation.on_Env
    infixr onB           val op onB = B.on
    infixr oo            val op oo  = Realisation.oo
    infixr B_cplus_E     val op B_cplus_E  = B.cplus_E
    infixr B_plus_E      val op B_plus_E   = B.plus_E
    infixr B_plus_T      val op B_plus_T   = B.plus_T
    infixr G_plus_G      val op G_plus_G   = G.plus
    infixr B_plus_G      val op B_plus_G   = B.plus_G
    infixr E_plus_E      val op E_plus_E   = E.plus
    infixr F_plus_F      val op F_plus_F   = F.plus
    infixr B_plus_F      val op B_plus_F   = B.plus_F
    infixr SE_plus_SE    val op SE_plus_SE = SE.plus

      (*the following three types are for the signature:*)
    type PreElabTopdec  = IG.topdec
    type PostElabTopdec = OG.topdec
    type StaticBasis = ModuleEnvironments.Basis


    (*Error handling stuff*)
    type ParseInfo  = ParseInfo.ParseInfo
    type ElabInfo = ElabInfo.ElabInfo

    val okConv = ElabInfo.from_ParseInfo

    fun errorConv (i : ParseInfo, error_info : ErrorInfo.ErrorInfo) : ElabInfo =
          ElabInfo.plus_ErrorInfo (okConv i) error_info

    fun typeConv (i : ParseInfo, type_info : TypeInfo.TypeInfo) : ElabInfo =
          ElabInfo.plus_TypeInfo (okConv i) type_info

    fun repeatedIdsError (i : ParseInfo,
			  rids : ErrorInfo.RepeatedId list)
          : ElabInfo =
            errorConv (i, ErrorInfo.REPEATED_IDS rids)

    (*repeaters (op =) [1,2,1,3,4,4] = [1,4].  Used to check
     syntactic restrictions:*)

    fun repeaters eq ys =
          let
	    fun occurences x [] = 0
	      | occurences x (y::ys) = 
	          (if eq (x,y) then 1 else 0) + occurences x ys
	  in
	    List.all (fn x => (occurences x ys) > 1) ys
	  end

    fun memberTyVarList x xs =
          List.exists (fn y => TyVar.eq (x,y)) xs
    fun isEmptyTyVarList xs = case xs of nil => true | _ => false

	 
    (*Phi_match: We bind the argument names in error_result so
    that the argument signature returned in the event of an
    error is as general as possible:*)

    fun Phi_match (i, funsig', E0) =
      let val (res, rea) = Phi.match_via(funsig',E0)
      in (typeConv(i,TypeInfo.FUNCTOR_APP_INFO rea), res)     
	  (* was: (okConv i, Phi.match (funsig', E0)) *)
      end handle No_match reason =>
	    let
	      val (T, E, T'E') = Phi.to_T_and_E_and_Sigma funsig'
	      val (T', E') = Sigma.to_T_and_E T'E'
	      val E_bogus = Sigma.from_T_and_E (TyName.Set.difference (E.tynames E')
						  (TyName.Set.union T T'), 
						E')
	    in 
	      (errorConv (i, reason), E_bogus)
	    end

    (*Sigma_match: We rename flexible names in error_result so
     that the result structure returned in the event of an error
     is as general as possible:*)

    fun Sigma_match (i, Sigma, E) =
	  (okConv i, Sigma.match (Sigma, E))
	  handle No_match reason =>
	    (errorConv (i, reason), Sigma.instance Sigma)

    (*initial_TE datdesc = the TE to be used initially in
     elaborating a datdesc. We determine the correct equality
     attributes when we maximise equality:*)

    fun initial_TE (IG.DATDESC (_, explicittyvars, tycon, _, None)) =
          TE.init explicittyvars tycon
      | initial_TE (IG.DATDESC (_, explicittyvars, tycon, _, Some datdesc)) =
	  TE.plus (TE.init explicittyvars tycon, initial_TE datdesc)

    (*wellformed_E is used for the `where type' construct*)

    fun wellformed_E E =
          let val (SE, TE, VE) = E.un E
	  in
	    wellformed_SE SE andalso wellformed_TE TE
	  end
    and wellformed_TE TE =
          TE.fold
	    (fn tystr => fn bool => bool andalso wellformed_TyStr tystr)
	       true TE
    and wellformed_TyStr tystr =
          let val (theta, VE) = TyStr.to_theta_and_VE tystr
	  in
	    TypeFcn.is_TyName theta orelse VE.is_empty VE
	  end
    and wellformed_SE SE =
          SE.fold (fn E => fn bool => bool andalso wellformed_E E) true SE


    (*structure sharing*)
    exception Share of ErrorInfo.ErrorInfo
    local
      datatype trie = Trie of (tycon, TyStr list) FinMap.map
			    * (StrId.strid, trie) FinMap.map
      (*build ``trie''*)
      fun update_strid_map f (Trie (tycon_map, strid_map)) = (Trie (tycon_map, f strid_map))
      fun update_tycon_map f (Trie (tycon_map, strid_map)) = (Trie (f tycon_map, strid_map))
      val trie0 = Trie (FinMap.empty, FinMap.empty)
      fun insert_xy update_xmap ycombine ynil (x,y) = 
	    update_xmap
	      (fn xmap =>
	       FinMap.add (x, ycombine y (case FinMap.lookup xmap x of Some y' => y'
	                                                               | None => ynil),
	                   xmap))
      fun insert_E E trie =
	    TE.Fold    ((insert_xy update_tycon_map (General.curry (op ::)) [] )
			: tycon * TyStr -> trie -> trie)
	      (SE.Fold ((insert_xy update_strid_map insert_E trie0)
			: strid * Env -> trie -> trie)
	         trie (E.to_SE E))
	      (E.to_TE E)

      (*traverse ``trie'':*)
      fun tystr_to_tyname tyname_rigid strids tycon tystr =
	    let val (theta, VE) = TyStr.to_theta_and_VE tystr
	    in
	      (case TypeFcn.to_TyName theta of
		 Some t =>
		   if tyname_rigid t then
		     raise Share (ErrorInfo.SHARING_TYPE_RIGID
				  (TyCon.implode_LongTyCon (rev strids, tycon), t))
		   else t
	       | None => raise Share (ErrorInfo.SHARING_TYPE_NOT_TYNAME
				      (TyCon.implode_LongTyCon (rev strids, tycon), theta)))
	    end

      fun phi_that_identifies_tynames tynames =
	    (*tynames must have at least one element*)
	    (case tynames of
	       [] => impossible "phi_that_identifies_tynames"
	     | ts as t1::ts' =>
		 let val arity = TyName.arity t1
		 in
		   if List.forAll (fn t => TyName.arity t = arity) ts' then
		     Realisation.from_T_and_theta
		       (TyName.Set.fromList tynames,
			TypeFcn.from_TyName
			  (TyName.freshTyName
			     {tycon = TyName.tycon t1, arity = TyName.arity t1, 
			      equality = List.exists TyName.equality ts}))
		   else raise Share (ErrorInfo.SHARING_TYPE_ARITY ts)
		 (*TODO 07/03/1997 19:09. tho.  that^ does not
		  result in a terribly informative error message.
		  It will have todo for now.*)
		 end)

      (*the `strids' argument is the current path, only
       needed for error messaging; the `tyname_rigid' argument is passed
       around everywhere only to be available in `tystr_to_tyname'*)

      fun traverse tyname_rigid strids (Trie (tycon_map, strid_map)) : realisation =
	    tycon_traverse tyname_rigid strids tycon_map oo
	    strid_traverse tyname_rigid strids strid_map
      and tycon_traverse tyname_rigid strids tycon_map =
	    FinMap.Fold
	      (fn ((tycon, tystrs), phi) =>
	       phi oo phi_that_identifies_tynames
	                (map (tystr_to_tyname tyname_rigid strids tycon) tystrs))
		 Realisation.Id tycon_map
      and strid_traverse tyname_rigid strids strid_map =
	    FinMap.Fold
	      (fn ((strid, trie), phi) =>
	       phi oo traverse tyname_rigid (strid::strids) trie)
	         Realisation.Id strid_map
    in
      fun share tyname_rigid (Es : Env list) =
	    traverse tyname_rigid [] (List.foldL insert_E trie0 Es)
    end (*local*)


    fun map_Some_on_2nd (x,y) = (x,Some y)
      
    fun elab_X_opt (Y, Some X) elab_X empty_Z =
          map_Some_on_2nd (elab_X (Y, X))
      | elab_X_opt (Y, None) elab_X empty_Z = (empty_Z, None)

    (*****************************************************)
    (* Structure Expressions - Definition v3 pages 36-37 *)
    (*****************************************************)

    fun elab_strexp (B : Basis, strexp : IG.strexp) : (Env * OG.strexp) =

      (case strexp of
	(* Generative *)                                    (*rule 50*)
	IG.STRUCTstrexp (i, strdec) =>
	  let val (E, out_strdec) = elab_strdec (B, strdec)
	  in
	    (E, OG.STRUCTstrexp (okConv i, out_strdec))
	  end

	(* Structure identifier *)                          (*rule 51*)
      | IG.LONGSTRIDstrexp (i, longstrid) => 
	  (case B.lookup_longstrid B longstrid of
	     Some E => (E, OG.LONGSTRIDstrexp (okConv i, longstrid))
	   | None =>
	       (E.bogus,
		OG.LONGSTRIDstrexp
		  (errorConv (i, ErrorInfo.LOOKUP_LONGSTRID longstrid), longstrid)))

	                                                    (*rule 52*)
      | IG.TRANSPARENT_CONSTRAINTstrexp (i, strexp, sigexp) =>
	  let val (E, out_strexp) = elab_strexp (B, strexp)
	      val (Sigma, out_sigexp) = elab_sigexp (B, sigexp)
	      val (out_i, E') = Sigma_match (i, Sigma, E)
	      val out_i = ElabInfo.plus_TypeInfo out_i (TypeInfo.TRANS_CONSTRAINT_INFO E')
	  in
	    (E', OG.TRANSPARENT_CONSTRAINTstrexp (out_i, out_strexp, out_sigexp))
	  end
	                                                    (*rule 53*)
      | IG.OPAQUE_CONSTRAINTstrexp (i, strexp, sigexp) =>
	  let val (E, out_strexp) = elab_strexp (B, strexp)
	      val (Sigma, out_sigexp) = elab_sigexp (B, sigexp)
	      val (out_i, E'') = Sigma_match (i, Sigma, E)
	      val E' = Sigma.instance Sigma (*this instantiation is the difference
					     from the transparent constraint above*)
	  in
	    (E', OG.OPAQUE_CONSTRAINTstrexp (out_i, out_strexp, out_sigexp))
	  end

	(* Functor application *)                           (*rule 54*)
      | IG.APPstrexp (i, funid, strexp) =>
	  let
	    val (E, out_strexp) = elab_strexp (B, strexp)
	  in
	    (case B.lookup_funid B funid of
	       Some Phi =>
		 let
		   val (out_i, T'E') = Phi_match (i, Phi, E)
		   val E' = Sigma.instance T'E'
		 in
		   (E', OG.APPstrexp (out_i, funid, out_strexp))
		 end
	     | None =>
		 (E.bogus,
		  OG.APPstrexp (errorConv (i, ErrorInfo.LOOKUP_FUNID funid),
				funid, out_strexp)))
	  end

	(* Local declaration *)                             (*rule 55*)
      | IG.LETstrexp (i, strdec, strexp) =>
	  let val (E1, out_strdec) = elab_strdec (B, strdec)
	      val (E2, out_strexp) = elab_strexp (B B_cplus_E E1, strexp)
	  in
	    (E2, OG.LETstrexp (okConv i, out_strdec, out_strexp))
	  end)

    (********************************************************)
    (* Structure-level Declarations - Definition v3 page 37 *)
    (********************************************************)

    and elab_strdec (B : Basis, strdec : IG.strdec) : (Env * OG.strdec) =
      (case strdec of

	(* Core declaration *)                              (*rule 56*)
	IG.DECstrdec (i, dec) => 
	  let
	    (*Note that E is always principal for dec*)
	    val (E, out_dec) = ElabDec.elab_dec (B.to_C B, dec)
	  in
	    (E,  OG.DECstrdec (okConv i, out_dec))
	  end

	(* Structure declaration *)                         (*rule 57*)
      | IG.STRUCTUREstrdec (i, strbind) =>
	  let val (SE, out_strbind) = elab_strbind (B, strbind)
	  in
	    (E.from_SE SE,  OG.STRUCTUREstrdec (okConv i, out_strbind))
	  end

	(* Local declaration *)                             (*rule 58*)
      | IG.LOCALstrdec (i, strdec1, strdec2) =>
	  let
	    val (E1, out_strdec1) = elab_strdec (B, strdec1)
	    val (E2, out_strdec2) = elab_strdec (B B_cplus_E E1, strdec2)
	  in
	    (E2,  OG.LOCALstrdec (okConv i, out_strdec1, out_strdec2))
	  end

	(* Empty declaration *)                             (*rule 59*)
      | IG.EMPTYstrdec i => (E.empty,  OG.EMPTYstrdec (okConv i))

	(* Sequential declaration *)                        (*rule 60*)
      | IG.SEQstrdec (i, strdec1, strdec2) =>
	  let
	    val (E1, out_strdec1) = elab_strdec (B, strdec1)
	    val (E2, out_strdec2) = elab_strdec (B B_cplus_E E1, strdec2)
	  in
	    (E1 E_plus_E E2,
	     OG.SEQstrdec (okConv i, out_strdec1, out_strdec2))
	  end)

    (**********************************************)
    (* Structure Bindings - Definition v3 page 38 *)
    (**********************************************)

    and elab_strbind (B : Basis, strbind : IG.strbind)
      : (StrEnv * OG.strbind) =
      (case strbind of 
       (* Structure bindings *)                             (*rule 61*)
       IG.STRBIND (i, strid, strexp, strbind_opt) =>
	 let
	   val (E, out_strexp) = elab_strexp (B, strexp)
	   val (SE, out_strbind_opt) =
	         elab_X_opt (B B_plus_T (E.tynames E), strbind_opt)
		   elab_strbind SE.empty
	   val out_i = if EqSet.member strid (SE.dom SE)
		       then repeatedIdsError (i, [ErrorInfo.STRID_RID strid])
		       else okConv i
	 in
	   (SE.singleton (strid, E) SE_plus_SE SE,
	    OG.STRBIND (out_i, strid, out_strexp, out_strbind_opt))
	 end)

    (*****************************************************************)
    (* Signature Expressions, Definition v3 page 38, rules 63 and 64 *)
    (*****************************************************************)

    (*elaborate a sigexp to an E*)
    and elab_sigexp' (B : Basis, sigexp : IG.sigexp) : (Env * OG.sigexp) =
      (case sigexp of

	 (* Generative *)                                   (*rule 62*)
	 IG.SIGsigexp (i, spec) =>
	   let val (E, out_spec) = elab_spec (B, spec)
	   in
	     (E, OG.SIGsigexp (okConv i, out_spec))
	   end

	 (* Signature identifier *)                         (*rule 63*)
       | IG.SIGIDsigexp (i, sigid) =>
	   (case B.lookup_sigid B sigid of
	      Some sigma =>
		let val E = Sigma.instance sigma
		in
		  (E, OG.SIGIDsigexp (okConv i, sigid))
		end
	    | None =>
		(E.bogus,
		 OG.SIGIDsigexp (errorConv (i, ErrorInfo.LOOKUP_SIGID sigid), sigid)))

	                                                    (*rule 64*)
       | IG.WHERE_TYPEsigexp (i, sigexp, explicittyvars, longtycon, ty) =>  
	   let
	     val (E, out_sigexp) = elab_sigexp' (B, sigexp)
	     val alphas = map TyVar.from_ExplicitTyVar explicittyvars
	     val (tau, out_ty) = ElabDec.elab_ty (B.to_C B, ty)
	     fun return (E_result, out_i) =
	           (E_result, OG.WHERE_TYPEsigexp
		                (out_i, out_sigexp, explicittyvars, longtycon, out_ty))
	     fun fail error_info = return (E, errorConv (i, error_info))
	   in
	     (case E.lookup_longtycon E longtycon of
		None => fail (ErrorInfo.LOOKUP_LONGTYCON longtycon)
	      | Some tystr =>
		  let val (theta, VE) = TyStr.to_theta_and_VE tystr in
		    (case TypeFcn.to_TyName theta of
		       None => fail (ErrorInfo.WHERE_TYPE_NOT_TYNAME (longtycon, theta, tau))
		     | Some t =>
			 if TyName.Set.member t (B.to_T B)
			  then fail (ErrorInfo.WHERE_TYPE_RIGID (longtycon, t))
			 else if TyName.arity t <> List.size alphas
			  then fail (ErrorInfo.WHERE_TYPE_ARITY (alphas, (longtycon, t))) else
			 let val theta' = TypeFcn.from_TyVars_and_Type (alphas, tau)
			     val phi = Realisation.singleton (t, theta')
			     val phi_E = Realisation.on_Env phi E in
			 if TyName.equality t andalso not (TypeFcn.admits_equality theta')
			  then fail (ErrorInfo.WHERE_TYPE_EQTYPE (longtycon, t, tau))
			 else if not (wellformed_E phi_E)
			  then fail (ErrorInfo.WHERE_TYPE_NOT_WELLFORMED (longtycon, t, tau))
			  else return (phi_E, okConv i) end) end)(*justjokin*)
	   end)


    (*********************************************************)
    (* Signature Expressions, Definition v3 page 38, rule 65 *)
    (*********************************************************)

    (*elaborate a sigexp to a Sigma*)                       (*rule 65*)
    and elab_sigexp (B : Basis, sigexp : IG.sigexp) : (Sig * OG.sigexp) =
          let
	    val (E, out_sigexp) = elab_sigexp' (B, sigexp)
	    val T = TyName.Set.difference (E.tynames E) (B.to_T B)
	  in
	    (Sigma.from_T_and_E (T, E), out_sigexp)
	  end


    (*********************************************************)
    (* Signature Declarations - Definition v3 page  39       *)
    (*********************************************************)

	                                                    (*rule 66*)
    and elab_sigdec (B : Basis, sigdec : IG.sigdec) : (SigEnv * OG.sigdec) =
      (case sigdec of

	(* Single declaration *)
	IG.SIGNATUREsigdec (i, sigbind) =>
	  let
	    val (G, out_sigbind) = elab_sigbind (B, sigbind)
	  in
	    (G, OG.SIGNATUREsigdec (okConv i, out_sigbind))
	  end)


    (**********************************************)
    (* Signature Bindings - Definition v3 page 39 *)
    (**********************************************)

    and elab_sigbind (B : Basis, sigbind : IG.sigbind) : (SigEnv * OG.sigbind) =

      (case sigbind of

	(* Signature bindings *)                            (*rule 67*)
	IG.SIGBIND (i, sigid, sigexp, None) =>
	  let
	    val (sigma, out_sigexp) = elab_sigexp (B, sigexp)
	    val G = G.singleton (sigid, sigma)
	  in
	    (G, OG.SIGBIND (okConv i, sigid, out_sigexp, None))
	  end

	(* Signature bindings *)                            (*rule 67*)
      | IG.SIGBIND (i, sigid, sigexp, Some sigbind) =>
	  let
	    val (sigma, out_sigexp) = elab_sigexp (B, sigexp)
	    val G1 = G.singleton (sigid, sigma)
	    val (G2, out_sigbind) = elab_sigbind (B, sigbind)
	    val out_i = if EqSet.member sigid (G.dom G2)
			then repeatedIdsError (i, [ErrorInfo.SIGID_RID sigid])
			else okConv i
	  in
	    (G1 G_plus_G G2,
	     OG.SIGBIND (out_i, sigid, out_sigexp, Some out_sigbind))
	  end)

    (**********************************************)
    (* Specifications - Definition v3 pages 39-40 *)
    (**********************************************)

    and elab_spec (B : Basis, spec : IG.spec) : (Env * OG.spec) =

      (*TODO 23/01/1997 18:27. tho.: in the old ElabTopdec.elab_spec
       accumulating B's and E's were used to speed up things.  Maybe
       I should do the same.  For now I stick to the definition and
       return incremental E's which must explicitly be added to the
       basis, before the next spec is elaborated.*)
           
      (case spec of

	(* Value specification *)                           (*rule 68*)
	IG.VALspec (i, valdesc) =>
	  let
	    val _ = Level.push ()
	    val (VE, out_valdesc) = elab_valdesc (B.to_C B, valdesc)
	    val _ = Level.pop ()
	  in
	    (E.from_VE (VE.close VE), OG.VALspec (okConv i, out_valdesc))
	  end

	(* Type specification *)                            (*rule 69% *)
      | IG.TYPEspec (i, typdesc) =>
	  let val (TE, out_typdesc) = elab_typdesc false (B.to_C B, typdesc)
	  in
	    (E.from_TE TE,  OG.TYPEspec (okConv i, out_typdesc))
	  end

	(* Equality type specification *)
      | IG.EQTYPEspec (i, typdesc) =>                       (*rule 70*)
	  let val (TE, out_typdesc) = elab_typdesc true (B.to_C B, typdesc)
	  in
	    (E.from_TE TE,  OG.EQTYPEspec (okConv i, out_typdesc))
	  end

	(* Datatype specification *)                        (*rule 71*)
      | IG.DATATYPEspec (i, datdesc) =>
	  let
	    val TE = initial_TE datdesc
	    val ((VE, TE), out_datdesc) =
	            elab_datdesc (C.cplus_TE (B.to_C B, TE), datdesc)
	    val (VE, TE) = Environments.maximise_equality_in_VE_and_TE (VE, TE)
	  in
	    (E.from_VE_and_TE (VE,TE),
	     OG.DATATYPEspec (okConv i, out_datdesc))
	  end

	(*Datatype replication specification*)              (*rule 72*)
      | IG.DATATYPE_REPLICATIONspec (i, tycon, longtycon) => 
	  (case B.lookup_longtycon B longtycon of
	     Some tystr =>
	       let val (theta, VE) = TyStr.to_theta_and_VE tystr
		   val TE = TE.singleton (tycon, tystr)
	       in
		 (E.from_VE_and_TE (VE,TE),
		  OG.DATATYPE_REPLICATIONspec (okConv i, tycon, longtycon))
	       end
	   | None =>
	       (E.bogus,
		OG.DATATYPE_REPLICATIONspec
		  (errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon),
		   tycon, longtycon)))

	(* Exception specification *)                       (*rule 73*)
      | IG.EXCEPTIONspec (i, exdesc) =>
	  let val (VE, out_exdesc) = elab_exdesc (B.to_C B, exdesc) 
	  in
	    (E.from_VE VE, OG.EXCEPTIONspec (okConv i, out_exdesc))
	  end

	(* Structure specification *)                       (*rule 74*)
      | IG.STRUCTUREspec (i, strdesc) =>
	  let val (SE, out_strdesc) = elab_strdesc (B, strdesc)
	  in
	    (E.from_SE SE,  OG.STRUCTUREspec (okConv i, out_strdesc))
	  end

	(* Include specification *)                         (*rule 75*)
      | IG.INCLUDEspec (i, sigexp) =>
	  let val (E, out_sigexp) = elab_sigexp' (B, sigexp)
	  in
	    (E, OG.INCLUDEspec (okConv i, out_sigexp))
	  end

	(* Empty specification *)                           (*rule 76*)
      | IG.EMPTYspec i => (E.empty,  OG.EMPTYspec (okConv i))

	(* Sequential specification *)                      (*rule 77*)
      | IG.SEQspec (i, spec1, spec2) =>
	  let
	    val (E1, out_spec1) = elab_spec (B, spec1)
	    val (E2, out_spec2) = elab_spec (B B_cplus_E E1, spec2)
	    val repeated_ids =
		   map ErrorInfo.STRID_RID
		     (EqSet.list
		        (EqSet.intersect (SE.dom (E.to_SE E1)) (SE.dom (E.to_SE E2))))
		   @
		   map ErrorInfo.TYCON_RID
		     (EqSet.list
		        (EqSet.intersect (TE.dom (E.to_TE E1)) (TE.dom (E.to_TE E2))))
		   @
		   map ErrorInfo.ID_RID
		     (EqSet.list
		        (EqSet.intersect (VE.dom (E.to_VE E1)) (VE.dom (E.to_VE E2))))		 
	    val out_i = (case repeated_ids of
			   [] => okConv i
			 | repeated_ids => repeatedIdsError (i, repeated_ids))
	  in
	    (E.plus (E1,E2), OG.SEQspec (out_i, out_spec1, out_spec2))
	  end
	                                                    (*rule 78*)
      | IG.SHARING_TYPEspec (i, spec, longtycon_withinfo_s) =>
	  let
	    val (E, out_spec) = elab_spec (B, spec)
	    val (T, out_longtycon_withinfo_s) =
	           List.foldR
		   (fn IG.WITH_INFO (i, longtycon_i) =>
		    fn (T, out_longtycon_withinfo_s) =>
		     let val (T, out_i) =
		           (case E.lookup_longtycon E longtycon_i of
			      Some tystr_i =>
				let val (theta_i, VE_i) = TyStr.to_theta_and_VE tystr_i
				in
				  (case TypeFcn.to_TyName theta_i of
				     Some t_i =>
				       if TyName.Set.member t_i (B.to_T B) then
					 (T, errorConv (i, ErrorInfo.SHARING_TYPE_RIGID
							     (longtycon_i, t_i)))
				       else
					 (TyName.Set.insert t_i T, okConv i)
				   | None => (T, errorConv (i, ErrorInfo.SHARING_TYPE_NOT_TYNAME
							           (longtycon_i, theta_i))))
				end
			    | None =>
				(T, errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon_i)))
		     in
		       (T, OG.WITH_INFO (out_i, longtycon_i) :: out_longtycon_withinfo_s)
		     end)
		          (TyName.Set.empty, []) longtycon_withinfo_s
	  in
	    (*if everything is allright, T will have at least one member:*)
	    (case TyName.Set.list T of
	       [] => (E, OG.SHARING_TYPEspec
		           (okConv i, out_spec, out_longtycon_withinfo_s))
	     | ts as t1::ts' =>
		 let val arity = TyName.arity t1
		 in
		   if List.forAll (fn t => TyName.arity t = arity) ts' then
		     let val t = TyName.freshTyName
		                   {tycon = TyName.tycon t1, arity = arity, 
				    equality = List.exists TyName.equality ts}
			 val phi = Realisation.from_T_and_theta
			             (T, TypeFcn.from_TyName t)
		     in
		       (Realisation.on_Env phi E,
			OG.SHARING_TYPEspec (okConv i, out_spec, out_longtycon_withinfo_s))
		     end
		   else
		     (E, OG.SHARING_TYPEspec
		           (errorConv (i, ErrorInfo.SHARING_TYPE_ARITY ts),
			    out_spec, out_longtycon_withinfo_s))
		 end)
	  end
      
      | IG.SHARINGspec (i, spec, longstrid_withinfo_s) =>
	  let val (E, out_spec) = elab_spec (B, spec)
	      val (Es, out_longstrid_withinfo_s) =
		     List.foldR
		     (fn IG.WITH_INFO (i, longstrid) =>
		      (fn (Es, out_longstrid_withinfo_s) =>

		       (case E.lookup_longstrid E longstrid of
			  Some E_i => (E_i::Es,
				       OG.WITH_INFO (okConv i, longstrid)
				       ::out_longstrid_withinfo_s)
			| None => (Es, OG.WITH_INFO
				         (errorConv (i, ErrorInfo.LOOKUP_LONGSTRID longstrid),
					  longstrid)::out_longstrid_withinfo_s))))
		        ([],[]) longstrid_withinfo_s
	  in
	    (Realisation.on_Env (share (fn t => TyName.Set.member t (B.to_T B)) Es) E,
	     OG.SHARINGspec (okConv i, out_spec, out_longstrid_withinfo_s))
	    handle Share error_info =>
	    (E, OG.SHARINGspec (errorConv (i, error_info), out_spec,
				out_longstrid_withinfo_s))
	  end)


    (**********************************************)
    (* Value Descriptions - Definition v3 page 41 *)
    (**********************************************)

    and elab_valdesc (C : Context, valdesc : IG.valdesc)
      : (VarEnv * OG.valdesc) =
	                                                    (*rule 79*)
      (case valdesc of
	IG.VALDESC (i, id, ty, valdesc_opt) =>
	  let
	    val (tau, out_ty) = ElabDec.elab_ty (C, ty)
	    val sigma = TypeScheme.from_Type tau
	    val (VE, out_valdesc_opt) =
	          elab_X_opt (C, valdesc_opt) elab_valdesc VE.empty
	    val out_i = if EqSet.member id (VE.dom VE)
			then repeatedIdsError (i, [ErrorInfo.ID_RID id])
			else if IG.DecGrammar.is_'true'_'nil'_etc id
			     then errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [id])
			     else okConv i
	  in
	    (VE.plus (VE.singleton_var (id, sigma), VE),
	     OG.VALDESC (out_i, id, out_ty, out_valdesc_opt))
	  end)
(*KILL 24/06/1997 15:43. tho.:
      (case valdesc of
	IG.VALDESC (i, id, ty, None) =>
	  let
	    val (tau, out_ty) = ElabDec.elab_ty (C, ty)
	    val sigma = TypeScheme.from_Type tau
	  in
	    (VE.singleton_var (id, sigma),
	     OG.VALDESC (okConv i, id, out_ty, None))
	  end

      | IG.VALDESC (i, id, ty, Some valdesc) =>
	  let
	    val (tau, out_ty) = ElabDec.elab_ty (C, ty)
	    val sigma = TypeScheme.from_Type tau
	    val (VE, out_valdesc) = elab_valdesc (C, valdesc)
	    val out_i = if EqSet.member id (VE.dom VE)
			then repeatedIdsError (i, [ErrorInfo.ID_RID id])
			else okConv i
	  in
	    (VE.plus (VE.singleton_var (id, sigma), VE),
	     OG.VALDESC (out_i, id, out_ty, Some out_valdesc))
	  end)
*)

    (*********************************************)
    (* Type Descriptions - Definition v3 page 41 *)
    (*********************************************)

    and elab_typdesc (equality : bool) (C : Context, typdesc : IG.typdesc)
      : (TyEnv * OG.typdesc) =
	                                                    (*rule 80*)
       (case typdesc of
	 IG.TYPDESC (i, explicittyvars, tycon, typdesc_opt) =>
	   let
	     val tyvars = map TyVar.from_ExplicitTyVar explicittyvars
	     val tyvars_repeated = repeaters TyVar.eq tyvars
	     val arity = List.size explicittyvars
	     val t = TyName.freshTyName
	               {tycon=tycon, arity=arity, equality=equality}
	     val theta = TypeFcn.from_TyName t
	     val tystr = TyStr.from_theta_and_VE (theta, VE.empty)
	     val (TE, out_typdesc_opt) = elab_X_opt (C, typdesc_opt)
	                                   (elab_typdesc equality) TE.empty
	     val out_i = if EqSet.member tycon (TE.dom TE) then
			    repeatedIdsError (i, [ErrorInfo.TYCON_RID tycon])
			  else if not (isEmptyTyVarList tyvars_repeated) then
			    repeatedIdsError
			      (i, map ErrorInfo.TYVAR_RID tyvars_repeated)
			       else okConv i
	   in
	     (TE.plus (TE.singleton (tycon, tystr), TE),
	      OG.TYPDESC (out_i, explicittyvars, tycon, out_typdesc_opt))
	   end)

    (*************************************************)
    (* Datatype Descriptions - Definition v3 page 42 *)
    (*************************************************)
	                                                    (*rule 81*)
    and elab_datdesc (C : Context, datdesc : IG.datdesc)
      : ((VarEnv * TyEnv) * OG.datdesc) =
        (case datdesc of
	  IG.DATDESC (i, explicittyvars, tycon, condesc, datdesc_opt) =>
	    let
	      val _ = Level.push()

	      val tyvars = map TyVar.from_ExplicitTyVar explicittyvars
	      val (theta, _) = TyStr.to_theta_and_VE
		                 (noSome (C.lookup_tycon C tycon) "datdesc(1)")
	      val t = noSome (TypeFcn.to_TyName theta) "datdesc(2)"
	      val taus = map Type.from_TyVar tyvars
	      val tau = Type.from_ConsType (Type.mk_ConsType (taus, t))
	      val (constructor_map, out_condesc) = elab_condesc (C, tau, condesc)
	      val VE = constructor_map.to_VE constructor_map
		    
	      (*The following lists must be made before closing VE,
	       as explicit tyvars are turned into ordinary tyvars when
	       closed.*)

	      val tyvars_repeated = repeaters TyVar.eq tyvars
	      val tyvars_not_bound =
		    List.all 
		      (fn tyvar => not (memberTyVarList tyvar tyvars)) 
		         (map TyVar.from_ExplicitTyVar 
			        (IG.getExplicitTyVarsCondesc condesc))

	      val _ = Level.pop()
	      val VE_closed = VE.close VE
	      val theta = TypeFcn.from_TyName t
	      val tystr = TyStr.from_theta_and_VE (theta, VE_closed)

	      val ((VE', TE'), out_datdesc_opt) =
		    elab_X_opt (C, datdesc_opt) elab_datdesc
		      (VE.empty, TE.empty)
	      val repeated_constructors =
		    EqSet.list (EqSet.intersect (VE.dom VE_closed) (VE.dom VE'))
	    in
	      (case (if EqSet.member tycon (TE.dom TE')
		       then [ErrorInfo.TYCON_RID tycon] else [])
		    @ map ErrorInfo.ID_RID repeated_constructors
		    @ map ErrorInfo.TYVAR_RID tyvars_repeated of
		 [] =>
	      (case tyvars_not_bound of
		 [] => 
		   ( (VE.plus (VE_closed, VE'),
		      TE.plus (TE.singleton (tycon, tystr), TE')) ,
		     OG.DATDESC (if TyCon.is_'true'_'nil'_etc tycon then
				   errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [])
				 else if TyCon.is_'it' tycon then
				   errorConv (i, ErrorInfo.SPECIFYING_IT)
				 else okConv i,
				 explicittyvars, tycon, out_condesc,
				 out_datdesc_opt) )
	       | _ => 
		   ( (VE', TE') ,
		     OG.DATDESC (errorConv
				  (i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ
				        tyvars_not_bound),
				 explicittyvars, tycon, out_condesc,
				 out_datdesc_opt) ))
	       | repeated_ids_errorinfos => 
		   ( (VE', TE') ,
		     OG.DATDESC (repeatedIdsError (i, repeated_ids_errorinfos),
				 explicittyvars, tycon, out_condesc,
				 out_datdesc_opt) ))
	    end)

    (****************************************************)
    (* Constructor Descriptions - Definition v3 page 42 *)
    (****************************************************)

    and elab_condesc (C : Context, tau : Type, condesc : IG.condesc)
      : (constructor_map * OG.condesc) =

       (case condesc of
	                                                    (*rule 82*)
	 IG.CONDESC (i, con, None, condesc_opt) =>
	   let
	     val sigma = TypeScheme.from_Type tau
	     val (constructor_map, out_condesc_opt) =
	           elab_condesc_opt(C, tau, condesc_opt)
	   in
	     (constructor_map.add con sigma constructor_map,
	      OG.CONDESC (out_i_for_condesc con constructor_map i, con,
			  None, out_condesc_opt))
	   end

       | IG.CONDESC (i, con, Some ty, condesc_opt) =>
	   let
	     val (tau', out_ty) = ElabDec.elab_ty (C, ty)
	     val arrow = Type.mk_Arrow (tau', tau)
	     val sigma = TypeScheme.from_Type arrow
	     val (constructor_map, out_condesc_opt) =
	           elab_condesc_opt (C, tau, condesc_opt)
	   in
	     (constructor_map.add con sigma constructor_map,
	      OG.CONDESC (out_i_for_condesc con constructor_map i, con,
			  Some out_ty, out_condesc_opt))
	   end)

    and out_i_for_condesc con constructor_map i = 
          if constructor_map.in_dom con constructor_map
	  then repeatedIdsError (i, [ErrorInfo.CON_RID con])
	  else if IG.DecGrammar.is_'true'_'nil'_etc con
	       then errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [con])
	       else if IG.DecGrammar.is_'it' con
		    then errorConv (i, ErrorInfo.SPECIFYING_IT)
		    else okConv i

    and elab_condesc_opt
      (C : Context, tau : Type, condesc_opt : IG.condesc Option)
      : (constructor_map * OG.condesc Option) =
          elab_X_opt (C, condesc_opt) 
            (fn (C, condesc) => elab_condesc (C, tau, condesc))
	      constructor_map.empty

    (**************************************************)
    (* Exception Descriptions - rule 83             *)
    (**************************************************)

    and elab_exdesc (C : Context, exdesc : IG.exdesc)
      : (VarEnv * OG.exdesc) =
	                                                    (*rule 83*)
       (case exdesc of
	 IG.EXDESC (i, excon, None, exdesc_opt) =>
	   let val (VE, out_exdesc_opt) =
	             elab_X_opt (C, exdesc_opt) elab_exdesc VE.empty
	   in
	     (VE.plus (VE.singleton_excon (excon, Type.Exn), VE),
	      OG.EXDESC (out_i_for_exdesc excon VE i, excon, None,
			 out_exdesc_opt))
	   end
	| IG.EXDESC (i, excon, Some ty, exdesc_opt) =>
	   let
	     val (tau, out_ty) = ElabDec.elab_ty (C, ty)
	     val tyvars = Type.tyvars tau
	     val arrow = Type.mk_Arrow (tau, Type.Exn)
	     val (VE, out_exdesc_opt) =
	           elab_X_opt (C, exdesc_opt) elab_exdesc VE.empty
	     val out_i = (case tyvars of
			    [] => out_i_for_exdesc excon VE i
	                  | _ => errorConv (i, ErrorInfo.EXDESC_SIDECONDITION))
	   in
	     (VE.plus (VE.singleton_excon (excon, arrow), VE),
	      OG.EXDESC (out_i, excon, Some out_ty, out_exdesc_opt))
	   end)

    and out_i_for_exdesc excon VE i =
          if EqSet.member excon (VE.dom VE)
	  then repeatedIdsError (i, [ErrorInfo.EXCON_RID excon])
	  else if IG.DecGrammar.is_'true'_'nil'_etc excon
	       then errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [excon])
	       else if IG.DecGrammar.is_'it' excon
		    then errorConv (i, ErrorInfo.SPECIFYING_IT)
		    else okConv i

    (***************************************************)
    (* Structure Desctriptions - Definition v3 page 41 *)
    (***************************************************)

    and elab_strdesc (B : Basis, strdesc : IG.strdesc)
      : (StrEnv * OG.strdesc) =
	                                                    (*rule 84*)
      (case strdesc of
	 IG.STRDESC (i, strid, sigexp, None) =>
	   let val (E, out_sigexp) = elab_sigexp' (B, sigexp)
	   in
	     (SE.singleton (strid, E),
	      OG.STRDESC (okConv i, strid, out_sigexp, None))
	   end
       | IG.STRDESC (i, strid, sigexp, Some strdesc) =>
	   let
	     val (E, out_sigexp) = elab_sigexp' (B, sigexp)
	     val (SE, out_strdesc) =
	           elab_strdesc (B.plus_T (B, E.tynames E), strdesc)
	     val out_i = if EqSet.member strid (SE.dom SE)
			 then repeatedIdsError (i, [ErrorInfo.STRID_RID strid])
			 else okConv i
	   in
	     (SE.singleton (strid, E) SE_plus_SE SE,
	      OG.STRDESC (out_i, strid, out_sigexp, Some out_strdesc))
	   end)



    (****************************************************)
    (* Functor Declarations - Definition v3 pages 42-43 *)
    (****************************************************)

    and elab_fundec (B : Basis, fundec : IG.fundec)
      : (FunEnv * OG.fundec) =
	                                                    (*rule 85*)
      (case fundec of
	 IG.FUNCTORfundec (i, funbind) =>
	   let val (F, out_funbind) = elab_funbind (B, funbind)
	   in
	     (F, OG.FUNCTORfundec(okConv i, out_funbind))
	   end)

    (********************************************)
    (* Functor Bindings - Definition v3 page 43 *)
    (********************************************)

    and elab_funbind (B : Basis, funbind : IG.funbind)
      : (FunEnv * OG.funbind) =
	                                                    (*rule 86*)
      (case funbind of
	IG.FUNBIND (i, funid, strid, sigexp, strexp, funbind_opt) =>
	  let
	    val (T_E, out_sigexp) = elab_sigexp (B, sigexp)
	    val (T, E) = Sigma.to_T_and_E T_E
	    val (E', out_strexp) =
	          elab_strexp
		    (B.cplus_E (B, E.from_SE (SE.singleton (strid,E))) ,
		     strexp)
	    val T' = TyName.Set.difference (E.tynames E')
	                                   (TyName.Set.union (B.to_T B) T)
	    val T'E' = Sigma.from_T_and_E (T',E')
	    val (F, out_funbind_opt) =
	          elab_X_opt (B, funbind_opt) elab_funbind F.empty
	    val out_i = if EqSet.member funid (F.dom F)
			then repeatedIdsError (i, [ErrorInfo.FUNID_RID funid])
			else okConv i
	  in
	    (F.singleton (funid, Phi.from_T_and_E_and_Sigma (T, E, T'E')) F_plus_F F,
	     OG.FUNBIND (out_i, funid, strid, out_sigexp, out_strexp,
			 out_funbind_opt))
	  end)


    (************************************************************)
    (* Top-level Declarations - Definition 1997, rules 87-89    *)
    (************************************************************)

    and elab_topdec (B : Basis, topdec : IG.topdec)
          : (Basis * OG.topdec) =
      let
	(*Rules 87-89 are quite alike and elab_topdec0 implements all
	 three of them.  The difference is the kind of topdec (strdec,
	 sigdec or fundec) and the kind of environment (E, G or F).  The
	 only other difference is that there should be no tyvars check
	 on a sigdec; this difference I ignore, i.e., a sigdec is
	 checked.  Is that a problem?

	 I'm sorry about this gigantic let, I couldn't get it to
	 typecheck when elab_topdec and elab_topdec0 were declared with
	 an `and' in between.*)

	fun elab_topdec0
	     (B : Basis)
	     (i : IG.info, X : 'X, topdec_opt : IG.topdec Option)
	     (*'X is an IG.strdec, IG.fundec, or IG.sigdec*)
	     (elab_X : Basis * 'X -> 'EGF * 'out_X)
	     (*'out_X is like 'X, except it is `OG.' instead of `IG.', and
	      'EGF is an E, F, or G*)
	     (B_cplus_EGF : Basis * 'EGF -> Basis)
	     (EGF_tynames : 'EGF -> TyName.Set.Set)
	     (B_from_T_and_EGF : TyName.Set.Set * 'EGF -> Basis)
	     (Xtopdec : OG.info * 'out_X * OG.topdec Option -> OG.topdec)
	     (*Xtopdec is OG.STRtopdec, OG.SIGtopdec or OG.FUNtopdec*)
	     : Basis * OG.topdec =
	  let
	    val (EGF, out_X) = elab_X (B, X)
	    val (B', out_topdec_opt) =
	          elab_X_opt (B_cplus_EGF(B,EGF), topdec_opt)
		    elab_topdec B.empty
	    val B'' = B.plus (B_from_T_and_EGF (EGF_tynames EGF, EGF), B')
	  in
	    (*TODO: it may not be that smart to run through the whole
	     basis with tyvarsB every time a new topdec is added, as I do here:
	     04/12/1996 14:29. tho.*)
	    (case B.tyvars' B'' of
	       [] => (B'', Xtopdec (okConv i, out_X, out_topdec_opt))
	     | criminals : (IG.DecGrammar.Ident.id * TyVar list) list =>
		 let val tyvars = List.foldL (General.curry op @ o #2) [] criminals
		 in
		   (case List.all (is_Some o TyVar.to_ExplicitTyVar) tyvars of
		      [] =>
			(List.apply
		         Type.instantiate_arbitrarily tyvars ;
			 Flags.warnings :=
			   ("Free type variables are not allowed at top-level;\n\
			    \see `The Definition of Standard ML (Revised)', section G.8.\n\
			    \I substituted int for them in the type for "
			    ^ pp_list (quote o IG.DecGrammar.Ident.pr_id o #1) criminals
			    ^ ".\n")
			   :: !Flags.warnings ;
			 (B'', Xtopdec (okConv i, out_X, out_topdec_opt)))
		    | unguarded_tyvars : TyVar list =>
			(B.bogus,
			 Xtopdec (errorConv
				    (i, ErrorInfo.UNGUARDED_TYVARS unguarded_tyvars),
				    (*one could extend the error info to also contain
				     the criminal id's?*)
				  out_X, out_topdec_opt)))
		 end)
	  end
      in
	(case topdec of
	   IG.STRtopdec strdec_etc =>
	     elab_topdec0 B strdec_etc elab_strdec B.cplus_E
	       E.tynames B.from_T_and_E OG.STRtopdec
	 | IG.SIGtopdec sigdec_etc =>
	     elab_topdec0 B sigdec_etc elab_sigdec B.cplus_G
	       G.tynames B.from_T_and_G OG.SIGtopdec
	 | IG.FUNtopdec fundec_etc =>
	     elab_topdec0 B fundec_etc elab_fundec B.cplus_F
	       F.tynames B.from_T_and_F OG.FUNtopdec)
      end (*let*)
  
    (********
    Printing functions
    ********)

    val layoutStaticBasis = B.layout
  end;
