(*************************************************************)
(* Elaboration of top-level declarations, modules included,  *)
(*   Section 5 of the Definition, v3                         *)
(*************************************************************)

(*$ElabTopdec: TOPDEC_GRAMMAR ELABDEC ENVIRONMENTS STATOBJECT
	MODULE_STATOBJECT MODULE_ENVIRONMENTS STRID SIGID NAME
	ELAB_REPOSITORY PARSE_INFO ELAB_INFO PRETTYPRINT CRASH FLAGS
	FINMAP ELABTOPDEC OrderSet*)

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
   sharing type ElabInfo.TypeInfo.strid = StrId.strid
       and type ElabInfo.TypeInfo.TyName = StatObject.TyName

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
     sharing type OG.tycon = IG.tycon = ElabInfo.TypeInfo.tycon
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
     sharing type ModuleStatObject.StringTree = PrettyPrint.StringTree
         and type ModuleStatObject.SigMatchError = ElabInfo.ErrorInfo.SigMatchError

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
         and type ModuleEnvironments.Basis = ElabInfo.TypeInfo.Basis

   structure Name : NAME
     sharing type Name.name = StatObject.TyName.name

   structure ElabRep : ELAB_REPOSITORY
     sharing ElabRep.TyName = ModuleEnvironments.TyName
         and type ElabRep.name = Name.name
         and type ElabRep.ElabBasis = ModuleEnvironments.Basis
	 and type ElabRep.funid = IG.funid
	 and type ElabRep.realisation = Environments.realisation
   structure ElabDec : ELABDEC
     sharing type ElabDec.PreElabDec = IG.dec
     sharing type ElabDec.PostElabDec = OG.dec
     sharing type ElabDec.PreElabTy = IG.ty
     sharing type ElabDec.PostElabTy = OG.ty
     sharing type ElabDec.Env = Environments.Env
     sharing type ElabDec.Context = Environments.Context
     sharing type ElabDec.Type = StatObject.Type
     sharing type ElabDec.TyName = StatObject.TyName

   structure FinMap : FINMAP
   structure Flags : FLAGS
   structure Crash : CRASH
     ) : ELABTOPDEC  =
  struct
    fun impossible s = Crash.impossible ("ElabTopdec." ^ s)
    fun noSome None s = impossible s
      | noSome (Some x) s = x
    fun quote s = "`" ^ s ^ "'"
    fun is_Some None = false
      | is_Some (Some x) = true
    val StringTree_to_string = PrettyPrint.flatten1
    fun pp_list pp_x [] = ""
      | pp_list pp_x [x] = pp_x x
      | pp_list pp_x [x,x'] = pp_x x ^ " and " ^ pp_x x'
      | pp_list pp_x (x::xs) = pp_x x ^ ", " ^ pp_list pp_x xs

    (*import from StatObject:*)
    structure Level        = StatObject.Level
    structure TyVar        = StatObject.TyVar
         type TyVar        = StatObject.TyVar
    structure TyName       = StatObject.TyName
         type TyName       = TyName.TyName
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

    structure Ident        = IG.DecGrammar.Ident
    structure TyCon        = IG.DecGrammar.TyCon
    structure ErrorInfo    = ElabInfo.ErrorInfo
    structure TypeInfo     = ElabInfo.TypeInfo

    infixr oo                           fun R1 oo R2 = Realisation.oo(R1,R2)
    infixr B_plus_E                     fun B B_plus_E E = B.plus_E(B,E)
    infixr B_plus_B                     fun B B_plus_B B' = B.plus(B,B')
    infixr G_plus_G                     fun G1 G_plus_G G2 = G.plus(G1,G2)
    infixr B_plus_G                     fun B B_plus_G G = B.plus_G(B,G)
    infixr E_plus_E                     fun E1 E_plus_E E2 = E.plus(E1,E2)
    infixr F_plus_F                     fun F1 F_plus_F F2 = F.plus(F1,F2)
    infixr B_plus_F                     fun B B_plus_F F = B.plus_F(B,F)
    infixr SE_plus_SE                   fun SE1 SE_plus_SE SE2 = SE.plus(SE1,SE2)

					local val a = op @
					in fun op @ x = a x
					end
    fun tynames_E E = E.tynames E
    fun tynames_G G = G.tynames G
    fun tynames_F F = F.tynames F
    fun tyvars_Type tau = Type.tyvars tau
    fun tyvars_E' E = E.tyvars' E
    fun tyvars_F' F = F.tyvars' F

      (*the following three types are for the signature:*)
    type PreElabTopdec  = IG.topdec
    type PostElabTopdec = OG.topdec
    type StaticBasis = ModuleEnvironments.Basis

    (* debugging stuff *)
    fun pr_Env E = PrettyPrint.outputTree (print, E.layout E, 120)

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


    (*Sigma_match: We rename flexible names in error_result so
     that the result structure returned in the event of an error
     is as general as possible:*)

    fun Sigma_match (i, Sigma, E) =
      (okConv i, Sigma.match (Sigma, E))
      handle No_match reason =>
	(errorConv (i, ErrorInfo.SIGMATCH_ERROR reason), Sigma.instance Sigma)

    fun Sigma_match' (i, Sigma, E) =                                            (* The realisation maps abstract type names *)
      let val (E_trans_result, E_opaque_result, phi) = Sigma.match' (Sigma, E)  (* E_opaque_result into what they stand for *)
      in (okConv i, E_trans_result, E_opaque_result, phi)                       (* in E_trans_result. *)
      end handle No_match reason =>
	let val E = Sigma.instance Sigma
	in (errorConv (i, ErrorInfo.SIGMATCH_ERROR reason), E, E, Realisation.Id)
	end

    (*initial_TE datdesc = the TE to be used initially in
     elaborating a datdesc. We determine the correct equality
     attributes when we maximise equality:*)

    fun initial_TE (IG.DATDESC (_, explicittyvars, tycon, _, opt)) =
      let val TE = TE.init explicittyvars tycon
      in case opt
	   of None => TE
	    | Some datdesc => let val TE' = initial_TE datdesc
			      in TE.plus(TE,TE')
			      end
      end

    fun Sigma_instance' a = Sigma.instance' a

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


 structure StructureSharing =
   struct

    exception Share of ErrorInfo.ErrorInfo

    fun update(a,b,m) = case FinMap.lookup m a
				  of Some bs => FinMap.add(a,b::bs,m)
				   | None => FinMap.add(a,[b],m)

    (* We first collect a list of tyname lists which must be identified. *)

    fun collect_TE (flexible : TyName -> bool, path : strid list, TEs, acc) : TyName list list =
      let val tcmap = List.foldL (fn TE => fn acc => TE.Fold(fn (tycon, tystr) => fn acc =>
							     update(tycon,tystr,acc)) acc TE) FinMap.empty TEs

	  (* Eliminate entries with less than two component, check
	   * arities and flexibility of involved tynames. Further,
	   * extract tynames from type structures. *)

      in
	    FinMap.Fold(fn ((tycon, []),acc) => acc
	                 | ((tycon, [tystr]), acc) => acc
			 | ((tycon, tystrs), acc) =>
			let fun tystr_to_tyname tystr =
			      let val theta = TyStr.to_theta tystr
			      in case TypeFcn.to_TyName theta
				   of None => raise Share (ErrorInfo.SHARING_TYPE_NOT_TYNAME
							   (TyCon.implode_LongTyCon (rev path, tycon), theta))
				    | Some t => t
			      end 
			    val tynames = map tystr_to_tyname tystrs   (* we know that there are more than zero *)
			    val arity = case tynames
					  of t :: _ => TyName.arity t
					   | _ => impossible "SHAREspec.collect"
			    val _ = List.apply (fn t => if TyName.arity t = arity then
						           if flexible t then ()
							   else raise Share (ErrorInfo.SHARING_TYPE_RIGID
									     (TyCon.implode_LongTyCon (rev path, tycon), t))
							else raise Share (ErrorInfo.SHARING_TYPE_ARITY tynames)) tynames
			in tynames::acc
			end) acc tcmap
      end

    fun collect_E (flexible, path, Es, acc) : TyName list list =
      let val (SEs, TEs) = List.foldL(fn E => fn (SEs, TEs) =>
				      let val (SE,TE,VE) = E.un E
				      in (SE::SEs,TE::TEs)
				      end) ([],[]) Es
	  val acc = collect_SE(flexible, path, SEs, acc)
      in collect_TE(flexible, path, TEs, acc)
      end

    and collect_SE (flexible, path, SEs, acc) : TyName list list =
      let val smap = List.foldL (fn SE => fn acc => SE.Fold(fn (strid, E) => fn acc =>
							    update(strid,E,acc)) acc SE) FinMap.empty SEs
      in
	    FinMap.Fold(fn ((strid, []), acc) => acc  	  (* Eliminate entries with *)
	                 | ((strid, [E]), acc) => acc     (* less than two component. *)
			 | ((strid, Es), acc) => collect_E(flexible,strid::path,Es,acc))
	    acc smap
      end


    (* Collapse tynames set if any candidates identifies two such *)

    fun collapse ([], Ts) : TyName.Set.Set list = Ts
      | collapse (T::Ts, Ts') =
      let fun split ([], no, yes) = (no, yes)
	    | split (T'::Ts'', no, yes) =
	      if TyName.Set.isEmpty(TyName.Set.intersect T' T) then split(Ts'',T'::no, yes)
	      else split(Ts'',no, T'::yes)
      in case split(Ts,[],[])
	   of (no, []) => collapse(no, T::Ts')
	    | (no, yes) => 
	     let val Tnew = List.foldL(fn T => fn T' => TyName.Set.union T T') T yes
	     in collapse(Tnew::no, Ts')
	     end
      end

    (* Find a representative; if everything is allright, T 
     * will have at least one member. *)

      fun find ([], acc) = impossible "IG.SHARINGspec.find"
	| find ([t], acc) = (t, acc)
	| find (t::ts, acc) = if TyName.equality t then (t, ts @ acc)
			      else find (ts,t::acc)

    (* Build up a realisation with plus; this is possible since we
     * know all components are now distinct. *)

    fun build [] : TyName list * realisation = ([], Realisation.Id)
      | build (T::Ts) = let val (t, T') = find(T,[])
			    val phi = Realisation.from_T_and_theta (TyName.Set.fromList T', TypeFcn.from_TyName t) 
			    val (T'', phi') = build Ts
			in (T' @ T'', phi' oo phi)
			end

    fun share (flexible, Es : Env list) : TyName list * realisation =
      let val Ts = map TyName.Set.fromList (collect_E(flexible,[],Es,[]))
	  val Ts : TyName list list = map TyName.Set.list (collapse(Ts,[]))
      in build Ts
      end

   end

     val share = StructureSharing.share
     exception Share = StructureSharing.Share

(*
    (*structure sharing*)
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

      (* Find a representative; if everything is allright, T 
       * will have at least one member. *)

      fun find ([], acc) = impossible "IG.SHARINGspec.find"
	| find ([t], acc) = (t, acc)
	| find (t::ts, acc) = if TyName.equality t then (t, ts @ acc)
			      else find (ts,t::acc)

      fun phi_that_identifies_tynames T : TyName list * realisation = (* also returns the support *)
	let val (t, T') = find(T, []) 
	    val arity = TyName.arity t 	    (* check arities *)
	in if List.forAll (fn t => TyName.arity t = arity) T' then
	     (T', Realisation.from_T_and_theta (TyName.Set.fromList T', TypeFcn.from_TyName t))
	   else raise Share (ErrorInfo.SHARING_TYPE_ARITY T)
		 (*TODO 07/03/1997 19:09. tho.  that^ does not
		  result in a terribly informative error message.
		  It will have todo for now.*)
	end

      (*the `strids' argument is the current path, only
       needed for error messaging; the `tyname_rigid' argument is passed
       around everywhere only to be available in `tystr_to_tyname'*)

      fun traverse tyname_rigid strids (Trie (tycon_map, strid_map)) : TyName list * realisation =
	let val (T1,phi1) = tycon_traverse tyname_rigid strids tycon_map
	    val (T2,phi2) = strid_traverse tyname_rigid strids strid_map
	in (T1 @ T2, phi1 oo phi2)
	end

      and tycon_traverse tyname_rigid strids tycon_map =
	    FinMap.Fold
	      (fn ((tycon, tystrs), (T, phi)) =>
	       let val (T', phi') = phi_that_identifies_tynames (map (tystr_to_tyname tyname_rigid strids tycon) tystrs)
	       in (T @ T', phi oo phi')
	       end) ([], Realisation.Id) tycon_map

      and strid_traverse tyname_rigid strids strid_map =
	    FinMap.Fold
	      (fn ((strid, trie), (T,phi)) =>
	       let val (T',phi') = traverse tyname_rigid (strid::strids) trie
	       in (T @ T', phi oo phi')
	       end) ([], Realisation.Id) strid_map
    in
      fun share tyname_rigid (Es : Env list) : TyName list * realisation =
	    traverse tyname_rigid [] (List.foldL insert_E trie0 Es)
    end (*local*)
*)

    (* ------------------------------------------------
     * Match object in repository (for recompilation)
     * ------------------------------------------------ *)

    fun match_and_update_repository (funid,T',E') : unit =
      let val N' = map TyName.name (TyName.Set.list T')
	  val B' = B.from_E E'
	  val obj = (ElabRep.empty_infix_basis,B.empty,(Realisation.Id, TyName.Set.empty), 
		     N',ElabRep.empty_infix_basis,B', Realisation.Id)
      in case ElabRep.lookup_elab funid
	   of Some (index,(_,_,_,N,_,B,_)) =>  (*names in N already marked gen. since the object is returned by lookup. *)
	     let val _:{} = List.apply Name.mark_gen N'
	         val _:{} = B.match(B',B)
		 val _:{} = List.apply Name.unmark_gen N'
	     in ElabRep.owr_elab(funid,index,obj)
	     end
	    | None => ElabRep.add_elab(funid,obj)
      end

    (* --------------------------------------------
     *  Checking for respecifications
     * -------------------------------------------- *)

    local
      structure VIdSet = OrderSet(structure Order = struct type T = Ident.id
							   val lt = fn x => fn y => Ident.< (x,y)
						    end
				  structure PP = PrettyPrint)
      structure TyConSet = OrderSet(structure Order = struct type T = TyCon.tycon
							     val lt = fn x => fn y => TyCon.< (x,y)
						      end
				    structure PP = PrettyPrint)
      structure StrIdSet = OrderSet(structure Order = struct type T = StrId.strid
							     val lt = fn x => fn y => StrId.< (x,y)
						      end
				    structure PP = PrettyPrint)
    in
      type ids = StrIdSet.Set * TyConSet.Set * VIdSet.Set
      val ids_empty = (StrIdSet.empty, TyConSet.empty, VIdSet.empty)
      fun add_ids_strid((strid_set,tycon_set,vid_set), strid) =
	let val ids = (StrIdSet.insert strid strid_set, tycon_set, vid_set)
	in (StrIdSet.member strid strid_set, ids)
	end
      fun add_ids_tycon((strid_set,tycon_set,vid_set), tycon) =
	let val ids = (strid_set, TyConSet.insert tycon tycon_set, vid_set)
	in (TyConSet.member tycon tycon_set, ids)
	end
      fun add_ids_vid((strid_set,tycon_set,vid_set), vid) =
	let val ids = (strid_set, tycon_set, VIdSet.insert vid vid_set)
	in (VIdSet.member vid vid_set, ids)
	end
    end

    (* -------------------------
     * Some utilities
     * ------------------------- *)

    fun map_Some_on_2nd (x,y) = (x,Some y)
    fun map_Some_on_2nd' (x,y,z) = (x,Some y,z)
    fun map_Some_on_3nd (x,z,y) = (x,z,Some y)
      
    fun elab_X_opt (Y, Some X) elab_X empty_Z = map_Some_on_2nd (elab_X (Y, X))
      | elab_X_opt (Y, None) elab_X empty_Z = (empty_Z, None)

    fun elab_X_opt' (Y, Some X) elab_X empty_Z empty_W = map_Some_on_3nd (elab_X (Y, X))
      | elab_X_opt' (Y, None) elab_X empty_Z empty_W = (empty_Z, empty_W, None)

    fun elab_X_opt'' (Y, Some X, ids) elab_X empty_Z = map_Some_on_2nd' (elab_X (Y, X, ids))
      | elab_X_opt'' (Y, None, ids) elab_X empty_Z = (empty_Z, None, ids)

    fun Phi_match_via a =  Phi.match_via a
    fun Sigma_to_T_and_E a = Sigma.to_T_and_E a
    fun VE_close a = VE.close a
    fun maximise_equality_in_VE_and_TE a = Environments.maximise_equality_in_VE_and_TE a
    fun lookup_longstrid a = B.lookup_longstrid a
    fun lookup_funid a = B.lookup_funid a
    fun lookup_sigid a = B.lookup_sigid a
    fun lookup_longtycon a = E.lookup_longtycon a
    fun lookup_longtycon' a = B.lookup_longtycon a
    fun lookup_longstrid' a = E.lookup_longstrid a
    fun lookup_tycon a = C.lookup_tycon a

    (*****************************************************)
    (* Structure Expressions - Definition v3 pages 36-37 *)
    (*****************************************************)

    fun elab_strexp (B : Basis, strexp : IG.strexp) : (TyName list * Env * OG.strexp) =

      (case strexp of
	(* Generative *)                                    (*rule 50*)
	IG.STRUCTstrexp (i, strdec) =>
	  let val (T, E, out_strdec) = elab_strdec (B, strdec)
	  in
	    (T, E, OG.STRUCTstrexp (okConv i, out_strdec))
	  end

	(* Structure identifier *)                          (*rule 51*)
      | IG.LONGSTRIDstrexp (i, longstrid) => 
	  (case lookup_longstrid B longstrid of
	     Some E => ([], E, OG.LONGSTRIDstrexp (okConv i, longstrid))
	   | None =>
	       ([], E.bogus,
		OG.LONGSTRIDstrexp
		  (errorConv (i, ErrorInfo.LOOKUP_LONGSTRID longstrid), longstrid)))

	                                                    (*rule 52*)
      | IG.TRANSPARENT_CONSTRAINTstrexp (i, strexp, sigexp) =>
	  let val (T, E, out_strexp) = elab_strexp (B, strexp)
	      val (Sigma, out_sigexp) = elab_sigexp (B, sigexp)
	      val (out_i, E') = Sigma_match (i, Sigma, E)
	      val out_i = ElabInfo.plus_TypeInfo out_i (TypeInfo.TRANS_CONSTRAINT_INFO E')
	  in
	    (T, E', OG.TRANSPARENT_CONSTRAINTstrexp (out_i, out_strexp, out_sigexp))
	  end
	                                                    (*rule 53*)
      | IG.OPAQUE_CONSTRAINTstrexp (i, strexp, sigexp) =>
	  let val (T, E, out_strexp) = elab_strexp (B, strexp)
	      val (Sigma, out_sigexp) = elab_sigexp (B, sigexp)
	      val (out_i, E_trans_result, E_opaque_result, phi) = Sigma_match' (i, Sigma, E)
	      val out_i = ElabInfo.plus_TypeInfo out_i (TypeInfo.OPAQUE_CONSTRAINT_INFO (E_trans_result,phi))
	  in
	    (T, E_opaque_result, OG.OPAQUE_CONSTRAINTstrexp (out_i, out_strexp, out_sigexp))
	  end

	(* Functor application *)                           (*rule 54*)
      | IG.APPstrexp (i, funid, strexp) =>
	  let
	    val (T, E, out_strexp) = elab_strexp (B, strexp)
	  in
	    case lookup_funid B funid of
	       Some Phi =>                                     (* the realisation that we annotate *)
		(let val (T'E', rea) = Phi_match_via(Phi,E)    (* need also account for generative *)
		     val (T',E') = Sigma_to_T_and_E T'E'        (* names in the functor body.       *)
		     val out_i = typeConv(i,TypeInfo.FUNCTOR_APP_INFO (rea,E'))
(*
		     val _ = print ("**Applying " ^ OG.FunId.pr_FunId funid ^ "\n")
		     val _ = (print "**Functor argument E = \n"; pr_Env E; print "\n")
		     val _ = (print "**Functor result E = \n"; pr_Env E'; print "\n")
*)
		     val _:{} = match_and_update_repository (funid,T',E')
		 in (T @ (TyName.Set.list T'), E', OG.APPstrexp (out_i, funid, out_strexp)) 
		 end handle No_match reason =>                       (* We bind the argument names in error_result *)
		   let	                                             (* so that the argument signature returned is *)
		     val (T0, E, T'E') = Phi.to_T_and_E_and_Sigma Phi (* as general as possible.                    *)
		     val (T', E') = Sigma.to_T_and_E T'E'
		     val T'E' = Sigma.from_T_and_E (TyName.Set.difference (tynames_E E')
						       (TyName.Set.union T0 T'), E')
		     val out_i = errorConv (i, ErrorInfo.SIGMATCH_ERROR reason)
		     val E' = Sigma.instance T'E'
		   in (T @ (TyName.Set.list T'), E', OG.APPstrexp (out_i, funid, out_strexp)) 
		   end
		 )
	     | None =>
		 (T, E.bogus,
		  OG.APPstrexp (errorConv (i, ErrorInfo.LOOKUP_FUNID funid),
				funid, out_strexp))
	  end

	(* Local declaration *)                             (*rule 55*)
      | IG.LETstrexp (i, strdec, strexp) =>
	  let val (T1, E1, out_strdec) = elab_strdec (B, strdec)
	      val (T2, E2, out_strexp) = elab_strexp (B B_plus_E E1, strexp)
	  in
	    (T1 @ T2, E2, OG.LETstrexp (okConv i, out_strdec, out_strexp))
	  end)

    (********************************************************)
    (* Structure-level Declarations - Definition v3 page 37 *)
    (********************************************************)

    and elab_strdec (B : Basis, strdec : IG.strdec) : (TyName list * Env * OG.strdec) =
      (case strdec of

	(* Core declaration *)                              (*rule 56*)
	IG.DECstrdec (i, dec) => 
	  let
	    val (T, E, out_dec) = ElabDec.elab_dec (B.to_C B, dec)
	  in
	    (T, E, OG.DECstrdec (okConv i, out_dec))
	  end

	(* Structure declaration *)                         (*rule 57*)
      | IG.STRUCTUREstrdec (i, strbind) =>
	  let
	    val (T, SE, out_strbind) = elab_strbind (B, strbind)
	  in
	    (T, E.from_SE SE, OG.STRUCTUREstrdec (okConv i, out_strbind))
	  end

	(* Local declaration *)                             (*rule 58*)
      | IG.LOCALstrdec (i, strdec1, strdec2) =>
	  let
	    val (T1, E1, out_strdec1) = elab_strdec (B, strdec1)
	    val (T2, E2, out_strdec2) = elab_strdec (B B_plus_E E1, strdec2)
	  in
	    (T1 @ T2, E2, OG.LOCALstrdec (okConv i, out_strdec1, out_strdec2))
	  end

	(* Empty declaration *)                             (*rule 59*)
      | IG.EMPTYstrdec i => ([], E.empty, OG.EMPTYstrdec (okConv i))

	(* Sequential declaration *)                        (*rule 60*)
      | IG.SEQstrdec (i, strdec1, strdec2) =>
	  let
	    val (T1, E1, out_strdec1) = elab_strdec (B, strdec1)
	    val (T2, E2, out_strdec2) = elab_strdec (B B_plus_E E1, strdec2)
	  in
	    (T1 @ T2, E1 E_plus_E E2,
	     OG.SEQstrdec (okConv i, out_strdec1, out_strdec2))
	  end)

    (**********************************************)
    (* Structure Bindings - Definition v3 page 38 *)
    (**********************************************)

    and elab_strbind (B : Basis, strbind : IG.strbind)
      : (TyName list * StrEnv * OG.strbind) =
      (case strbind of 
       (* Structure bindings *)                             (*rule 61*)
       IG.STRBIND (i, strid, strexp, strbind_opt) =>
	 let
	   val (T, E, out_strexp) = elab_strexp (B, strexp)
	   val (T', SE, out_strbind_opt) =
	         elab_X_opt' (B, strbind_opt)
		   elab_strbind [] SE.empty
	   val out_i = if EqSet.member strid (SE.dom SE)
		       then repeatedIdsError (i, [ErrorInfo.STRID_RID strid])
		       else okConv i
	 in
	   (T' @ T, SE.singleton (strid, E) SE_plus_SE SE,
	    OG.STRBIND (out_i, strid, out_strexp, out_strbind_opt))
	 end)

    (*****************************************************************)
    (* Signature Expressions, Definition v3 page 38, rules 63 and 64 *)
    (*****************************************************************)

    (*elaborate a sigexp to an E*)
    and elab_sigexp' (B : Basis, sigexp : IG.sigexp) : (TyName list * Env * OG.sigexp) =
      case sigexp 

	 (* Generative *)                                   (*rule 62*)
	of IG.SIGsigexp (i, spec) =>
	   let val (T, E, out_spec, _) = elab_spec (B, spec, ids_empty)
	   in
	     (T, E, OG.SIGsigexp (okConv i, out_spec))
	   end

	 (* Signature identifier *)                         (*rule 63*)
       | IG.SIGIDsigexp (i, sigid) =>
	   (case lookup_sigid B sigid of
	      Some sigma =>
		let val (T,E) = Sigma_instance' sigma
		in
		  (TyName.Set.list T, E, OG.SIGIDsigexp (okConv i, sigid))
		end
	    | None =>
		([], E.bogus,
		 OG.SIGIDsigexp (errorConv (i, ErrorInfo.LOOKUP_SIGID sigid), sigid)))

	                                                    (*rule 64*)
       | IG.WHERE_TYPEsigexp (i, sigexp, explicittyvars, longtycon, ty) =>  
	   let
	     val (T, E, out_sigexp) = elab_sigexp' (B, sigexp)
	     val _ = Level.push()
	     val alphas = map TyVar.from_ExplicitTyVar explicittyvars
	     val (tau, out_ty) = ElabDec.elab_ty (B.to_C B, ty)
	     val _ = Level.pop()
	     fun return (T, E_result, out_i) =
	           (T, E_result, OG.WHERE_TYPEsigexp
		                (out_i, out_sigexp, explicittyvars, longtycon, out_ty))
	     fun fail error_info = return ([], E, errorConv (i, error_info))
	   in
	     case lookup_longtycon E longtycon 
	       of None => fail (ErrorInfo.LOOKUP_LONGTYCON longtycon)
		| Some tystr =>
		  let val (theta, VE) = TyStr.to_theta_and_VE tystr 
		  in case TypeFcn.to_TyName theta 
		       of None => fail (ErrorInfo.WHERE_TYPE_NOT_TYNAME (longtycon, theta, tau))
			| Some t =>
			 if not(TyName.Set.member t (TyName.Set.fromList T)) then
			   fail (ErrorInfo.WHERE_TYPE_RIGID (longtycon, t))
			 else if TyName.arity t <> List.size alphas then
			   fail (ErrorInfo.WHERE_TYPE_ARITY (alphas, (longtycon, t))) 
			 else
			   let val theta' = TypeFcn.from_TyVars_and_Type (alphas, tau)
			       val phi = Realisation.singleton (t, theta')
			       val phi_E = Realisation.on_Env phi E 
			   in
			     if TyName.equality t andalso not (TypeFcn.admits_equality theta') then
			       fail (ErrorInfo.WHERE_TYPE_EQTYPE (longtycon, t, tau))
			     else if not (wellformed_E phi_E) then
			       fail (ErrorInfo.WHERE_TYPE_NOT_WELLFORMED (longtycon, t, tau))
			     else return (List.dropAll (fn t' => TyName.eq(t,t')) T, 
					  phi_E, okConv i) 
			   end 
		  end
	   end


    (*********************************************************)
    (* Signature Expressions, Definition v3 page 38, rule 65 *)
    (*********************************************************)

    (*elaborate a sigexp to a Sigma*)                       (*rule 65*)
    and elab_sigexp (B : Basis, sigexp : IG.sigexp) : (Sig * OG.sigexp) =
          let
	    val (T, E, out_sigexp) = elab_sigexp' (B, sigexp)
(*	    val T = TyName.Set.difference (tynames_E E) (B.to_T B) *)
	  in
	    (Sigma.from_T_and_E (TyName.Set.fromList T, E), out_sigexp)
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

    and elab_spec (B : Basis, spec : IG.spec, ids: ids) : (TyName list * Env * OG.spec * ids) =
           
      (case spec of

	(* Value specification *)                           (*rule 68*)
	IG.VALspec (i, valdesc) =>
	  let
	    val _ = Level.push ()
	    val (VE, out_valdesc, ids) = elab_valdesc (B.to_C B, valdesc, ids)
	    val _ = Level.pop ()
	  in
	    ([], E.from_VE (VE_close VE), OG.VALspec (okConv i, out_valdesc), ids)
	  end

	(* Type specification *)                            (*rule 69% *)
      | IG.TYPEspec (i, typdesc) =>
	  let val (T, TE, out_typdesc, ids) = elab_typdesc false (B.to_C B, typdesc, ids)
	  in
	    (T, E.from_TE TE,  OG.TYPEspec (okConv i, out_typdesc), ids)
	  end

	(* Equality type specification *)
      | IG.EQTYPEspec (i, typdesc) =>                       (*rule 70*)
	  let val (T, TE, out_typdesc, ids) = elab_typdesc true (B.to_C B, typdesc, ids)
	  in
	    (T, E.from_TE TE,  OG.EQTYPEspec (okConv i, out_typdesc), ids)
	  end

	(* Datatype specification *)                        (*rule 71*)
      | IG.DATATYPEspec (i, datdesc) =>
	  let
	    val TE = initial_TE datdesc
	    val ((VE, TE), out_datdesc, ids) = elab_datdesc (C.plus_TE (B.to_C B, TE), datdesc, ids)
	    val (VE, TE) = Environments.maximise_equality_in_VE_and_TE (VE, TE)
	    val T = TE.fold(fn tystr => fn T => 
			    case TypeFcn.to_TyName(TyStr.to_theta tystr)
			      of Some t => t::T
			       | None => impossible "IG.DATATYPEspec.theta not tyname") [] TE
	  in
	    (T, E.from_VE_and_TE (VE,TE),
	     OG.DATATYPEspec (okConv i, out_datdesc), ids)
	  end

	(*Datatype replication specification*)              (*rule 72*)
      | IG.DATATYPE_REPLICATIONspec (i, tycon, longtycon) => 
	  (case lookup_longtycon' B longtycon of
	     Some tystr =>
	       let val (theta, VE) = TyStr.to_theta_and_VE tystr
		   val TE = TE.singleton (tycon, tystr)
		   val (errors,ids) = case add_ids_tycon(ids,tycon)
					of (true, ids) => ([ErrorInfo.TYCON_RID tycon], ids)
					 | (false, ids) => ([], ids) 
		   val (errors, ids) = VE.Fold(fn (vid,_) => fn (errors, ids) =>
					      (case add_ids_vid(ids, vid)
						 of (true,ids) => (ErrorInfo.ID_RID vid :: errors, ids)
						  | (false, ids) => (errors, ids))) (errors,ids) VE 
		   val out_i = case errors
				 of [] => okConv i
				  | _ => repeatedIdsError(i,errors) 
	       in
		 ([], E.from_VE_and_TE (VE,TE),
		  OG.DATATYPE_REPLICATIONspec (out_i, tycon, longtycon), ids)
	       end
	   | None =>
	       ([], E.bogus,
		OG.DATATYPE_REPLICATIONspec
		  (errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon),
		   tycon, longtycon), ids))

	(* Exception specification *)                       (*rule 73*)
      | IG.EXCEPTIONspec (i, exdesc) =>
	  let val (VE, out_exdesc, ids) = elab_exdesc (B.to_C B, exdesc, ids) 
	  in
	    ([], E.from_VE VE, OG.EXCEPTIONspec (okConv i, out_exdesc), ids)
	  end

	(* Structure specification *)                       (*rule 74*)
      | IG.STRUCTUREspec (i, strdesc) =>
	  let val (T, SE, out_strdesc, ids) = elab_strdesc (B, strdesc, ids)
	  in
	    (T, E.from_SE SE, OG.STRUCTUREspec (okConv i, out_strdesc), ids)
	  end

	(* Include specification *)                         (*rule 75*)
      | IG.INCLUDEspec (i, sigexp) =>
	  let val (T, E, out_sigexp) = elab_sigexp' (B, sigexp)
	      val (SE,TE,_) = E.un E
	      val strids = EqSet.list (SE.dom SE)
	      val (errors, ids) = List.foldL (fn strid => fn (errors,ids) =>
					      (case add_ids_strid(ids,strid)
						 of (true, ids) => (ErrorInfo.STRID_RID strid :: errors, ids)
						  | (false, ids) => (errors, ids))) ([], ids) strids 
	      val tycons = EqSet.list (TE.dom TE)
	      val (errors, ids) = List.foldL (fn tycon => fn (errors,ids) =>
					      (case add_ids_tycon(ids,tycon)
						 of (true, ids) => (ErrorInfo.TYCON_RID tycon :: errors, ids)
						  | (false, ids) => (errors, ids))) (errors, ids) tycons
	      val out_i = case errors
			    of [] => okConv i
			     | _ => repeatedIdsError(i,errors) 
	      val out_i = ElabInfo.plus_TypeInfo out_i (TypeInfo.INCLUDE_INFO (strids,tycons))
	  in
	    (T, E, OG.INCLUDEspec (out_i, out_sigexp), ids)
	  end

	(* Empty specification *)                           (*rule 76*)
      | IG.EMPTYspec i => ([], E.empty,  OG.EMPTYspec (okConv i), ids)

	(* Sequential specification *)                      (*rule 77*)
      | IG.SEQspec (i, spec1, spec2) =>
	  let
	    val (T1, E1, out_spec1, ids) = elab_spec (B, spec1, ids)
	    val (T2, E2, out_spec2, ids) = elab_spec (B B_plus_E E1, spec2, ids)
	    val out_i = okConv i
	  in
	    (T1 @ T2, E.plus (E1,E2), OG.SEQspec (out_i, out_spec1, out_spec2), ids)
	  end
	                                                    (*rule 78*)
      | IG.SHARING_TYPEspec (i, spec, longtycon_withinfo_s) =>
	  let
	    val (T0, E, out_spec, ids) = elab_spec (B, spec, ids)
	    val (T, out_longtycon_withinfo_s, error) =
	           List.foldR
		   (fn IG.WITH_INFO (i, longtycon_i) =>
		    fn (T', out_longtycon_withinfo_s, error) =>
		     let val (T'', out_i, error) =
		           case lookup_longtycon E longtycon_i
			     of Some tystr_i =>
			       let val theta_i = TyStr.to_theta tystr_i
			       in case TypeFcn.to_TyName theta_i 
				    of Some t_i =>
				      if TyName.Set.member t_i (TyName.Set.fromList T0) then
					(t_i::T', okConv i, error)
				      else
					(T', errorConv (i, ErrorInfo.SHARING_TYPE_RIGID(longtycon_i, t_i)), true)
				     | None => (T', errorConv (i, ErrorInfo.SHARING_TYPE_NOT_TYNAME
							       (longtycon_i, theta_i)), true)
			       end
			      | None => (T', errorConv (i, ErrorInfo.LOOKUP_LONGTYCON longtycon_i), true)
		     in
		       (T'', OG.WITH_INFO (out_i, longtycon_i) :: out_longtycon_withinfo_s, error)
		     end)
		          ([], [], false) longtycon_withinfo_s
	  in 
	    if error then ([], E.bogus, OG.SHARING_TYPEspec(okConv i, out_spec, out_longtycon_withinfo_s), ids)
	    else let 
	       
		   (* Now, find a representative; if everything is allright, T 
		    * will have at least one member. *)

		   fun find ([], acc) = impossible "IG.SHARING_TYPEspec.find"
		     | find ([t], acc) = (t, acc)
		     | find (t::ts, acc) = if TyName.equality t then (t, ts @ acc)
					   else find (ts,t::acc)
		   val (t0, T') = find(T, []) 
		   val arity = TyName.arity t0
		   val T0' = List.dropAll (fn t => List.exists (fn t' => TyName.eq(t,t')) T') T0
		 in 
		   if List.forAll (fn t => TyName.arity t = arity) T' then
		     let val phi = Realisation.from_T_and_theta (TyName.Set.fromList T', TypeFcn.from_TyName t0)
		     in (T0', Realisation.on_Env phi E,
			 OG.SHARING_TYPEspec (okConv i, out_spec, out_longtycon_withinfo_s), ids)
		     end
		   else (T0', E, OG.SHARING_TYPEspec
			 (errorConv (i, ErrorInfo.SHARING_TYPE_ARITY T),
			  out_spec, out_longtycon_withinfo_s), ids)
		 end
	  end
      
      | IG.SHARINGspec (i, spec, longstrid_withinfo_s) =>
	  let val (T0, E, out_spec, ids) = elab_spec (B, spec, ids)
	      val (Es, out_longstrid_withinfo_s) =
		     List.foldR
		     (fn IG.WITH_INFO (i, longstrid) =>
		      (fn (Es, out_longstrid_withinfo_s) =>

		       (case lookup_longstrid' E longstrid of
			  Some E_i => (E_i::Es,
				       OG.WITH_INFO (okConv i, longstrid)
				       ::out_longstrid_withinfo_s)
			| None => (Es, OG.WITH_INFO
				         (errorConv (i, ErrorInfo.LOOKUP_LONGSTRID longstrid),
					  longstrid)::out_longstrid_withinfo_s))))
		        ([],[]) longstrid_withinfo_s
	      fun member T t = TyName.Set.member t (TyName.Set.fromList T)
	  in let val (T', phi) = share (member T0, Es)
	         val T0' = List.dropAll (fn t => List.exists (fn t' => TyName.eq(t,t')) T') T0 
	     in
	       (T0', Realisation.on_Env phi E,
		OG.SHARINGspec (okConv i, out_spec, out_longstrid_withinfo_s), ids)
	     end handle Share error_info =>
	       (T0, E, OG.SHARINGspec (errorConv (i, error_info), out_spec,
				       out_longstrid_withinfo_s), ids)
	  end
      ) (*specs*)


    (**********************************************)
    (* Value Descriptions - Definition v3 page 41 *)
    (**********************************************)

    and elab_valdesc (C : Context, valdesc : IG.valdesc, ids : ids)
      : (VarEnv * OG.valdesc * ids) =
	                                                    (*rule 79*)
      (case valdesc of
	IG.VALDESC (i, id, ty, valdesc_opt) =>
	  let
	    val (tau, out_ty) = ElabDec.elab_ty (C, ty)
	    val sigma = TypeScheme.from_Type tau
	    val (error, ids) = add_ids_vid(ids,id)
	    val (VE, out_valdesc_opt, ids) = elab_X_opt'' (C, valdesc_opt, ids) elab_valdesc VE.empty
	    val out_i = if error then
	                  repeatedIdsError (i, [ErrorInfo.ID_RID id])
			else if IG.DecGrammar.is_'true'_'nil'_etc id then 
			  errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [id])
			else okConv i
	  in
	    (VE.plus (VE.singleton_var (id, sigma), VE),
	     OG.VALDESC (out_i, id, out_ty, out_valdesc_opt), ids)
	  end)

    (*********************************************)
    (* Type Descriptions - Definition v3 page 41 *)
    (*********************************************)

    and elab_typdesc (equality : bool) (C : Context, typdesc : IG.typdesc, ids : ids)
      : (TyName list * TyEnv * OG.typdesc * ids) =
	                                                    (*rule 80*)
       (case typdesc of
	 IG.TYPDESC (i, explicittyvars, tycon, typdesc_opt) =>
	   let
	     val tyvars = map TyVar.from_ExplicitTyVar explicittyvars
	     val tyvars_repeated = repeaters TyVar.eq tyvars
	     val arity = List.size explicittyvars
	     val t = TyName.freshTyName {tycon=tycon, arity=arity, equality=equality}
	     val theta = TypeFcn.from_TyName t
	     val tystr = TyStr.from_theta_and_VE (theta, VE.empty)
	     val (error, ids) = add_ids_tycon(ids,tycon)
	     val (T, TE, out_typdesc_opt, ids) = 
	       case typdesc_opt
		 of None => ([], TE.empty, None, ids)
		  | Some typdesc' => let val (T,TE,out_typdesc,ids) = elab_typdesc equality (C, typdesc', ids)
				     in (T,TE, Some out_typdesc, ids)
				     end
	     val out_i = if error then
			    repeatedIdsError (i, [ErrorInfo.TYCON_RID tycon])
			 else if not (isEmptyTyVarList tyvars_repeated) then
			    repeatedIdsError (i, map ErrorInfo.TYVAR_RID tyvars_repeated)
			 else okConv i
	   in
	     (t::T, TE.plus (TE.singleton (tycon, tystr), TE),
	      OG.TYPDESC (out_i, explicittyvars, tycon, out_typdesc_opt), ids)
	   end)

    (*************************************************)
    (* Datatype Descriptions - Definition v3 page 42 *)
    (*************************************************)
	                                                    (*rule 81*)
    and elab_datdesc (C : Context, datdesc : IG.datdesc, ids : ids)
      : ((VarEnv * TyEnv) * OG.datdesc * ids) =
        (case datdesc of
	  IG.DATDESC (i, explicittyvars, tycon, condesc, datdesc_opt) =>
	    let
	      val _ = Level.push()

	      val tyvars = map TyVar.from_ExplicitTyVar explicittyvars
	      val (theta, _) = TyStr.to_theta_and_VE
		                 (noSome (lookup_tycon C tycon) "datdesc(1)")
	      val t = noSome (TypeFcn.to_TyName theta) "datdesc(2)"
	      val taus = map Type.from_TyVar tyvars
	      val tau = Type.from_ConsType (Type.mk_ConsType (taus, t))
	      val (constructor_map, out_condesc, ids) = elab_condesc (C, tau, condesc, ids)
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

	      val (error, ids) = add_ids_tycon(ids,tycon)
	      val ((VE', TE'), out_datdesc_opt, ids) = elab_X_opt'' (C, datdesc_opt, ids) 
		                                       elab_datdesc (VE.empty, TE.empty)
	    in
	      (case (if error then [ErrorInfo.TYCON_RID tycon] else [])
		    @ map ErrorInfo.TYVAR_RID tyvars_repeated 
		 of [] =>
		   (case tyvars_not_bound 
		      of [] => 
			( (VE.plus (VE_closed, VE'),
			   TE.plus (TE.singleton (tycon, tystr), TE')) ,
			 OG.DATDESC (if TyCon.is_'true'_'nil'_etc tycon then
				       errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [])
				     else if TyCon.is_'it' tycon then
				       errorConv (i, ErrorInfo.SPECIFYING_IT)
					  else okConv i,
					    explicittyvars, tycon, out_condesc,
					    out_datdesc_opt), ids )
		       | _ => 
			( (VE', TE') ,
			 OG.DATDESC (errorConv
				     (i, ErrorInfo.TYVARS_NOT_IN_TYVARSEQ
				      tyvars_not_bound),
				     explicittyvars, tycon, out_condesc,
				     out_datdesc_opt), ids ))
		  | repeated_ids_errorinfos => 
		      ( (VE', TE') ,
		       OG.DATDESC (repeatedIdsError (i, repeated_ids_errorinfos),
				   explicittyvars, tycon, out_condesc,
				   out_datdesc_opt), ids ))
	    end)

    (****************************************************)
    (* Constructor Descriptions - Definition v3 page 42 *)
    (****************************************************)

    and elab_condesc (C : Context, tau : Type, condesc : IG.condesc, ids : ids)
      : (constructor_map * OG.condesc * ids) =

       (case condesc of
	                                                    (*rule 82*)
	 IG.CONDESC (i, con, None, condesc_opt) =>
	   let
	     val sigma = TypeScheme.from_Type tau
	     val (error, ids) = add_ids_vid(ids,con)
	     val (constructor_map, out_condesc_opt, ids) =
	           elab_condesc_opt(C, tau, condesc_opt, ids)
	   in
	     (constructor_map.add con sigma constructor_map,
	      OG.CONDESC (out_i_for_condesc con constructor_map i error, con,
			  None, out_condesc_opt), ids)
	   end

       | IG.CONDESC (i, con, Some ty, condesc_opt) =>
	   let
	     val (tau', out_ty) = ElabDec.elab_ty (C, ty)
	     val arrow = Type.mk_Arrow (tau', tau)
	     val sigma = TypeScheme.from_Type arrow
	     val (error, ids) = add_ids_vid(ids,con)
	     val (constructor_map, out_condesc_opt, ids) = elab_condesc_opt (C, tau, condesc_opt, ids)
	   in
	     (constructor_map.add con sigma constructor_map,
	      OG.CONDESC (out_i_for_condesc con constructor_map i error, con,
			  Some out_ty, out_condesc_opt), ids)
	   end)

    and out_i_for_condesc con constructor_map i error = 
          if error then 
	    repeatedIdsError (i, [ErrorInfo.CON_RID con])
	  else if IG.DecGrammar.is_'true'_'nil'_etc con then 
	    errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [con])
          else if IG.DecGrammar.is_'it' con then 
	    errorConv (i, ErrorInfo.SPECIFYING_IT)
	  else okConv i

    and elab_condesc_opt
      (C : Context, tau : Type, condesc_opt : IG.condesc Option, ids : ids)
      : (constructor_map * OG.condesc Option * ids) =
          elab_X_opt'' (C, condesc_opt, ids) 
            (fn (C, condesc, ids) => elab_condesc (C, tau, condesc, ids))
	      constructor_map.empty

    (**************************************************)
    (* Exception Descriptions - rule 83             *)
    (**************************************************)

    and elab_exdesc (C : Context, exdesc : IG.exdesc, ids : ids)
      : (VarEnv * OG.exdesc * ids) =
	                                                    (*rule 83*)
       (case exdesc of
	 IG.EXDESC (i, excon, None, exdesc_opt) =>
	   let val (error, ids) = add_ids_vid(ids,excon)
	       val (VE, out_exdesc_opt, ids) = elab_X_opt'' (C, exdesc_opt, ids) elab_exdesc VE.empty
	   in
	     (VE.plus (VE.singleton_excon (excon, Type.Exn), VE),
	      OG.EXDESC (out_i_for_exdesc excon VE i error, excon, None,
			 out_exdesc_opt), ids)
	   end
	| IG.EXDESC (i, excon, Some ty, exdesc_opt) =>
	   let
	     val (tau, out_ty) = ElabDec.elab_ty (C, ty)
	     val tyvars = tyvars_Type tau
	     val arrow = Type.mk_Arrow (tau, Type.Exn)
	     val (error, ids) = add_ids_vid(ids,excon)
	     val (VE, out_exdesc_opt, ids) = elab_X_opt'' (C, exdesc_opt, ids) elab_exdesc VE.empty
	     val out_i = (case tyvars of
			    [] => out_i_for_exdesc excon VE i error
	                  | _ => errorConv (i, ErrorInfo.EXDESC_SIDECONDITION))
	   in
	     (VE.plus (VE.singleton_excon (excon, arrow), VE),
	      OG.EXDESC (out_i, excon, Some out_ty, out_exdesc_opt), ids)
	   end)

    and out_i_for_exdesc excon VE i error =
          if error then 
	    repeatedIdsError (i, [ErrorInfo.EXCON_RID excon])
	  else if IG.DecGrammar.is_'true'_'nil'_etc excon then
	    errorConv (i, ErrorInfo.SPECIFYING_TRUE_NIL_ETC [excon])
          else if IG.DecGrammar.is_'it' excon then
	    errorConv (i, ErrorInfo.SPECIFYING_IT)
	  else okConv i

    (***************************************************)
    (* Structure Desctriptions - Definition v3 page 41 *)
    (***************************************************)

    and elab_strdesc (B : Basis, strdesc : IG.strdesc, ids : ids)
      : (TyName list * StrEnv * OG.strdesc * ids) =
	                                                    (*rule 84*)
      (case strdesc of
	 IG.STRDESC (i, strid, sigexp, None) =>
	   let val (T, E, out_sigexp) = elab_sigexp' (B, sigexp)
	       val (error,ids) = add_ids_strid(ids,strid)
	       val out_i = if error then repeatedIdsError(i, [ErrorInfo.STRID_RID strid])
			   else okConv i
	   in
	     (T, SE.singleton (strid, E),
	      OG.STRDESC (out_i, strid, out_sigexp, None), ids)
	   end
       | IG.STRDESC (i, strid, sigexp, Some strdesc) =>
	   let
	     val (T, E, out_sigexp) = elab_sigexp' (B, sigexp)
	     val (error, ids) = add_ids_strid(ids,strid)
	     val (T', SE, out_strdesc, ids) = elab_strdesc (B, strdesc, ids)
	     val out_i = if error then repeatedIdsError (i, [ErrorInfo.STRID_RID strid])
			 else okConv i
	   in
	     (T @ T', SE.singleton (strid, E) SE_plus_SE SE,
	      OG.STRDESC (out_i, strid, out_sigexp, Some out_strdesc), ids)
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
	    val B' = B B_plus_E (E.from_SE (SE.singleton (strid,E)))
	    val (T'', E', out_strexp) = elab_strexp(B', strexp)
	    val T' = TyName.Set.intersect (tynames_E E') (TyName.Set.fromList T'')
	    val T'E' = Sigma.from_T_and_E (T',E')
	    val (F, out_funbind_opt) = elab_X_opt (B, funbind_opt) elab_funbind F.empty
	    val out_i = if EqSet.member funid (F.dom F)
			then repeatedIdsError (i, [ErrorInfo.FUNID_RID funid])
			else ElabInfo.plus_TypeInfo (okConv i) 
			  (TypeInfo.FUNBIND_INFO {argE=E,elabB=B',T=T'',resE=E',rea_opt=None})
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
	fun deal_with_free_tyvars(i, []) = okConv i
	  | deal_with_free_tyvars(i, criminals : (Ident.id * TyVar list) list) = 
	  let val tyvars = List.foldL (General.curry op @ o #2) [] criminals
	  in case List.all (is_Some o TyVar.to_ExplicitTyVar) tyvars 
	       of [] => (List.apply Type.instantiate_arbitrarily tyvars ;
			 Flags.warnings :=
			 ("Free type variables are not allowed at top-level;\n\
			  \see `The Definition of Standard ML (Revised)', section G.8.\n\
			  \I substituted int for them in the types for "
			  ^ pp_list (quote o Ident.pr_id o #1) criminals
			  ^ ".\n")
			 :: !Flags.warnings ;
			 okConv i)
		| unguarded_tyvars : TyVar list =>
		 errorConv (i, ErrorInfo.UNGUARDED_TYVARS unguarded_tyvars)
	               (* one could extend the error info to also contain
			* the criminal id's *)

	  end
      in
	case topdec 
	  of IG.STRtopdec (i, strdec, topdec_opt) =>                                      (* 87 *)
	    let val (_, E, out_strdec) = elab_strdec(B, strdec)
	        val (B', out_topdec_opt) = elab_topdec_opt(B B_plus_E E, topdec_opt)
		val B'' = (B.from_E E) B_plus_B B'
		val out_i = deal_with_free_tyvars(i, tyvars_E' E)
	    in (B'', OG.STRtopdec(out_i, out_strdec, out_topdec_opt))
	    end
	   | IG.SIGtopdec (i, sigdec, topdec_opt) =>                                      (* 88 *)
	    let val (G, out_sigdec) = elab_sigdec(B, sigdec)
	        val (B', out_topdec_opt) = elab_topdec_opt(B B_plus_G G, topdec_opt)
		val B'' = (B.from_G G) B_plus_B B'
	    in (B'', OG.SIGtopdec(okConv i, out_sigdec, out_topdec_opt))
	    end
	   | IG.FUNtopdec (i, fundec, topdec_opt) =>                                      (* 89 *)
	    let val (F, out_fundec) = elab_fundec(B, fundec)
	        val (B', out_topdec_opt) = elab_topdec_opt(B B_plus_F F, topdec_opt)
		val B'' = (B.from_F F) B_plus_B B'                     (* Actually, it holds that *)
		val out_i = deal_with_free_tyvars(i, tyvars_F' F)      (* tynames F \ (T of B) = {} *)
	    in (B'', OG.FUNtopdec(out_i, out_fundec, out_topdec_opt))
	    end
      end (*let*)

    and elab_topdec_opt (B : Basis, topdec_opt : IG.topdec Option) : (Basis * OG.topdec Option) =
      case topdec_opt
	of Some topdec => let val (B', out_topdec) = elab_topdec(B, topdec)
			  in (B', Some out_topdec)
			  end
	 | None => (B.empty, None)
 
    val elab_topdec = fn a => (TyName.Rank.reset();
			       let val res = elab_topdec a
			       in TyName.Rank.reset();
				 res
			       end)

    (********
    Printing functions
    ********)

    val layoutStaticBasis = B.layout
  end;
