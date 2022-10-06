
structure StatObject: STATOBJECT =
  struct
    structure PP = PrettyPrint
    structure ExplicitTyVar = SyntaxTyVar
    structure ExplicitTyVarMap = SyntaxTyVar.Map

    val print_type_levels = ref false     (* for debugging *)

    fun die s = Crash.impossible ("StatObject." ^ s)
    fun noSome NONE s = die s
      | noSome (SOME x) s = x
    fun is_Some NONE = false
      | is_Some (SOME x) = true
    fun map_opt f (SOME x) = SOME (f x)
      | map_opt f NONE = NONE
    fun pr s = TextIO.output (TextIO.stdOut, s)
    val print_node = Report.print o PP.reportStringTree o PP.NODE
    fun pr_st st = PP.outputTree (print, st, 100)

    fun mapi f xs =
        rev(#2(List.foldl (fn (x,(i,xs)) =>
                              (i+1, f (x,i) :: xs)) (0,nil) xs
              ))

    type tycon = TyCon.tycon
    structure TyName = TyName
    structure Rank = TyName.Rank
    type rank = Rank.rank
    type TyName = TyName.TyName
    type lab  = Lab.lab (*record labels*)
    val ONE = Lab.mk_IntegerLab 1
    val TWO = Lab.mk_IntegerLab 2
    val THREE = Lab.mk_IntegerLab 3
    type scon = SCon.scon
    type strid = Ident.strid
    type StringTree = PP.StringTree

    (*
     * New, more efficient representation of types and substitutions on types
     *
     * -- Birkedal, nov. 1993
     *
     * Major cleanup
     *
     * -- Elsman, nov. 1998
     *)

    (* Bound type variables in type schemes have level GENERIC. For a
     * tree with root n, n has level GENERIC if there is a node in the
     * tree that represents a generic tyvar. Levels in type functions
     * are always GENERIC. *)

    (*let-level's*)

    type level = int

    structure Level = struct
      val GENERIC = ~1  (* level of generic tyvars / type shemes *)
      val NONGENERIC = 0   (* level of non-generic types, that are not tyvars *)
      local val level_ref = ref 0  (*current let-level*)
      in
	fun push () = level_ref := !level_ref + 1
	fun pop () = level_ref := !level_ref - 1
	fun current () = !level_ref
      end (*local*)
      fun pr i = if i = GENERIC then "generic"
		 else if i = NONGENERIC then "nongeneric"
		 else Int.toString i
    end (*Level*)

    val dummy_rank_ref = ref(Rank.current())  (* Used for bound type variables. *)

    type ExplicitTyVar = ExplicitTyVar.SyntaxTyVar

    datatype TypeDesc =
        TYVAR of TyVar
      | ARROW of Type * Type
      | RECTYPE of RecType
      | CONSTYPE of Type list * TyName.TyName

    and TyLink =
        NO_TY_LINK of TyVarDesc        (* type variable *)
      | TY_LINK of Type                (* type variable linked to type *)

    and RecType =
        NILrec
      | VARrec of {RowVar : RowVar, level : level ref}    (* a rowvar has a level *)
      | ROWrec of Lab.lab * Type * RecType   (* labels are ordered during unification *)

    and RecLink =
        NO_REC_LINK of int               (* row variable; the stamp is here to ease printing *)
      | REC_LINK of RecType              (* row variable linked to row *)

    withtype Type        = {TypeDesc : TypeDesc, level : level ref}
	 and TyVar       = TyLink ref
	 and RowVar      = RecLink ref
         and TyVarDesc =
	   {id : int,         (* Two type variables are equal if their ids are equal *)
	    equality : bool,  (* Does the tyvar admit equality *)
	    rank: rank ref,   (* The rank field contains an updatable rank for the
			       * type variable. See TYNAME for further comments. *)
	    overloaded : TyName.Set.Set option,
                              (* The overloaded field contains a list of type names that
			       * the overloaded tyvar may be instantiated to, for instance
			       * [Type.Real, Type.Int, Type.Word] if the overloaded
			       * tyvar stands for a socalled num (e.g., in the type
			       * scheme for `+'). *)
	    explicit : ExplicitTyVar option,  (* Does the tyvar stem from an explicit tyvar; explicit
					       * tyvars may not be unified with other types. *)
	    inst :  {TypeDesc : TypeDesc, level : level ref} option ref}
                          (* For instantiation of type schemes. This
			   * type is identical to `Type option ref' *)

    type FunType     = Type
    type ConsType    = Type

    fun TyVarDesc_eq ({id,...}:TyVarDesc, {id=id2,...}:TyVarDesc) =
        id=id2

    fun TyVarDesc_lt ({id,...}:TyVarDesc, {id=id2,...}:TyVarDesc) =
        id < id2

    fun TyVarDesc_id ({id,...}:TyVarDesc) =
        id

    fun findType ty =
      case #TypeDesc ty
	of TYVAR (tl as ref (TY_LINK ty')) =>
	  (case #TypeDesc ty'
	     of TYVAR (tl' as ref (TY_LINK ty'')) => (tl := TY_LINK ty''; findType ty'')
	      | _ => ty')
	 | _ => ty

    fun findRecType r =
      case r
	of VARrec {RowVar = rl as ref (REC_LINK r'), ...} =>
	  let val r'' = findRecType r' in rl := REC_LINK r''; r'' end
	 | _ => r

    (* For prettyprinting and the like it's most convenient to be able
     * to change a RecType into a (lab, Type) SortedMap with optional
     * RowVar. *)

    fun sanitiseRecType r : (Type Lab.Map.map) * ({RowVar : RowVar, level : level ref} option) =
      case findRecType r
	of NILrec => (Lab.Map.empty, NONE)
	 | VARrec rv => (Lab.Map.empty, SOME rv)
	 | ROWrec(lab, tau, r') =>
	  let val (map, rvOpt) = sanitiseRecType r'
	  in (Lab.Map.add (lab, tau, map), rvOpt)
	  end

    datatype eq_significant = EQ_SIGNIFICANT | EQ_NOT_SIGNIFICANT

   (* Type printing, including association info for type variable
    * names.  One of these is created for each top-level item printed;
    * it is passed around and side-effected. *)

    datatype TVNames = NAMES of {tv: int, letter: int} list ref
		     | NONAMES		(* NONAMES -> don't bother. *)
    fun newTVNames () = NAMES (ref [])


    (* Normalization of Type, RecType, etc.; after normalization, we can assume:
     *    TyVarDesc:
     *       (1) inst field is ref(NONE)
     *    TyLink:
     *       (1) TY_LINK variants do not occur
     *    RecType:
     *       (1) VARrec do not occur
     *    RecLink:
     *       (1) Does not occur
     *    RowVar:
     *       (1) Does not occur
     *)

    fun norm_Type ty =
	let val {TypeDesc,level} = findType ty
	    val TypeDesc =
		case TypeDesc of
		    TYVAR (ref (NO_TY_LINK tvd)) =>
			(case #inst tvd of
			     ref (SOME _) => die "norm_Type.TYVAR"
			   | _ => TypeDesc)
		  | TYVAR _ => die "norm_Type.TYVAR2"
		  | ARROW(t1,t2) => ARROW(norm_Type t1, norm_Type t2)
		  | RECTYPE rt => RECTYPE(norm_RecType rt)
		  | CONSTYPE(ts,tn) => CONSTYPE(map norm_Type ts,tn)
	in {TypeDesc=TypeDesc,level=level}
	end

    and norm_RecType rt =
	case findRecType rt of
	    NILrec => NILrec
	  | VARrec _ => die "norm_RecType.uninstantiated rowvar"
	  | ROWrec (l,t,rt) => ROWrec(l,norm_Type t, norm_RecType rt)
(*
    (* Pickling *)
    val pu_TyVarDesc =
	let fun to (((id,b),e,r),ov,ex) : TyVarDesc =
		{id=id, base=b, equality=e,rank=r,overloaded=ov,explicit=ex,inst=ref NONE}
	    fun from {id, base,equality=e,rank=r,overloaded=ov,explicit=ex,inst} =
		(((id,base),e,r),ov,ex)
	in Pickle.convert (to,from)
	    (Pickle.tup3Gen0(Pickle.tup3Gen0(Pickle.pairGen0(Pickle.int,Pickle.string),
					     Pickle.bool,
					     TyName.Rank.pu_rankrefOne),
			     Pickle.optionGen (TyName.Set.pu TyName.pu),
			     Pickle.optionGen ExplicitTyVar.pu))
	end

    val pu_NoTyLink =
	Pickle.convert (NO_TY_LINK, fn NO_TY_LINK a => a | _ => die "pu_NoTyLink.NO_TY_LINK")
	pu_TyVarDesc
*)

    fun ppTyVarDesc {id : int,
	             equality : bool,
	             rank: rank ref,
	             overloaded : TyName.Set.Set option,
	             explicit : ExplicitTyVar option,
	             inst :  {TypeDesc : TypeDesc, level : level ref} option ref} =
        "<" ^ String.concatWith "," [Int.toString id,Bool.toString equality,
                                     Rank.pp (!rank),
                                     case overloaded of NONE => "noovl"
                                                      | SOME _ => "ovl",
                                     case explicit of NONE => "noexpl"
                                                    | SOME _ => "expl"
                                    ] ^ ">"


    fun ppTyVar (ref (NO_TY_LINK tvdesc)) = ppTyVarDesc tvdesc
      | ppTyVar (ref _) = "tylink"

(*
    local

      fun eq (tv,tv') =
          tv = tv' orelse
	  case (tv, tv') of
              (ref(NO_TY_LINK tvd1), ref(NO_TY_LINK tvd2)) => TyVarDesc_eq(tvd1,tvd2)
                                                              andalso #rank tvd1 = #rank tvd2
	    | (ref (TY_LINK _), _)  => die "TyVar.eq_tv1"
	    | (_, ref (TY_LINK _)) => die "TyVar.eq_tv2"

    in
    val pu_TyVar : TyVar Pickle.pu = Pickle.ref0EqGenPP ppTyVar eq pu_NoTyLink
    end

    val pu_Type : TypeDesc Pickle.pu -> Type Pickle.pu =
	let fun to (td,l) = {TypeDesc=td,level=l}
	    fun from {TypeDesc=td,level=l} = (td,l)
	in Pickle.cache "Type" (fn pu_td => Pickle.convert (to,from)
				(Pickle.pairGen0(pu_td,Pickle.ref0EqGen (fn (r1,r2) => !r1 = !r2) Pickle.int)))
	end

    val pu_Types : TypeDesc Pickle.pu -> Type list Pickle.pu =
	Pickle.cache "Types" (fn pu_td => Pickle.listGen (pu_Type pu_td))

    fun swap (x,y) = (y,x)

    local
      val (pu_TypeDesc, _) =
	let fun TypeDescToInt (TYVAR _) = 0
	      | TypeDescToInt (ARROW _) = 1
	      | TypeDescToInt (RECTYPE _) = 2
	      | TypeDescToInt (CONSTYPE _) = 3
	    fun RecTypeToInt (NILrec) = 0
	      | RecTypeToInt (ROWrec _) = 1
	      | RecTypeToInt (VARrec _) = die "RecTypeToInt"
	    fun TypeDescTYVAR (pu_TypeDesc,pu_RecType) =
		Pickle.con1 TYVAR (fn TYVAR a => a | _ => die "pu_TypeDesc.TYVAR")
		pu_TyVar
	    fun TypeDescARROW (pu_TypeDesc,pu_RecType) =
		Pickle.con1 ARROW (fn ARROW a => a | _ => die "pu_TypeDesc.ARROW")
		(Pickle.pairGen0(pu_Type pu_TypeDesc, pu_Type pu_TypeDesc))
	    fun TypeDescRECTYPE  (pu_TypeDesc,pu_RecType) =
		Pickle.con1 RECTYPE (fn RECTYPE a => a | _ => die "pu_TypeDesc.RECTYPE")
		pu_RecType
	    fun TypeDescCONSTYPE (pu_TypeDesc,pu_RecType) =
		Pickle.con1 (CONSTYPE o swap) (fn CONSTYPE a => swap a | _ => die "pu_TypeDesc.CONSTYPE")
		(Pickle.pairGen0(TyName.pu,pu_Types pu_TypeDesc))

	    fun RecTypeNILrec (a,b) = Pickle.con0 NILrec b

	    fun RecTypeROWrec (pu_TypeDesc,pu_RecType) =
		Pickle.con1 ROWrec (fn ROWrec a => a | _ => die "pu_RecType.ROWrec")
		(Pickle.tup3Gen0(Lab.pu,pu_Type pu_TypeDesc,pu_RecType))

	    val TypeDescFuns =
		[TypeDescTYVAR, TypeDescARROW, TypeDescRECTYPE, TypeDescCONSTYPE]
	    val RecTypeFuns = [RecTypeNILrec, RecTypeROWrec]

	in Pickle.data2Gen("StatObject.TypeDesc",TypeDescToInt,TypeDescFuns,
			   "StatObject.RecType",RecTypeToInt,RecTypeFuns)
	end
    in
      val pu_Type = pu_Type pu_TypeDesc
    end
*)
    structure TyVar =
      struct

	(* Does a type variable admit equality; findType must have
	 * been applied prior to calling this function. *)

	fun equality (ref (NO_TY_LINK tvd)) = #equality tvd
	  | equality (ref (TY_LINK _)) = die "TyVar.equality"

	(* Equality of type variables; two type variables are equal if
	 * their refs are equal or if their ids are equal; we assume that
	 * equality of type variables is tested only for non-linked
	 * type variables, that is, we require that findType has been applied
	 * before equality of type variables is tested. *)

	fun eq (tv,tv') = tv = tv' orelse
	  case (tv, tv')
	    of (ref(NO_TY_LINK tvd1), ref(NO_TY_LINK tvd2)) => TyVarDesc_eq(tvd1,tvd2)
	     | (ref (TY_LINK _), _)  => die "TyVar.eq_tv1"
	     | (_, ref (TY_LINK _)) => die "TyVar.eq_tv2"

	fun lt (tv,tv') =
	  case (tv, tv')
	    of (ref(NO_TY_LINK tvd1), ref(NO_TY_LINK tvd2)) => TyVarDesc_lt(tvd1,tvd2)
	     | (ref (TY_LINK _), _)  => die "TyVar.eq_tv1"
	     | (_, ref (TY_LINK _)) => die "TyVar.eq_tv2"

        fun id tv =
            case tv of
                ref(NO_TY_LINK tvd) => TyVarDesc_id tvd
              | _ => die "TyVar.id"

	fun eq' EQ_NOT_SIGNIFICANT (tv,tv') = eq (tv,tv')
	  | eq' EQ_SIGNIFICANT (tv,tv') = eq (tv,tv') andalso equality tv = equality tv'

	(* Get overloading information; findType must have been
	 * applied prior to calling these function. *)

	fun get_overloaded (ref (NO_TY_LINK tvd)) = #overloaded tvd
	  | get_overloaded (ref (TY_LINK _)) = die "TyVar.get_overloaded"

	fun resolve_overloaded (ref (NO_TY_LINK {overloaded=SOME ts,...})) = ts
	  | resolve_overloaded _ = die "resolve_overloaded"

	fun is_overloaded0 (SOME _) = true
	  | is_overloaded0 NONE = false
	fun is_overloaded tv = (is_overloaded0 o get_overloaded) tv


	(* Setting and unsetting of a type instance. Used for
	 * instantiation of type schemes. *)

	fun set_instance (ref (NO_TY_LINK {inst, ...}), ty) =
	  (case !inst
	     of SOME _ => die "set_instance.tyvar already instantiated"
	      | NONE => inst := SOME ty)
	  | set_instance _ = die "set_instance.link"

	fun unset_instance (ref (NO_TY_LINK {inst, ...})) =
	  (case !inst
	     of SOME _ => inst := NONE
	      | NONE =>  die "unset_instance.tyvar not instantiated")
	  | unset_instance _ = die "unset_instance.link"


	(* Make a fresh type variable based on overloading and
	 * eq-admit info *)

	local val r = ref 0
	in
	  fun fresh0 {equality, overloaded, explicit} =
	    ref (NO_TY_LINK {id = (r := !r + 1 ; !r),
			     equality = equality,
			     rank=ref (Rank.current()),
			     overloaded = overloaded,
			     explicit = explicit,
			     inst = ref NONE})
	  fun fresh_normal () = fresh0 {equality=false, overloaded=NONE, explicit=NONE}
	  fun fresh_overloaded tynames =
	    fresh0 {equality=false,
		    overloaded=SOME (TyName.Set.fromList tynames),
		    explicit=NONE}

	  fun from_ExplicitTyVar extv = fresh0 {equality=ExplicitTyVar.isEquality extv,
						overloaded=NONE, explicit=SOME extv}
	end

      (* Make fresh type variables for bound type variables; this
       * function is used for instantiating type schemes. Notice that
       * the `explicit' info is not preserved. findType must have been
       * applied prior to calling this function. *)

	fun refresh (ref (NO_TY_LINK {id, equality, overloaded, ...})) =
	    fresh0 {equality = equality, overloaded = overloaded, explicit=NONE}
	  | refresh (ref (TY_LINK _)) = die "TyVar.refresh"

      (* Prettyprinting of types *)

      val ordA = ord #"a"
      fun pretty_string names pr_ty
	(ref (NO_TY_LINK {equality, overloaded, id, rank, ...})) =
	    let val boring = "U" ^ Int.toString id
	    in
		(if is_overloaded0 overloaded then "OVERLOADED" else "")
	      ^ (if equality then "''" else "'")
	      ^ (case names of
		     NAMES (L as ref L') =>
                     (case List.find (fn {tv, ...} => tv = id) L' of
                          SOME {letter, ...} => str(chr(ordA + letter))
                        | NONE =>
			  let val len = List.length L'
			  in
			    L := L' @ [{tv=id, letter=len}] ;
			    str(chr(ordA + len))
			  end)
           (*   ^ "[" ^ TyName.Rank.pp (!rank) ^ "]" *)
		 | NONAMES => boring)
	    end
	| pretty_string a pr_ty (ref (TY_LINK ty)) =
	    let val ty = findType ty
	    in case #TypeDesc ty
		 of TYVAR tv => pretty_string a pr_ty tv ^ "{" ^ Int.toString(!(#level ty)) ^ "}"
		  | _ => pr_ty ty ^ "{" ^ Int.toString(!(#level ty)) ^ "}"
	    end

      fun string' pr_ty tv = pretty_string NONAMES pr_ty tv
      fun layout' pr_ty tv = PP.LEAF(string' pr_ty tv)

      val pretty_string = fn a => pretty_string a (fn _ => "(ty_link)")
      val string = pretty_string NONAMES
      val layout = PP.LEAF o string

      (* Operations on sets of type variables *)

      fun memberTyVarSet x set = List.exists (fn y => eq(x,y)) set

      fun insertTyVarSet x set = if memberTyVarSet x set then set else x::set

      fun unionTyVarSet(set1, set2) =
	set1 @ List.filter (fn x => not(memberTyVarSet x set1)) set2

      fun minusTyVarSet (set1, set2) =
	List.filter (fn x => not(memberTyVarSet x set2)) set1

      fun intersectTyVarSet (set1, set2) =
	List.filter (fn x => memberTyVarSet x set1) set2

      fun pr_tyvars (l : TyVar list) : string =
	let fun pr_l [] = ""
	      | pr_l (a::b::rest) = string a ^ "," ^ pr_l (b::rest)
	      | pr_l [a] = string a
	in "(" ^ pr_l l ^ ")"
	end
(*
      val pu = pu_TyVar
*)

    end (*TyVar*)



    type Substitution = unit
    infix oo
    structure Substitution = struct
      val Id = ()
      val bogus = Id
      fun (S1 : Substitution) oo (S2 : Substitution) : Substitution = ()
      fun on (S : Substitution, tau : Type) : Type = findType tau
      fun onScheme (S : Substitution, (tvs,tau)) = (tvs, findType tau)
    end (*Substitution*)



    structure Type =
      struct

	local val r = ref 0
	in fun freshRow () = VARrec {RowVar = ref (NO_REC_LINK (r := !r + 1 ; !r)), level = ref (Level.current())}
	end

	fun layout ty =
	  let val ty = findType ty
	      val st =
		case #TypeDesc ty
		  of CONSTYPE ([], tyname) =>  TyName.layout tyname
		   | CONSTYPE (ty_list, tyname) =>
		    PP.NODE {start="(", finish=") " ^ TyName.pr_TyName tyname, indent=1,
			     children = map layout ty_list,
			     childsep = PP.LEFT ", "}
		   | RECTYPE r => RecType_layout r
		   | ARROW (ty, ty') =>
		    PP.NODE {start="(", finish=")", indent=1,
			     children=[layout ty, layout ty'],
			     childsep=PP.LEFT " -> "}
		   | TYVAR tv => TyVar.layout tv
	  in
	    if !print_type_levels then PP.NODE{start="[", finish="]",childsep=PP.RIGHT ", ", indent=1,
					       children=[st, PP.LEAF(Int.toString (!(#level ty)))]}
	    else st
	  end

	and RecType_layout r =
	  let val (m, rv_opt) = sanitiseRecType r
	      val finish = case rv_opt
			     of SOME {RowVar=rv,...} => " ... " ^ pr_RowVar rv ^ "}"
			      | NONE => "}"
	  in Lab.Map.layoutMap {start="{", eq=" : ", sep=", ", finish=finish}
	    (PP.layoutAtom Lab.pr_Lab) layout m
	  end

	and pr_RowVar (ref (NO_REC_LINK rho)) = "'r" ^ Int.toString rho
	  | pr_RowVar _ = die "pr_RowVar"

	local
	  fun Type_eq0 eq_significant (ty,ty') =
	    let val (ty,ty') = (findType ty, findType ty')
	        val ({TypeDesc,level},{TypeDesc=TypeDesc',level=level'}) = (ty,ty')
		  (* We could check here that the levels are different from Level.GENERIC *)
	    in
	      !level = !level' andalso
	      (case (TypeDesc, TypeDesc')
		 of (TYVAR (tv  as ref (NO_TY_LINK _)),
		     TYVAR (tv' as ref (NO_TY_LINK _))) => TyVar.eq' eq_significant (tv,tv')
		   | (ARROW (ty1,ty1'), ARROW (ty2,ty2')) =>
		   Type_eq0 eq_significant (ty1,ty2) andalso Type_eq0 eq_significant (ty1',ty2')
		   | (RECTYPE r1, RECTYPE r2) => RecType_eq0 eq_significant (r1,r2)
		   | (CONSTYPE (tys1,tyname1), CONSTYPE (tys2,tyname2)) =>
		   TyName.eq (tyname1, tyname2) andalso
		   TypeList_eq0 eq_significant (tys1,tys2)
		   | _ => false)
	    end

	  and RecType_eq0 eq_significant (r1,r2) =
	    let val (r1,r2) = (findRecType r1, findRecType r2)
	    in case (r1,r2)
		 of (NILrec, NILrec) => true
		  | (VARrec {RowVar,level}, VARrec {RowVar=RowVar',level=level'}) => !level = !level' andalso RowVar = RowVar'
		  | (ROWrec (l1,ty1,r1'), ROWrec (l2,ty2,r2')) =>
		   l1 = l2 andalso Type_eq0 eq_significant (ty1,ty2)
		   andalso RecType_eq0 eq_significant (r1',r2')
		  | _ => false
	    end

	  and TypeList_eq0 eq_significant (tys1,tys2) =
	    List.foldl (fn ((ty1,ty2),b) => b andalso
			Type_eq0 eq_significant (ty1,ty2))
	    true (BasisCompat.ListPair.zipEq (tys1,tys2))
	    handle BasisCompat.ListPair.UnequalLengths => false
	in
	  val RecType_eq = RecType_eq0 EQ_SIGNIFICANT
	  val eq = Type_eq0 EQ_SIGNIFICANT
	  val eq_equality_not_significant = Type_eq0 EQ_NOT_SIGNIFICANT  (* only used by TypeFcn.eq *)
	end (*local*)

      (*pretty_string_as_ty and pretty_string:*)
      local
	(* Precedence levels of operators:      tycon : 4
	                                        *     : 3
                                                ->    : 2
                                                {}(), : 1 *)

	fun parenthesize (operator_precedence : int, context_precedence, s : string) =
	      if operator_precedence < context_precedence then
		concat ["(", s, ")"] else s

        fun matches f xs=
            #2(Lab.Map.Fold (fn ((lab,_),(i,b)) => (i+1,b andalso f(i,lab)))
                              (0,true) xs)

	fun is_tuple_type m =
	    matches (fn (i, lab) => Lab.is_LabN(lab, i+1)) m

	(* ziptypes l1 l2 = a list of type options; the list has the
	 * same length as l2; it takes the form [SOME ty1', SOME ty2'
	 * ..., SOME ty_n', NONE, ..., NONE] where n' is min(length
	 * l1, length l2) *)

	fun ziptypes l1 [] = []
	  | ziptypes (x::xs) (y::ys)  = SOME x :: ziptypes xs ys
	  | ziptypes [] l2 = map (fn _ => NONE) l2

	(* fields_of_other_record (ty'_opt, fields) = a list of type
	 * options of the same length of fields, consisting of types
	 * taken from ty', if ty' is record type, and consisting of
	 * NONE otherwise*)

	fun fields_of_other_record (ty'_opt : Type option, fields : Type list) =
	      case ty'_opt of
		SOME {TypeDesc = RECTYPE r', ...} =>
		  let val r' = findRecType r'
		      val (m', rv') = sanitiseRecType r'
		  in
		      ziptypes (Lab.Map.range m') fields
		  end
	      | _ => map (fn field => NONE) fields

	(* TyName_string_as_opt (tyname, tyname'_opt) prints tyname as
	 * tyname'_opt, if the latter is present and the type names
	 * are the same. If the type names are different, they are
	 * printed differently, even if they have the same tycon. *)

	fun full_works tyname =
	    let val (i,b) = TyName.id tyname
	    in concat [TyName.pr_TyName tyname, "<",
		       Int.toString i , "-", b, ">"]
	    end

	fun TyName_string_as_opt (tyname, tyname'_opt) =
	     case tyname'_opt of
	       SOME tyname' =>
		  if TyName.eq (tyname,tyname') then TyName.pr_TyName tyname
		  else if TyName.tycon tyname = TyName.tycon tyname'
		       then full_works tyname
		       else TyName.pr_TyName tyname
	     | NONE => TyName.pr_TyName tyname

	fun pretty_string_as_opt names precedence (ty : Type, ty'_opt : Type option) =
	  let val ty = findType ty
	      val ty'_opt = map_opt findType ty'_opt
	      val st_ty =
		case #TypeDesc ty
		  of TYVAR tv => TyVar.pretty_string names tv

		   | RECTYPE r =>		(* See if we can print it as `a * b * ...'
						 * rather than `{1: a, 2: b, ...}' *)
		    let val r = findRecType r
		        val (m, rv) = sanitiseRecType r
		    in case (is_tuple_type m, rv)
			 of (true, NONE) => (* A possible (t1 * t2 * ...) type, and
					     * no rowvar. *)
			   print_tuple names precedence (m, ty'_opt)
			  | _ => (*Have to do the general print.*)
			   let val fields = Lab.Map.range m
			     val fields' = fields_of_other_record (ty'_opt, fields)
			     val field_types = map (pretty_string_as_opt names 1)
			       (ListPair.zip (fields,fields'))
			     val labels = map Lab.pr_Lab (Lab.Map.dom m)
			     fun colon_between (lab, ty) = lab ^ ": " ^ ty
			   in
			     ListUtils.stringSep "{" "}" ", " colon_between
			     (ListPair.zip (labels,field_types))
			   end
		    end

		   | ARROW (t1, t2) =>
		    parenthesize
		    (2, precedence,
		     (case ty'_opt of
			SOME {TypeDesc = ARROW(t1', t2'), ...} =>
			  pretty_string_as_opt names 3 (t1, SOME t1') ^ "->"
			  ^ pretty_string_as_opt names 2 (t2, SOME t2')
		      | _ =>
			  pretty_string_as_opt names 3 (t1, NONE) ^ "->"
			  ^ pretty_string_as_opt names 2 (t2, NONE)))

		   | CONSTYPE (tys, tyname) =>
		    let val (tys'_opt, tyname'_opt) =
		      case ty'_opt
			of SOME {TypeDesc = CONSTYPE(tys', tyname'), ...} =>
			  (ziptypes tys' tys, SOME tyname')
			 | _ => (map (fn _ => NONE) tys, NONE)
		    in
		      case (tys, tys'_opt)
			of (nil,_) => TyName_string_as_opt (tyname, tyname'_opt)
			 | ([ty], [ty']) =>
			  concat [pretty_string_as_opt names 4 (ty,ty'), " ",
				  TyName_string_as_opt (tyname, tyname'_opt)]
			 | _ =>
			  concat [ListUtils.stringSep "(" ") " ", "
				  (pretty_string_as_opt names 1)
				  (ListPair.zip (tys,tys'_opt)),
				  " ",
				  TyName_string_as_opt (tyname, tyname'_opt)]
		    end
	  in
	    if !print_type_levels then concat ["[", st_ty, ",", Int.toString (!(#level ty)), "]"]
	    else st_ty
	  end

	and print_tuple names precedence (m, ty'_opt: Type option)  =

	  (* Careful: "{1=x}" does not print as "(x)", and "{ }"
	   * should be "unit". We don't do this folding at all if
	   * there's a row var. *)

	      let
		val fields = Lab.Map.range m
		val fields' =
		      (case ty'_opt of
			 SOME {TypeDesc = RECTYPE r', ...} =>
			   let val r' = findRecType r'
			       val (m', rv') = sanitiseRecType r'
			   in (case (is_tuple_type m', rv') of
			         (true, NONE) =>
				   (* A possible (t1' * t2' *  ) type, and no rowvar: *)
				   ziptypes (Lab.Map.range m') fields
			       | _ => map (fn field => NONE) fields)
			   end
		       | _ => map (fn field => NONE) fields)
	      in
		(case (fields, fields') of
		   (nil, _) => "unit"	(* Hard-wired *)
		 | ([x], [x']) => "{1: " ^ pretty_string_as_opt  names 1 (x,x') ^ "}"
		 | _ => parenthesize (3, precedence,
				      ListUtils.stringSep "" "" " * "
				      (pretty_string_as_opt names 4)
				      (ListPair.zip(fields, fields'))))
	      end
      in
	val pretty_string_as_ty = fn names => fn (ty,ty') =>
	      pretty_string_as_opt names 1 (ty, SOME ty') : string
	val pretty_string = fn names =>  fn ty =>
	      pretty_string_as_opt names 1 (ty, NONE) : string
      end (*local*)

      val string_as_ty = pretty_string_as_ty NONAMES
      val string = pretty_string NONAMES

      fun from_TyVar tyvar = {TypeDesc = TYVAR tyvar, level = ref (Level.current ())}

      fun to_TyVar ty = case #TypeDesc (findType ty)
			  of TYVAR tyvar => SOME tyvar
			   | _ => NONE

      val fresh0 = from_TyVar o TyVar.fresh0
      val fresh_normal = from_TyVar o TyVar.fresh_normal
      fun from_RecType r = {TypeDesc = RECTYPE r, level = ref Level.NONGENERIC}

      fun to_RecType ty = case #TypeDesc (findType ty)
			    of RECTYPE t => SOME t
			     | _ => NONE

      (* contains_row_variable tau; true iff there exists a row
       * variable in tau. *)

      fun contains_row_variable t =
	case #TypeDesc (findType t)
	  of TYVAR _ => false
	   | ARROW (t1, t2) => contains_row_variable t1 orelse contains_row_variable t2
	   | RECTYPE r => RecType_contains_row_variable r
	   | CONSTYPE (tylist, _) => List.exists contains_row_variable tylist

      and RecType_contains_row_variable r =
	case findRecType r
	  of NILrec => false
	   | VARrec _ => true
	   | ROWrec (_, ty, r') => contains_row_variable ty orelse RecType_contains_row_variable r'


      structure RecType = struct
	val empty = NILrec		(* "{}" *)
	val dotdotdot = freshRow	(* "{...}" *)

	(*map: a sort-of compose function: apply a ty->ty to a record:*)
	fun map (f : Type -> Type) (r : RecType) : RecType =
	      (case findRecType r of
		 NILrec => NILrec
	       | VARrec v => VARrec v
	       | ROWrec (lab, ty, r') =>  ROWrec (lab, f ty, map f r'))

	(*add_field: add a field to a record:*)
	local
	  fun insertFields (fields : (Lab.lab * Type) list, r) : RecType =
	        (*The lab*Type list must be ordered over the labs:*)
	        (case (fields, findRecType r) of
		   ([], _) => r
		 | ((l, f) :: rest, NILrec) =>
		     ROWrec (l, f, insertFields (rest, NILrec))
		 | ((l, f) :: rest, VARrec v) =>
		     ROWrec (l, f, insertFields (rest, VARrec v))
		 | ((l, f) :: rest, ROWrec (l', f', r')) =>
		     if l = l' then die "insertFields"
		     else if Lab.<(l, l') then
		       ROWrec (l, f, insertFields (rest, r))
		     else
		       ROWrec (l', f', insertFields ((l, f) :: rest, r')))
	in
	  fun add_field (lab, ty) recType =
	        insertFields ([(lab, ty)], recType)
	end (*local*)

	(*apply: apply a ty->unit to a record*)
	fun apply (f : Type -> unit) (r : RecType) : unit =
	      (case findRecType r of
		 NILrec => ()
	       | VARrec v => ()
	       | ROWrec (lab, ty, r') =>  (f ty ; apply f r'))

	(*fold: apply a ty*'b->'b along a RecType, yielding a 'b. *)
	fun fold (f : Type * 'b -> 'b) (x : 'b) (r : RecType) : 'b =
	      (case findRecType r of
		 NILrec => x
	       | VARrec _ => x
	       | ROWrec (lab, ty, r') =>  fold f (f (ty,x)) r')

	fun sorted_labs r =
	      (case sanitiseRecType r of (m, _) => Lab.Map.dom m)
	fun to_list r =
	      let val m = #1 (sanitiseRecType r)
	      in
		BasisCompat.ListPair.zipEq (Lab.Map.dom m, Lab.Map.range m)
	      end handle BasisCompat.ListPair.UnequalLengths => die "to_list"
	fun to_pair r =
	      (case sanitiseRecType r of
		 (m, NONE) =>
		   (case Lab.Map.lookup m ONE of
		      NONE => die "Type.to_pair(L=?)"
		    | SOME tyL =>
			(case Lab.Map.lookup m TWO of
			   NONE => die "Type.to_pair(R=?)"
			 | SOME tyR => (tyL, tyR)))
	       | (_, SOME _) => (*It's flexible: punt*)
		   die "Type.to_pair(flexible)")
	fun sort (r : RecType) : RecType =
	      let
		val (m,varrec_opt) = sanitiseRecType r
		val dom = Lab.Map.dom m
		val range = Lab.Map.range m
	      in
		foldr
		  (fn ((lab,ty), r) => ROWrec(lab,ty,r))
		      (case varrec_opt of
			 SOME varrec => VARrec varrec
		       | NONE => NILrec)
			   (BasisCompat.ListPair.zipEq (dom,range))
	  end handle BasisCompat.ListPair.UnequalLengths => die "RecType.sort"

      end (*RecType*)

      (* Find free and bound type variables; We should use an accumulator
       * instead of a bucket. Moreover, in the case that a type has
       * level different from GENERIC then it is not necessary to look
       * for bound variables in the type. *)

      local
	datatype free_or_bound = FREE | BOUND

	val bucket = ref ([] : TyVar list)
	fun insert tv = if List.exists (fn tv' => TyVar.eq(tv,tv')) (!bucket) then ()
			else bucket := (tv :: (!bucket))

	fun tyvars0 f_b ty : unit =
	  let val ty = findType ty
	  in case #TypeDesc ty
	       of TYVAR (tl as ref (NO_TY_LINK _)) =>
		 (case f_b
		    of FREE => if !(#level ty) = Level.GENERIC then () else insert tl
		     | BOUND => if !(#level ty) = Level.GENERIC then insert tl else ())
		| TYVAR (ref (TY_LINK _))  => die "tyvars0"
		| RECTYPE r => RecType.fold (fn (ty, ()) => tyvars0 f_b ty) () (findRecType r)
		| ARROW (ty,ty') =>     (* For the compilation of value constructors, we
					 * extract the tyvars of ty' before those of ty.
					 * Martin-15/11/1998 *)
		    (tyvars0 f_b ty'; tyvars0 f_b ty)

		| CONSTYPE (types,_) => foldr (fn (ty,()) => tyvars0 f_b ty) () types
	  end

        fun tyvars1 f_b ty = (bucket := []; tyvars0 f_b ty; !bucket)

      in

	(* Some code in the compiler (compilation of primitives, e.g.)
	 * depend on the exact order the bound variables are
	 * extracted from a type - so expect things to blow up if you
	 * modify this very code. *)

	fun tyvars ty : TyVar list = tyvars1 FREE ty
	fun generic_tyvars ty : TyVar list = tyvars1 BOUND ty
      end

      fun tynames ty =
	    let val ty = findType ty
	    in case #TypeDesc ty
		 of TYVAR _ => TyName.Set.empty
		  | RECTYPE r => RecType.fold
		   (fn (ty, T) => TyName.Set.union (tynames ty) T)
		   TyName.Set.empty  r
		 | ARROW (ty, ty') => TyName.Set.union (tynames ty) (tynames ty')
		 | CONSTYPE (types, tyname) => foldl
		   (fn (ty, T) => TyName.Set.union (tynames ty) T)
		   (TyName.Set.singleton tyname) types
	    end

      fun from_pair (ty,ty') =
	    {TypeDesc = RECTYPE (RecType.add_field (ONE, ty)
				 (RecType.add_field (TWO, ty') RecType.empty)),
	     level = ref Level.NONGENERIC}
      fun from_triple (tau1, tau2, tau3) =
	    {TypeDesc = RECTYPE (RecType.add_field (ONE, tau1)
				 (RecType.add_field (TWO, tau2)
				  (RecType.add_field (THREE, tau3) RecType.empty))),
	     level = ref Level.NONGENERIC}
      val Unit = {TypeDesc = RECTYPE RecType.empty, level = ref Level.NONGENERIC}


      (* Function types *)

      fun from_FunType fty = fty
      fun to_FunType ty =
	    (case #TypeDesc (findType ty) of
	       ARROW (ty',ty'') => SOME ty
	     | _ => NONE)
      fun mk_FunType (ty,ty') =
	    {TypeDesc = ARROW (ty,ty'), level = ref Level.NONGENERIC}
      fun un_FunType ty =
	    (case #TypeDesc (findType ty) of
	       ARROW (ty,ty') => SOME (ty,ty')
	     | _ => NONE)


      (* Constructed types *)

      fun from_ConsType cty = cty
      fun to_ConsType ty =
	    (case #TypeDesc (findType ty) of
	       CONSTYPE (types, tyname) => SOME ty
	     | _ => NONE)

      fun mk_ConsType (typel, name) =
	    {TypeDesc = CONSTYPE (typel,name), level = ref Level.NONGENERIC}
      fun un_ConsType ty =
	    (case #TypeDesc (findType ty) of
	       CONSTYPE (typel,name) => SOME (typel,name)
	     | _ => NONE)
      val Exn = mk_ConsType ([], TyName.tyName_EXN)
      fun is_Exn ty =
	    (case #TypeDesc (findType ty) of
	       CONSTYPE ([], name) => TyName.eq (name, TyName.tyName_EXN)
	     | _ => false)
      fun mk_Arrow (ty,ty') = {TypeDesc = ARROW (ty,ty'), level = ref Level.NONGENERIC}
      fun un_Arrow ty =
	    (case #TypeDesc (findType ty) of
	       ARROW (t, t') => SOME (t, t')
	     | _ => NONE)
      fun is_Arrow ty =
	    (case #TypeDesc (findType ty) of
	       ARROW _ => true
	     | _ => false)
      fun mk_Ref t = mk_ConsType ([t], TyName.tyName_REF)


      (* Special constants *)

      val tag_values = Flags.is_on0 "tag_values"
      val values_64bit = Flags.is_on0 "values_64bit"

      val Int31        = mk_ConsType ([], TyName.tyName_INT31)
      val Int32        = mk_ConsType ([], TyName.tyName_INT32)
      val Int63        = mk_ConsType ([], TyName.tyName_INT63)
      val Int64        = mk_ConsType ([], TyName.tyName_INT64)
      val IntInf       = mk_ConsType ([], TyName.tyName_INTINF)
      fun IntDefault () =
        case (tag_values(), values_64bit()) of
            (true,  true)  => Int63
          | (true,  false) => Int31
          | (false, true)  => Int64
          | (false, false) => Int32

      val Real         = mk_ConsType ([], TyName.tyName_REAL)
      val String       = mk_ConsType ([], TyName.tyName_STRING)
      val Bool         = mk_ConsType ([], TyName.tyName_BOOL)
      val Char         = mk_ConsType ([], TyName.tyName_CHAR)

      val Word8        = mk_ConsType ([], TyName.tyName_WORD8)
      val Word31       = mk_ConsType ([], TyName.tyName_WORD31)
      val Word32       = mk_ConsType ([], TyName.tyName_WORD32)
      val Word63       = mk_ConsType ([], TyName.tyName_WORD63)
      val Word64       = mk_ConsType ([], TyName.tyName_WORD64)
      fun WordDefault () =
        case (tag_values(), values_64bit()) of
            (true,  true)  => Word63
          | (true,  false) => Word31
          | (false, true)  => Word64
          | (false, false) => Word32

      fun simple_scon ty = {type_scon = ty, overloading = NONE}

      (* MEMO: For bootstrapping, we don't want the source code to
         depend on us having Int63 and Int64 and friends in scope... *)

      fun fits31bits (i:IntInf.int) : bool =
          IntInf.<=(IntInf.fromLarge(Int31.toLarge(valOf Int31.minInt)), i) andalso
          IntInf.<=(i, IntInf.fromLarge(Int31.toLarge(valOf Int31.maxInt)))

      fun fits32bits (i:IntInf.int) : bool =
          IntInf.<=(IntInf.fromLarge(Int32.toLarge(valOf Int32.minInt)), i) andalso
          IntInf.<=(i, IntInf.fromLarge(Int32.toLarge(valOf Int32.maxInt)))

      fun fits64bits (i:IntInf.int) : bool =
          let val maxi64 : IntInf.int = 7*7*73*127*337*92737*649657  (* = 9223372036854775807 = 2^63-1 *)
              val mini64 : IntInf.int = ~ maxi64 - 1                 (* = ~9223372036854775808 = ~2^63 *)
          in IntInf.<=(mini64, i) andalso IntInf.<=(i, maxi64)
          end

      fun fits63bits (i:IntInf.int) : bool =
          let val maxi63 : IntInf.int = 7*7*73*127*337*92737*649657  (* = 9223372036854775807 = 2^63-1 *)
              val mini63 : IntInf.int = ~ maxi63 - 1                 (* = ~9223372036854775808 = ~2^63 *)
          in IntInf.<=(mini63, i) andalso IntInf.<=(i, maxi63)
          end

      fun of_scon sc =
	  case sc of
              SCon.INTEGER i =>
              let fun mk tns =
                      let val tv = TyVar.fresh_overloaded tns
		      in {type_scon=from_TyVar tv, overloading=SOME tv}
		      end
                  open TyName
              in if fits31bits i then
                   mk ([tyName_INTINF, tyName_INT32, tyName_INT31] @
                       (if values_64bit() then [tyName_INT64, tyName_INT63] else []))
                 else if fits32bits i then
                   mk ([tyName_INTINF, tyName_INT32] @
                       (if values_64bit() then [tyName_INT64, tyName_INT63] else []))
                 else if fits63bits i then
                   mk ([tyName_INTINF] @
                       (if values_64bit() then [tyName_INT64, tyName_INT63] else []))
                 else if fits64bits i then
                   mk ([tyName_INTINF] @
                       (if values_64bit() then [tyName_INT64] else []))
                 else simple_scon IntInf
              end
	   | SCon.STRING _ => simple_scon String
	   | SCon.REAL _ => simple_scon Real
	   | SCon.CHAR _ => simple_scon Char
	   | SCon.WORD w =>
             let fun mk tns =
                     let val tv = TyVar.fresh_overloaded tns
		     in {type_scon=from_TyVar tv, overloading=SOME tv}
		     end
                 fun pow2 (x:IntInf.int) : IntInf.int =
                     if x < 1 then 1
                     else 2 * pow2 (x - 1)
                 open TyName
             in if w <= 0xFF then
                  mk ([tyName_WORD32, tyName_WORD31, tyName_WORD8] @
                      (if values_64bit() then [tyName_WORD64, tyName_WORD63] else []))
                else if w <= 0x7FFFFFFF then
                  mk ([tyName_WORD32, tyName_WORD31] @
                      (if values_64bit() then [tyName_WORD64, tyName_WORD63] else []))
                else if w <= (0x7FFFFFFF*2+1) then
                  mk ([tyName_WORD32] @
                      (if values_64bit() then [tyName_WORD64, tyName_WORD63] else []))
                else if w <= pow2 63 - 1 andalso values_64bit() then
                  mk [tyName_WORD64, tyName_WORD63]
                else if w <= pow2 64 - 1 andalso values_64bit() then
                  simple_scon Word64
                else die ("immediate word constant 0w" ^ IntInf.toString w ^ " is not representable in " ^
                          (if values_64bit() then "64 bits" else "32 bits"))
             end

      local

	(* generalise overloaded type variables iff ov and level >
	 * current_level.  generalise imperative type variables iff
	 * (imp and level > current_level). *)

	fun generalise {ov, imp, tau} =
	  let val tau = findType tau
	      val {TypeDesc, level} = tau
	  in
	    if !level = Level.GENERIC then ()
	    else
		(case TypeDesc of
		   TYVAR tv =>
		     if !level > Level.current ()
		       then if (ov orelse not (TyVar.is_overloaded tv)) andalso imp
			      then level := Level.GENERIC
			    else level := Level.current ()
		     else ()
		 | ARROW (tau1, tau2) =>
		     level := Int.min (generalise {ov=ov, imp=imp, tau=tau1},
				       generalise {ov=ov, imp=imp, tau=tau2})
		 | RECTYPE r =>
		     level := generaliseRecType {ov=ov, imp=imp, r=r}
		 | CONSTYPE (taus, tyname) =>
		     level := foldl Int.min Level.NONGENERIC
		     (map (fn tau => generalise {ov=ov, imp=imp, tau=tau}) (rev taus))
	        );
	       !level
	    end

	  and generaliseRecType {ov, imp, r} =
	    let val r = findRecType r
	    in case r
		 of NILrec => Level.NONGENERIC
		  | VARrec {level,...} => (if !level > Level.current() then level := Level.current()
					   else (); Level.NONGENERIC)
		  | ROWrec (l,tau,r') => Int.min (generalise {ov=ov, imp=imp, tau=tau},
						  generaliseRecType {ov=ov, imp=imp, r=r'})
	    end
      in

	val generalise : {ov:bool,imp:bool,tau:Type} -> unit =
	  fn a => (generalise a; ())

      end (*local*)


      (* make_equality tau = SOME S iff S is a substitution that makes
       * tau an equality type.  NONE otherwise. *)

      local
	exception NotEquality

	fun make_equality0 ty =
	  case #TypeDesc (findType ty)
	    of TYVAR (tv as (ref (NO_TY_LINK ({explicit=SOME ExplicitTyVar,...})))) =>
	      if ExplicitTyVar.isEquality ExplicitTyVar then () else raise NotEquality
	     | TYVAR (tv as (ref (NO_TY_LINK ({equality, overloaded, id, rank, explicit=NONE, inst, ...})))) =>
	      if equality then ()
	      else tv := NO_TY_LINK {equality = true,  overloaded = overloaded,
				     rank = rank, id = id, explicit = NONE, inst = inst}
	     | TYVAR _ => die "make_equality"
	     | RECTYPE r => RecType.apply make_equality0 r
	     | CONSTYPE (ty_list, tyname) =>
		if TyName.eq (tyname, TyName.tyName_REF) orelse
                   TyName.eq (tyname, TyName.tyName_ARRAY) then ()
		(* "ref" and "array" are special cases; take them out straight away,
		 * otherwise we'll damage any tyvars within the args. *)
		else if TyName.equality tyname then List.app make_equality0 ty_list
		     else raise NotEquality
	     | ARROW _ => raise NotEquality
      in
	fun make_equality tau =
	  SOME (make_equality0 tau) handle NotEquality => NONE
      end


      (************************
       *
       * Unification of types
       *
       ************************)

      datatype unify_result = UnifyOk (* of Substitution *)
	                    | UnifyFail of string
                            | UnifyRankError of TyVar * TyName


      (* Unify types (tau,tau') s.t. (range (subst) n restr_tyvars) =
       * empty. *)

      fun restricted_unify {restricted_tyvars : TyVar list}
                           (tau, tau') : unify_result =
      let

	exception Unify of string           (* One of these local exceptions *)
	      and Rank of TyVar * TyName    (* is raised when a unification *)
					    (* error occurs *)

	(* The `occurs' check for tyvars in tys; besides doing the
	 * occurs check the level of type variables in ty with level >
	 * lev is set to lev. Further, the rank of type variables in
	 * ty with rank > rnk is set to rnk. Also, because unification
	 * should fail if a type variable with rank r1 is unified with
	 * a type containing type names with ranks greater than r1, we
	 * check for this condition here and raise Unify if the
	 * condition is true. *)

	fun occurs_tv_in_Type (tv: TyVar, ty: Type) : bool =
	  let
	    fun occursType ty =
	      case findType ty
		of {TypeDesc = TYVAR tv', ...} => TyVar.eq(tv,tv')
		 | {TypeDesc = ARROW(ty1, ty2), ...} => occursType ty1 orelse occursType ty2
		 | {TypeDesc = CONSTYPE(tys, tn), ...} => foldl (fn (ty, b) => b orelse occursType ty) false tys
		 | {TypeDesc = RECTYPE r, ...} => RecType.fold (fn (ty, b) => b orelse occursType ty) false r
	  in occursType ty
	  end

	fun occurs_rv_in_RecType(rv, r) =
	  case findRecType r
	    of NILrec => false
	     | VARrec {RowVar=rv',...} => (rv = rv')
	     | ROWrec(_, ty, r') => occurs_rv_in_Type(rv, ty) orelse occurs_rv_in_RecType(rv, r')

	and occurs_rv_in_Type(rv, ty) =
	  case #TypeDesc (findType ty)
	    of TYVAR _ => false
	     | ARROW(ty1, ty2) => occurs_rv_in_Type(rv, ty1) orelse occurs_rv_in_Type(rv, ty2)
	     | RECTYPE r => occurs_rv_in_RecType(rv, r)
	     | CONSTYPE(tys, _) => (foldl (fn (ty, result) => occurs_rv_in_Type(rv, ty) orelse result)
				    false tys)


	(* Decrease the levels of type variables and row variables in a type *)

	fun decr_level_Type (lev : level) (ty : Type) : unit =
	  let val ty = findType ty
	  in case ty
	       of {TypeDesc = TYVAR _, level} => if !level > lev then level := lev else ()
		| {TypeDesc = ARROW(ty1, ty2), ...} => (decr_level_Type lev ty1; decr_level_Type lev ty2)
		| {TypeDesc = CONSTYPE(tys, tn), ...} => app (decr_level_Type lev) tys
		| {TypeDesc = RECTYPE r, ...} => decr_level_RecType lev r
	  end
	and decr_level_RecType (lev : level) (r : RecType) : unit =
	  let val r = findRecType r
	  in case r
	       of NILrec => ()
		| VARrec {level,...} => if !level > lev then level := lev else ()
		| ROWrec(_, ty, r') => (decr_level_Type lev ty; decr_level_RecType lev r')
	  end


	(* Decrease the rank of type variables in a type *)

	fun decr_rank_Type (p as (tv,rnk : rank)) (ty : Type) : unit =
	  let val ty = findType ty
	  in case #TypeDesc ty
	       of TYVAR (ref (NO_TY_LINK {rank,...})) => if Rank.<=(rnk,!rank) then rank := rnk else ()
		| TYVAR (ref (TY_LINK _)) => die "decr_rank_Type"
		| ARROW(ty1, ty2) => (decr_rank_Type p ty1; decr_rank_Type p ty2)
		| CONSTYPE(tys, tn) => (if Rank.<=(Rank.from_TyName tn, rnk) then app (decr_rank_Type p) tys
					else raise Rank(tv,tn))
		| RECTYPE r => decr_rank_RecType p r
	  end
	and decr_rank_RecType (p as (tv,rnk : rank)) (r : RecType) : unit =
	  let val r = findRecType r
	  in case r
	       of NILrec => ()
		| VARrec _ => ()
		| ROWrec(_, ty, r') => (decr_rank_Type p ty; decr_rank_RecType p r')
	  end


	(* unify_with_overloaded_tyvar tynames1 tau; unify an
	 * overloaded tyvar with tau.  `tynames1' is the set of
	 * allowed tynames, that is, the set of tynames that the tyvar
	 * may be unified with.  For instance, `+' may get the type
	 * `alpha * alpha -> alpha', where alpha is an overloaded
	 * tyvar, and then `tynames1' for alpha will be
	 * {TyName.tyName_INT, TyName.tyName_WORD,
	 * TyName.tyName_REAL}. The function raises Unify if `tau' is
	 * neither a tyname in `tynames1', nor another tyvar.  If
	 * `tau' is itself an overloaded tyvar, the `overloaded' field
	 * of the resulting tyvar should be the intersection of the
	 * `overloaded' fields on the two unified tyvars. If tau is
	 * not overloaded, we must not change the set `tynames1'. *)

	fun unify_with_overloaded_tyvar tynames1 tau =
	  case #TypeDesc (findType tau)
	    of TYVAR (ref (NO_TY_LINK {explicit=SOME _,...})) =>
	      raise Unify "unify_with_overloaded_tyvar: explicit tyvar"
	     | TYVAR (tv as (ref (NO_TY_LINK {equality, id, overloaded=NONE,
					      rank, inst, ...}))) =>
	      let val tvd = {equality = equality, id = id, rank = rank,
			     overloaded=SOME tynames1, explicit = NONE, inst = inst}
	      in tv := NO_TY_LINK tvd
	      end
	     | TYVAR (tv as (ref (NO_TY_LINK {equality, id, overloaded=SOME tynames2,
					      rank, inst, ...}))) =>
	      let val overloadSet = TyName.Set.intersect tynames1 tynames2
                  val _ = if TyName.Set.isEmpty overloadSet then
                            raise Unify "unify_with_overloaded_tyvar: tyvars overloaded with distinct tynames"
                          else ()
		  val tvd = {equality = equality, id = id, rank = rank,
			     overloaded = SOME overloadSet, explicit = NONE, inst = inst}
	      in tv := NO_TY_LINK tvd
	      end
	     | CONSTYPE (taus, tyname) =>
	      if List.exists (fn tn => TyName.eq (tyname,tn)) (TyName.Set.list tynames1) then ()
	      else raise Unify "unify_with_overloaded_tyvar: not overloaded to this tyname"
	     | _ => raise Unify "unify_with_overloaded_tyvar: only overloaded to tynames"


	(* unify_with_tyvar tv; Check the attributes of an ordinary
	 * TyVar are satisfied. We assume the `occurs' check has
	 * already been done. *)

	fun unify_with_tyvar (tv as ref (NO_TY_LINK {equality, overloaded, ...}), tau) =
	  let
	    val S =
	      if equality then
		case make_equality tau
		  of SOME S => S
		   | NONE => raise Unify "unify_with_tyvar.1"
	      else Substitution.Id

	    val S' = case overloaded
		       of SOME tynames =>
			 unify_with_overloaded_tyvar tynames (Substitution.on (S, tau))
			| NONE => Substitution.Id
	  in
	    if ListUtils.member tv restricted_tyvars then raise Unify "unify_with_tyvar.2"
	    else tv := TY_LINK tau
	  end
	  | unify_with_tyvar _ = die "unify_with_tyvar"


	(* Check if we can safely unify an explicit TyVar and type ---
	 * assumes findType has been applied. In the case where both
	 * tau and tau' are tyvars, we check first if they are
	 * equal. *)

	fun unifyExplicit (ty  as {TypeDesc = TYVAR (tv as ref (NO_TY_LINK {explicit=SOME ExplicitTyVar,...})),
				   level=level},
			   ty' as {TypeDesc = TYVAR (tv' as ref (NO_TY_LINK {explicit=SOME ExplicitTyVar',...})),
				   level=level'}) =
	  if TyVar.eq(tv,tv') then
	    if !level = !level' then ()
	    else raise Unify "unifyExplicit.level"
	  else raise Unify "unifyExplicit.eq"

	  | unifyExplicit (ty  as {TypeDesc = TYVAR (tv as ref (NO_TY_LINK {explicit=SOME ExplicitTyVar,...})),
				   level=level},
			   ty' as {TypeDesc = TYVAR(tv' as ref (NO_TY_LINK r)),
				   level=level'}) =

	    (* Notice below that unification of overloaded type
	     * variable and explicit type variable is not allowed *)

	    let val {equality, overloaded, ...} = r
	    in
	       case (equality, ExplicitTyVar.isEquality ExplicitTyVar)
		 of (false,  _  ) => ()
		  | (true , true) => ()
		  | ( _   ,  _  ) => raise Unify "unifyExplicit.2";

		   if ListUtils.member tv' restricted_tyvars then raise Unify "unifyExplicit.3"
		   else if TyVar.is_overloaded tv' then raise Unify "unifyExplicit.4"
		   else if !level' < !level then

		       (*Test example:
			  (fn x => let val f = fn (y: ''a) => x = y in 2 end);*)

		       (*This exception is raised if an explicit type variable is
			unified with a type variable with lower level, since then
			the side condition concerning free type variables on rule
			15 in the Definition is not satisfied*)

		       raise Unify "unifyExplicit.5"
		   else
		       tv' := TY_LINK ty
	     end
	  | unifyExplicit (_, _) = raise Unify "unifyExplicit.7"

	(* unifyTyVar (tau, tau') = Check whether we can safely unify
	 * a TyVar (the first tau) and a type (tau').  unifyTyVar
	 * assumes findType has been applied. In the case where both
	 * tau and tau' are tyvars, we check first if they are
	 * equal. *)

	fun unifyTyVar (ty as {TypeDesc = TYVAR(ref(NO_TY_LINK{explicit=SOME _,...})),...},
			ty') = unifyExplicit(ty,ty')

	  | unifyTyVar
	      (ty  as {TypeDesc = TYVAR(tv as ref(NO_TY_LINK{rank=rank_ref as ref r, ...})),
		       level=level},
	       ty' as {TypeDesc = TYVAR(tv' as ref(NO_TY_LINK{rank=rank_ref' as ref r', ...})),
		       level=level'}) =
	      if TyVar.eq (tv,tv') then
		if !level = !level' then () else die "unifyTyVar"
	      else
		let val rank = Rank.min(r,r')
		in rank_ref := rank ;
		   rank_ref' := rank ;
		   if ListUtils.member tv restricted_tyvars then
		     if ListUtils.member tv' restricted_tyvars then raise Unify "unifyTyVar"
		     else (level := Int.min (!level, !level') ;
			   unify_with_tyvar(tv',ty))
		   else
		     (level' := Int.min (!level, !level') ;
		      unify_with_tyvar(tv,ty'))
		end
	  | unifyTyVar (ty  as {TypeDesc = TYVAR(tv as ref(NO_TY_LINK{rank, ...})),
				level = level},
			ty') =
	    if !level = Level.GENERIC then die "unifyTyVar.GENERIC"
	    else
	      (decr_level_Type (!level) ty' ;
	       decr_rank_Type (tv,!rank) ty' ;        (* the tv is for the error reporting *)
	       if occurs_tv_in_Type (tv, ty') orelse ListUtils.member tv restricted_tyvars
		 then raise Unify "unifyTyVar.2"
	       else unify_with_tyvar(tv, ty'))

	  | unifyTyVar _ = die "unifyTyVar"


	(*******************
	 * Unify two types
	 *******************)

	fun unifyType (ty,ty') =
	  let
	    val (ty,ty') = (findType ty,findType ty')
	  in
	    case (#TypeDesc ty, #TypeDesc ty')
	      of (TYVAR _, _) => unifyTyVar (ty, ty')
	       | (_, TYVAR _) => unifyTyVar (ty', ty)
	       | (RECTYPE r, RECTYPE r') => unifyRecType(r, r')
	       | (ARROW pair, ARROW pair') => unifyFunType(pair, pair')
	       | (CONSTYPE pair, CONSTYPE pair') => unifyConsType(pair, pair')
	       | (_, _) => raise Unify "unifyType"
	  end

	and unifyRecType (r1, r2) = unifyRow(r1, r2)

	and extract (lab: Lab.lab, row: RecType): RecType =
	  let
	    val row = findRecType row

	    fun rowVar NILrec = NONE
	      | rowVar(VARrec rv) = SOME rv
	      | rowVar(ROWrec(_, _, row')) = rowVar (findRecType row')
	  in
	      case rowVar row
		of NONE => raise Unify "extract"
		 | SOME rv =>
		  let val littleRow = ROWrec (lab, fresh_normal (), freshRow ())
		  in (unifyRowVar(rv,littleRow); row)
		  end
	  end

	and unifyRow (row1: RecType, row2: RecType): Substitution =
	  let val (row1,row2) = (RecType.sort row1, RecType.sort row2)
	  in unifyRow' (row1, row2)
	  end

	and unifyRowVar ({RowVar=rv,level}, row) =
	  (decr_level_RecType (!level) row;
	   if occurs_rv_in_RecType(rv, row) then raise Unify "unifyRowVar"
	   else rv := REC_LINK row)

	and unifyRow' (row1: RecType, row2: RecType) =
	  (* Assumes findRecType and RecType.sort has been applied --- done in unifyRow *)
	  if RecType_eq (row1, row2) then ()
	  else
	    case (row1, row2)
	      of (NILrec, NILrec) => Substitution.Id
	       | (_, VARrec rho) => unifyRowVar(rho, row1)
	       | (VARrec rho, _) => unifyRowVar(rho, row2)

	       | (ROWrec(lab1, ty1, row1'), ROWrec(lab2, ty2, row2')) =>
		if lab1 = lab2 then (unifyType(ty1, ty2); unifyRow(row1',row2'))
		else
		  if Lab.<(lab1, lab2) then	(* Pad out row2, try again *)
		    (extract(lab1,row2); unifyRow(row1,row2))
		  else				(* Pad out row1, try again *)
		    (extract(lab2,row1); unifyRow(row1,row2))

	       | _ => raise Unify "unifyRow'"

	and unifyFunType ((tau1, tau2), (tau3, tau4)) =
	  (unifyType(tau1, tau3); unifyType(tau2, tau4))

	and unifyConsType ((ty_list, tyname), (ty_list', tyname')) =
	  if TyName.eq(tyname, tyname') then                            (* Note that tyname=tyname' implies *)
	    List.app unifyType (ListPair.zip(ty_list, ty_list'))      (* length(ty_list)=length(ty_list') *)
	  else raise Unify "unifyConsType"
      in
	(unifyType (tau,tau'); UnifyOk)
	handle Unify s => UnifyFail s
	     | Rank(tv,tn) => UnifyRankError(tv,tn)
      end

      fun unify (ty1,ty2) =
          restricted_unify {restricted_tyvars=nil} (ty1,ty2)

      (* Matching functions for compilation manager *)

      local
	fun match_Type (ty,ty0) : unit =
	  let val (ty, ty0) = (findType ty, findType ty0)
	  in case (#TypeDesc ty,#TypeDesc ty0)
	       of (ARROW (tau,tau'), ARROW (tau0,tau0')) =>
		 (match_Type(tau,tau0); match_Type (tau',tau0'))
		| (RECTYPE rect, RECTYPE rect0) => match_RecType (rect,rect0)
		| (CONSTYPE (taus,tn), CONSTYPE (taus0,tn0)) =>
		 (match_Types (taus, taus0) ; TyName.match (tn, tn0))
		| _ => ()
	  end

	and match_Types ([],_) = ()
	  | match_Types (_,[]) = ()
	  | match_Types (tau::taus, tau0::taus0) =
	  (match_Type (tau, tau0) ; match_Types (taus, taus0))

	and match_RecType(rect,rect0) =
	  case (findRecType rect, findRecType rect0)
	    of (ROWrec (l,tau,rect), ROWrec (l0,tau0,rect0)) =>
	      if l <> l0 then ()
	      else (match_Type (tau,tau0) ; match_RecType (rect,rect0))
	     | _ => ()
      in
	val match = match_Type
      end (*local*)

(*
      val pu : Type Pickle.pu =
	  Pickle.convert (fn a => a, norm_Type)
	  pu_Type
*)
    end (*Type*)



    type TypeScheme = TyVar list * Type

    structure TypeScheme =
      struct

	(* Prettyprinting of type schemes *)

	fun pretty_string tvn (_, tau) = Type.pretty_string tvn tau
	fun layout (alphas, tau) =
	  PP.NODE{start="all" ^ TyVar.pr_tyvars alphas ^ ".",finish="",
		  indent=1,childsep=PP.NOSEP,children=[Type.layout tau]}
	fun string (alphas, tau) = "all" ^ TyVar.pr_tyvars alphas ^ "." ^ Type.string tau
	fun debug_string s sigma = (s ^ ": sigma= " ^ string sigma ^ "\n")

	(* instance_with_types (sigma, taus); instantiate sigma with
	 * taus for the bound type variables. Is it a good idea to
	 * update the levels on the fly? ME 1998-11-20 *)

	fun instance_with_types (([],tau),[]) = tau
	  | instance_with_types ((tvs,tau),taus) =
	  let
	    fun instanceType ty =
	      let val ty = findType ty
		  val {TypeDesc, level} = ty
	      in if !level <> Level.GENERIC then ty
		 else case TypeDesc
			of TYVAR (tv as ref (NO_TY_LINK {inst=ref(SOME ty),...})) => ty
			 | TYVAR (tv as ref (NO_TY_LINK {inst=ref NONE,...})) =>
			  die "instanceType.generic tyvar not instantiated"
			 | TYVAR (ref (TY_LINK _)) => die "instanceType.findType doesn't work"
			 | ARROW (ty1,ty2) => Type.mk_Arrow (instanceType ty1, instanceType ty2)
			 | RECTYPE r => Type.from_RecType (instanceRecType r)
			 | CONSTYPE (tys,tyname) => Type.mk_ConsType (map instanceType tys,tyname)
	      end
	    and instanceRecType r =
	      let val r = findRecType r
	      in case r
		   of NILrec => r
		    | VARrec rho => r
		    | ROWrec (l,ty,r') => ROWrec (l,instanceType ty,instanceRecType r')
	      end

	    val tvs_with_types = BasisCompat.ListPair.zipEq(tvs, taus)
	      handle BasisCompat.ListPair.UnequalLengths => die "instance_with_types.wrong number of types"

	    val _ = app TyVar.set_instance tvs_with_types
	    val tau' = instanceType tau
	    val _ = app TyVar.unset_instance tvs
	  in tau'
	  end

	(* instance sigma; instantiate sigma with fresh type variables *)

	fun instance' (sigma as (tyvars,ty)) =
	  let
	    fun fresh (tv as ref(NO_TY_LINK tvdesc)) = Type.from_TyVar(TyVar.refresh tv)
	      | fresh _ = die ("instance'.fresh: tysch= " ^ string sigma)
	    val types = map fresh tyvars
	  in (instance_with_types (sigma, types), types)
	  end

	fun instance sigma = #1 (instance' sigma)

	(* Equality on type schemes; equality on type schemes is not
	 * used by the elaborator, but is used to implement strong
	 * enrichment for the recompilation scheme. The equality
	 * function defined here is stricter than the equality on type
	 * schemes given in the (Revised) Definition of Standard ML,
	 * Section 4.5; the function defined here does not allow bound
	 * type variables to be reordered or deleted if they do not
	 * occur in the body. There is a good reason for this stricter
	 * functionality; the instance lists annotated on the abstract
	 * syntax tree during elaboration should always correspond to
	 * the list of bound type variables in the type scheme for the
	 * instantiated variable. The equality function here is also
	 * used to implement equality of type functions. *)

	fun eq (([],ty1), ([],ty2)) = Type.eq(ty1,ty2)
	  | eq (sigma1 as (tvs1,_), sigma2 as (tvs2,_)) =
	  length tvs1 = length tvs2 andalso
	  let fun fresh (tv as ref(NO_TY_LINK tvdesc)) = Type.from_TyVar(TyVar.refresh tv)
		| fresh _ = die ("eq.fresh: tysch= " ^ string sigma1)
	      val types = map fresh tvs1
	      val ty1 = instance_with_types (sigma1, types)
	      val ty2 = instance_with_types (sigma2, types)
	  in Type.eq(ty1,ty2)
	  end

	fun to_TyVars_and_Type x = x
	fun from_Type tau = ([],tau)

	(* Functions for finding free type variables and free type
	 * names in a type scheme *)

	fun tyvars (_,tau) = Type.tyvars tau
	fun tynames (_,tau) = Type.tynames tau

	(* Functions for determining if a type scheme generalises a
	 * type or another type scheme *)

	fun generalises_Type (sigma, tau') : bool =
	  let
	    val fv_sigma = tyvars sigma
	    val tau = instance sigma
	    val fv_tau' = Type.tyvars tau'
	  in
	    (* One must use restricted unify! Consider the call
	     * ``generalises_Type(int->int, 'a->int)''.  This call
	     * should return false! -- Martin *)

	    case Type.restricted_unify {restricted_tyvars=TyVar.unionTyVarSet(fv_tau',fv_sigma)} (tau,tau')
	      of Type.UnifyOk => true
	       | _ => false
	  end

	fun generalises_TypeScheme (sigma, (betas,tau')) : bool =
	    (TyVar.intersectTyVarSet (betas,tyvars sigma) = [])
	    andalso
	  let
	    fun uncurry f (x,y) = f x y
	    val fv_sigma = tyvars sigma
	    val tau = instance sigma
	    val fv_tau' = foldl (uncurry TyVar.insertTyVarSet) (Type.tyvars tau') betas
	  in
	    (* One must use restricted unify! Consider the call
	     * ``generalises_TypeScheme(\/().'a->int, \/'b.'b->int)''.  This call
	     * should return false! -- mael 2004-08-05; see test/weeks6.sml *)

	    case Type.restricted_unify {restricted_tyvars=TyVar.unionTyVarSet(fv_tau',fv_sigma)} (tau,tau')
	      of Type.UnifyOk => true
	       | _ => false
	  end

	(* close_overload tau; close a type and do not avoid
	 * generalisation of overloaded type variables; this function
	 * is used for creating type schemes for primitives. *)

	fun close_overload tau =
	  let val tvs = (Type.generalise {ov=true, imp=true, tau=tau};
			 Type.generic_tyvars tau)
	  in (tvs, tau)
	  end

	(* close imp sigma; close a type scheme. The tyvars list
	 * should be empty. The boolean imp should be false if the
	 * valbind that tau is the type of is expansive. *)

	fun close imp ([], tau) =
	  let val tvs = (Type.generalise {ov=false, imp=imp, tau=tau};
			 Type.generic_tyvars tau)
	  in (tvs, tau)
	  end
	  | close _ _ = die "TypeScheme.close"


      (* violates_equality T sigma = false, iff, assuming the tynames
       * in T admit equality, sigma admits equality, i.e.,
       * violates_equality T sigma = non((all t in T admit equality)
       * => sigma admits equality).  violates_equality is used when
       * maximising equality in a TE.  T will be those datatypes in TE
       * we tentatively assume to admit equality, and sigma will be
       * the type scheme of a constructor. *)

      local
	fun violates_equality0 T tau =
	  case #TypeDesc (findType tau)
	    of TYVAR _ => false
	     | RECTYPE r =>
	      (Type.RecType.fold (fn (tau, res) => res orelse violates_equality0 T tau)
	       false r)
	     | CONSTYPE (taus, tyname) =>
	      (if TyName.equality tyname orelse TyName.Set.member tyname T then
		 foldl (fn (tau, res) => res orelse violates_equality0 T tau)
		 false taus
	       else true)
	     | ARROW _ => true
      in
	fun violates_equality (T : TyName.Set.Set) (sigma : TypeScheme) : bool =
	      let val (_, tau) = to_TyVars_and_Type sigma
	      in case Type.un_Arrow tau
		   of NONE => false                                (*nullary constructor*)
		    | SOME (tau', _) => violates_equality0 T tau'  (*unary constructor*)
       	      end
      end (*local*)

      (* Matching for the recompilation manager *)

      fun match((_,tau1),(_,tau2)) : unit = Type.match(tau1,tau2)
(*
      val pu =
	  Pickle.pairGen(Pickle.listGen TyVar.pu,Type.pu)
*)
    end (*TypeScheme*)


    (* Type functions are implemented almost as type schemes. However,
     * bound type variables are maintained explicitly; the length of
     * this type variable list gives the arity of a type function. *)

    datatype TypeFcn = TYPEFCN of {tyvars : TyVar list, tau : Type}

    structure TypeFcn =
      struct

	fun layout (TYPEFCN {tyvars, tau}) =
	  let val tau = findType tau
	  in PP.NODE {start="/\\" ^ TyVar.pr_tyvars tyvars ^ ".", finish="", indent=0,
		      childsep=PP.NOSEP, children=[Type.layout tau]}
	  end

	fun eq (TYPEFCN {tyvars, tau}, TYPEFCN {tyvars=tyvars', tau=tau'}) =
	  TypeScheme.eq((tyvars,tau),(tyvars',tau'))

	fun from_TyVars_and_Type (tyvars : TyVar list, tau : Type) =
	  (Type.generalise {ov=false, imp=true, tau=tau};
	   TYPEFCN {tyvars=tyvars, tau=tau})

	fun apply (theta as (TYPEFCN {tyvars, tau}), taus : Type list) : Type =
	  TypeScheme.instance_with_types ((tyvars,tau),taus)

	fun arity (TYPEFCN {tyvars, tau}) = length tyvars

	(* admits_equality: We need only check if the type in an type
	 * function admits equality because the bound type variables
	 * have already been renamed to admit equality. *)

	fun admits_equality (theta as TYPEFCN {tyvars, tau}) : bool =
	  case Type.make_equality (TypeScheme.instance (tyvars,tau))
	    of SOME _ => true
	     | NONE => false

	fun tynames (TYPEFCN {tyvars, tau}) = Type.tynames tau

	fun grounded (theta : TypeFcn, tynameset : TyName.Set.Set) : bool =
	  TyName.Set.isEmpty (TyName.Set.difference (tynames theta) tynameset)

	fun from_TyName (tyname : TyName) : TypeFcn =
	  let fun make_list (0,acc) = acc
		| make_list (n,acc) = make_list(n-1, TyVar.fresh_normal ()::acc)
	      val tyvars = make_list (TyName.arity tyname, [])
	      val _ = Level.push ()
	      val tau = Type.mk_ConsType (map Type.from_TyVar tyvars, tyname)
	      val _ = Level.pop ()
	  in from_TyVars_and_Type (tyvars, tau)
	  end

	fun to_TyName (TYPEFCN {tyvars, tau}) : TyName option =
	  case Type.un_ConsType tau
	    of SOME (taus,t) =>
	      let fun check ([],[]) = true
		    | check (tv::tvs,tau::taus) =
		       (case Type.to_TyVar tau
			  of SOME tv' => TyVar.eq (tv,tv')
			   | NONE => false) andalso check(tvs,taus)
		    | check _ = false
	      in if check(tyvars,taus) then SOME t
		 else NONE
	      end
	     | _ => NONE

	val is_TyName = is_Some o to_TyName

	fun pretty_string names (TYPEFCN {tyvars, tau}) =
	  case tyvars
	    of [] => {vars="", body=Type.pretty_string names tau}
	     | [tyvar] => {vars=TyVar.pretty_string names tyvar,
			   body=Type.pretty_string names tau}
	     | tyvars => {vars=ListUtils.stringSep "(" ")" ", " (TyVar.pretty_string names) tyvars,
			  body=Type.pretty_string names tau}

	fun pretty_string' names theta = #body (pretty_string names theta)

	fun match (TYPEFCN{tau,...}, TYPEFCN{tau=tau0,...}) : unit = Type.match(tau,tau0)
(*
	val pu =
	    let fun to (tvs,t) = TYPEFCN {tyvars=tvs,tau=t}
		fun from (TYPEFCN {tyvars=tvs,tau=t}) = (tvs,t)
	    in Pickle.convert (to,from) TypeScheme.pu
	    end
*)
    end (*TypeFcn*)


    datatype TypeFcn' = TYNAME of TyName |  EXPANDED of TypeFcn
    datatype realisation =
        Not_Id of TypeFcn' TyName.Map.map
      | Realisation_Id

    structure Realisation = struct

      fun tynamesRng (Realisation_Id) = TyName.Set.empty
	| tynamesRng (Not_Id m) =
	  TyName.Map.Fold (fn ((tn,TYNAME tn'),acc) =>
			      if TyName.eq(tn,tn') then acc
			      else TyName.Set.insert tn' acc
	                    | ((tn,EXPANDED tf),acc) =>
				  TyName.Set.union (TyName.Set.remove tn
						   (TypeFcn.tynames tf))
				  acc)
	  TyName.Set.empty m

      fun dom Realisation_Id = TyName.Set.empty
	| dom (Not_Id m) = TyName.Set.fromList(TyName.Map.dom m)

      (* correct_levels_Type ty; correct levels of non-tyvar nodes in
       * ty. Used by on_Type. *)

      fun correct_levels_Type ty =
	let val ty = findType ty
	in if !(#level ty) = Level.GENERIC then ()
	   else
	     case ty
	       of {TypeDesc = TYVAR tv, level} => ()
		| {TypeDesc = ARROW (ty1,ty2), level} =>
		 level := Int.min (correct_levels_Type ty1, correct_levels_Type ty2)
		| {TypeDesc = RECTYPE r, level} =>
		 level := correct_levels_RecType r
		| {TypeDesc = CONSTYPE (tys,tyname), level} =>
		 level := foldl Int.min Level.NONGENERIC (map correct_levels_Type tys) ;
          !(#level ty)
	end

      and correct_levels_RecType r =
	let val r = findRecType r
	in case r
	     of NILrec => Level.NONGENERIC
	      | VARrec _ => Level.NONGENERIC
	      | ROWrec (l,ty,r') => Int.min (correct_levels_Type ty, correct_levels_RecType r')
	end

      val Id = Realisation_Id

      fun is_Id Realisation_Id = true   (* conservative test *)
	| is_Id _ = false

      fun singleton (t,theta) = Not_Id (TyName.Map.singleton(t,EXPANDED theta))

      fun from_T_and_tyname (T, t0) =
	if TyName.Set.isEmpty T then Realisation_Id
	else Not_Id(TyName.Set.fold (fn t => fn acc => TyName.Map.add(t,TYNAME t0,acc)) TyName.Map.empty T)

      fun renaming' (T: TyName.Set.Set) : TyName.Set.Set * realisation =
	if TyName.Set.isEmpty T then (TyName.Set.empty, Id)
	else let val (T', m) : TyName.Set.Set * TypeFcn' TyName.Map.map =
		    TyName.Set.fold (fn t : TyName => fn (T', m) =>
				     let val t' = TyName.freshTyName {tycon = TyName.tycon t, arity= TyName.arity t,
								      equality= TyName.equality t}
				     in (TyName.Set.insert t' T', TyName.Map.add (t, TYNAME t', m))
				     end) (TyName.Set.empty, TyName.Map.empty) T
	in (T',  Not_Id m)
	end

      fun renaming (T: TyName.Set.Set) : realisation = #2 (renaming' T)

      fun restrict T Realisation_Id = Realisation_Id
	| restrict T (Not_Id m) =
	let val m' = TyName.Set.fold(fn t => fn acc =>
				     case TyName.Map.lookup m t
				       of SOME theta => TyName.Map.add(t,theta,acc)
					| NONE => acc) TyName.Map.empty T
	in if TyName.Map.isEmpty m' then Realisation_Id
	   else Not_Id m'
	end

      fun restrict_from T Realisation_Id = Realisation_Id
	| restrict_from T (Not_Id m) =
	let val m' = TyName.Map.Fold(fn ((t, theta), acc) =>
				     if TyName.Set.member t T then acc
				     else TyName.Map.add(t,theta,acc)) TyName.Map.empty m
	in if TyName.Map.isEmpty m' then Realisation_Id
	   else Not_Id m'
	end

      local exception Inverse
      in fun inverse Realisation_Id = SOME Realisation_Id
	   | inverse (Not_Id m) =
	      (SOME(Not_Id(TyName.Map.Fold(fn ((t, theta), acc) =>
					   case theta
					     of TYNAME t' =>
					       if TyName.Set.member t' (TyName.Set.fromList(TyName.Map.dom acc)) then raise Inverse
					       else TyName.Map.add(t', TYNAME t, acc)
					      | EXPANDED theta' => raise Inverse) TyName.Map.empty m))
	       handle Inverse => NONE)
      end

      fun on_TyName Realisation_Id t : TypeFcn = TypeFcn.from_TyName t
	| on_TyName (Not_Id m) t = (case TyName.Map.lookup m t
				      of SOME(TYNAME t) => TypeFcn.from_TyName t
				       | SOME(EXPANDED theta) => theta
				       | NONE => TypeFcn.from_TyName t)

      fun on_TyName_set (rea : realisation) (T : TyName.Set.Set) =
	    if is_Id rea then T else
	      TyName.Set.fold
	        (fn t => fn T =>
		 TyName.Set.union (TypeFcn.tynames (on_TyName rea t)) T)
		  TyName.Set.empty T

      fun on_TyName' Realisation_Id t : TypeFcn' = TYNAME t
	| on_TyName' (Not_Id m) t = (case TyName.Map.lookup m t
				       of SOME theta => theta
					| NONE => TYNAME t)

      fun on_Type Realisation_Id ty = ty
	| on_Type phi ty =
	  (* NB: keep levels, so that it works for type schemes and type functions as well *)
	  (case findType ty of
	     {TypeDesc = TYVAR _, level} => ty
	   | {TypeDesc = RECTYPE r, level} =>
	       {TypeDesc = RECTYPE (Type.RecType.map (on_Type phi) r),
		level = ref (!level)}
	   | {TypeDesc = ARROW(ty1, ty2), level} =>
	       {TypeDesc = ARROW(on_Type phi ty1, on_Type phi ty2),
		level = ref (!level)}
	   | {TypeDesc = CONSTYPE(tylist, t),level} =>
	       (*definition pg 19. sec. 4.4 Types and Type Functions
		beta-conversion carried out after substituting in type
		functions for type names*)
	       let val theta = on_TyName' phi t

		   fun TypeFcn_apply' (TYNAME t', tau_list) =
			 {TypeDesc = CONSTYPE (tau_list,t'),
			  level = ref (!level)}
		     | TypeFcn_apply' (EXPANDED theta, tau_list) =
			 let val tau = (TypeFcn.apply (theta, tau_list))
			 in correct_levels_Type tau ;
			   tau
			 end
	       in
		 TypeFcn_apply' (theta, map (on_Type phi) tylist)
	       end)


      fun on_TypeScheme Realisation_Id scheme = scheme
	| on_TypeScheme phi (sigma as (tyvars,tau)) =
	  if List.exists TyVar.is_overloaded tyvars then sigma  (* type schemes for overloaded identifiers are rigid *)
	  else
            let val tau = on_Type phi tau
(*
              (* eliminate bound tyvars that are not in tau *)
              val tvs = Type.generic_tyvars tau
              val tyvars = foldr (fn (tv,acc) => if List.exists (fn t => t = tv) tvs then tv::acc else acc) nil tyvars
*)
            in (tyvars,tau)
            end

      fun on_TypeFcn Realisation_Id theta = theta
	| on_TypeFcn phi (theta as TYPEFCN {tyvars, tau}) =
        TYPEFCN{tyvars=tyvars,tau=on_Type phi tau}   (* NOTE: arity of theta should be preserved, which differ
                                                      * from the case for type schemes ; mael-2007-11-07 *)
(*
	let val (tyvars,tau) = on_TypeScheme phi (tyvars,tau)
	in TYPEFCN{tyvars=tyvars,tau=tau}
	end
*)
      fun on_TypeFcn' Realisation_Id typefcn' = typefcn'
	| on_TypeFcn' phi (TYNAME t) = on_TyName' phi t
	| on_TypeFcn' phi (EXPANDED theta) = EXPANDED (on_TypeFcn phi theta)

      fun on_Realisation Realisation_Id phi = phi
	| on_Realisation phi Realisation_Id = Realisation_Id
	| on_Realisation phi (Not_Id m) =
	Not_Id(TyName.Map.Fold (fn ((t,theta), acc) =>
				TyName.Map.add(t,on_TypeFcn' phi theta,acc)) TyName.Map.empty m)

      fun layout_TypeFcn' (TYNAME t) = TyName.layout t
	| layout_TypeFcn' (EXPANDED theta) = TypeFcn.layout theta
      fun layout Realisation_Id = PP.LEAF "Id"
	| layout (Not_Id m) = TyName.Map.layoutMap {start="{",eq=" -> ", finish="}",sep=", "}
	TyName.layout layout_TypeFcn' m

      fun (Realisation_Id : realisation) oo (phi : realisation) : realisation = phi
	| phi oo Realisation_Id = phi
	| (phi1 as Not_Id m1) oo phi2 =
	case on_Realisation phi1 phi2
	  of Realisation_Id => phi1
	   | Not_Id m2 =>
	    let
(*
		fun member t nil = false
		  | member t0 (t::ts) = TyName.eq (t0,t) orelse member t0 ts
	        val d1 = TyName.Map.dom m1
	        fun loop nil = ()
		  | loop (t::ts) =
		  if member t d1 then
		    (PP.printTree(layout phi1);
		     PP.printTree(layout phi2)
		     ;die ("realisation map overlay: " ^ TyName.pr_TyName t))
		  else loop ts
*)
	    in (*loop (TyName.Map.dom m2); *)
	      Not_Id(TyName.Map.plus(m1, m2))
 	    end

      fun enrich (rea0, (rea,T)) =
	TyName.Set.fold (fn t => fn acc => acc andalso
			 TypeFcn.eq(on_TyName rea0 t, on_TyName rea t)) true T

      fun eq (Realisation_Id, Realisation_Id) = true       (* conservative check, thus eq is a bad word for it;
							    * - better now ; mael 2004-04-06 *)
	| eq (rea1,rea2) =
	let val T = TyName.Set.union (dom rea1) (dom rea2)
	in enrich (rea1,(rea2,T))
	end
(*
	| eq (rea1,rea2) =
	let val T = dom rea1
	in TyName.Set.eq T (dom rea2) andalso enrich (rea1,(rea2,T))
	end
*)
      fun match (Realisation_Id, rea0) = ()
	| match (Not_Id m, rea0) =
	let fun convert (EXPANDED theta) = theta
	      | convert (TYNAME t) = TypeFcn.from_TyName t
	in TyName.Map.Fold(fn ((t, theta),_) => TypeFcn.match(convert theta,on_TyName rea0 t)) () m
	end
(*
      val pu_TypeFcn' =
	  let fun toInt (TYNAME _) = 0
		| toInt (EXPANDED _) = 1
	      fun fun_TYNAME _ =
		  Pickle.con1 TYNAME (fn TYNAME a => a | _ => die "pu_TypeFcn'.TYNAME")
		  TyName.pu
	      fun fun_EXPANDED _ =
		  Pickle.con1 EXPANDED (fn EXPANDED a => a | _ => die "pu_TypeFcn'.EXPANDED")
		  TypeFcn.pu
	  in Pickle.dataGen("StatObject.TypeFcn'",toInt,[fun_TYNAME,fun_EXPANDED])
	  end
      val pu =
	  let fun to (SOME e) = Not_Id e
		| to NONE = Realisation_Id
	      fun from (Not_Id e) = SOME e
		| from Realisation_Id = NONE
	  in Pickle.convert (to,from)
	      (Pickle.optionGen(TyName.Map.pu TyName.pu pu_TypeFcn'))
	  end
*)
    end (*Realisation*)

    structure Picklers : sig
       type 'a pu = 'a Pickle.pu
       type pty
       type ptysch
       val pu_pty            : pty pu
       val pu_ptysch         : ptysch pu
       val type_to_pty       : Type -> pty
       val pty_to_type       : pty -> Type
       val typesch_to_ptysch : TypeScheme -> ptysch
       val ptysch_to_typesch : ptysch -> TypeScheme

       val pu_Type           : Type pu
       val pu_TypeScheme     : TypeScheme pu
       val pu_TypeFcn        : TypeFcn pu
       val pu_realisation    : realisation pu
    end  = struct

      type 'a pu = 'a Pickle.pu

      (* all serialised type schemes should be closed, thus type variables can be local *)
      type ptv = { id:int
                 , equality:bool
                 , overloaded: TyName.Set.Set option
  (*
                 , explicit: ExplicitTyVar option     (* whether a type variable is explicit or not should
                                                       * be irrelevant for serialisation *)
  *)
                 }

      datatype pty =
          Tyvar of ptv
        | Arrow of pty * pty
        | Rectype of (Lab.lab * pty) list
        | Constype of pty list * TyName.TyName

      type ptysch = ptv list * pty

      val pu_ptv : ptv Pickle.pu =
          Pickle.convert (fn (id,equality,overloaded) => {id=id,equality=equality,overloaded=overloaded},
                          fn {id,equality,overloaded} => (id,equality,overloaded))
                         (Pickle.tup3Gen0(Pickle.int,
                                          Pickle.bool,
                                          Pickle.optionGen(TyName.Set.pu TyName.pu)))

      val pu_pty : pty Pickle.pu =
          let fun toInt pty =
                  case pty of
                      Tyvar _ => 0
                    | Arrow _ => 1
                    | Rectype _ => 2
                    | Constype _ => 3
              fun fun_Tyvar _ =
                  Pickle.con1 Tyvar (fn Tyvar a => a | _ => die "pu_pty.Tyvar")
                              pu_ptv
              fun fun_Arrow pu_pty =
                  Pickle.con1 Arrow (fn Arrow a => a | _ => die "pu_pty.Arrow")
                              (Pickle.pairGen0(pu_pty, pu_pty))
              fun fun_Rectype pu_pty =
                  Pickle.con1 Rectype (fn Rectype a => a | _ => die "pu_pty.Rectype")
                              (Pickle.listGen(Pickle.pairGen0(Lab.pu,pu_pty)))
              fun fun_Constype pu_pty =
                  Pickle.con1 Constype (fn Constype a => a | _ => die "pu_pty.Rectype")
                              (Pickle.pairGen0(Pickle.listGen pu_pty, TyName.pu))
          in Pickle.dataGen ("PickleType.pu_pty",toInt,
                             [fun_Tyvar,fun_Arrow,
                              fun_Rectype,fun_Constype])
          end

      val pu_ptysch : ptysch Pickle.pu =
          Pickle.pairGen(Pickle.listGen pu_ptv, pu_pty)


      local
        type ptvMap = pty IntFinMap.map

        fun type_to_pty0 (S: ptvMap) (ty:Type) : pty =
            case #TypeDesc(findType ty) of
                TYVAR (ref (NO_TY_LINK tvd)) =>
                (case IntFinMap.lookup S (#id tvd) of
                     SOME pty => pty
                   | NONE => die "type_to_pty0: lookup")
              | TYVAR _ => die "type_to_pty0.TYVAR link: impossible"
              | ARROW (ty1,ty2) => Arrow(type_to_pty0 S ty1, type_to_pty0 S ty2)
              | RECTYPE rt => let fun collect (rt,acc) =
                                      case findRecType rt of
                                          NILrec => rev acc
                                        | ROWrec (l,ty,rt) => collect (rt,(l,type_to_pty0 S ty)::acc)
                                        | VARrec _ => die "type_to_pty0:VARrec"
                              in Rectype (collect (rt,nil))
                              end
              | CONSTYPE (tys,tn) => Constype (map (type_to_pty0 S) tys,tn)
      in
        fun type_to_pty (ty:Type) : pty =
            type_to_pty0 IntFinMap.empty ty
        fun typesch_to_ptysch ((tyvars,ty): TyVar list * Type) : ptysch =
            let val l =
                    mapi (fn (tyvar as ref (NO_TY_LINK tvd), i) =>
                             (#id tvd, {id=i,
                                        equality= #equality tvd,
                                        overloaded= #overloaded tvd})
                           | _ => die "typesch_to_ptysch"
                         ) tyvars
                val S = IntFinMap.fromList (map (fn (id,tv) => (id,Tyvar tv)) l)
            in (map #2 l, type_to_pty0 S ty)
            end
      end

      local
        structure H = Pickle.Hash
        type hm = word IntFinMap.map
        fun type_hash0 (hm:hm) (ty:Type) (acc:H.acc) : H.acc =
            case #TypeDesc(findType ty) of
                TYVAR (ref (NO_TY_LINK tvd)) =>
                H.comb (fn acc => case IntFinMap.lookup hm (#id tvd) of
                                      SOME w => H.word w acc
                                    | NONE => die "type_hash: lookup") acc
              | TYVAR _ => die "type_hash.TYVAR link: impossible"
              | ARROW (ty1,ty2) => H.comb (type_hash0 hm ty2) (H.comb (type_hash0 hm ty1) (H.comb (H.word 0w1377) acc))
              | RECTYPE rt =>
                let fun loop rt acc =
                        case findRecType rt of
                            NILrec => acc
                          | ROWrec (l,ty,rt) => H.comb (loop rt) (H.comb (H.string (Lab.pr_Lab l)) (H.comb (type_hash0 hm ty) acc))
                          | VARrec _ => die "type_hash:VARrec"
                in loop rt (H.word 0w4513 acc)
                end
              | CONSTYPE (tys,tn) =>
                let fun loop nil acc = acc
                      | loop (ty::tys) acc = H.comb (loop tys) (H.comb (type_hash0 hm ty) acc)
                in loop tys (H.int (#1(TyName.id tn)) acc)
                end
      in
        fun type_hash (ty:Type) (acc:H.acc) : H.acc =
            type_hash0 IntFinMap.empty ty acc

        fun typesch_hash ((tyvars,ty): TyVar list * Type) (acc:H.acc) : H.acc =
            let val l =
                    mapi (fn (tyvar as ref (NO_TY_LINK tvd), i) =>
                             (#id tvd, Word.fromInt i)
                           | _ => die "typesch_hash"
                         ) tyvars
                val hm = IntFinMap.fromList l
            in type_hash0 hm ty acc
            end
      end

      local (* all type schemes are closed, which leads to a simple implementation *)
        structure H = Pickle.Hash
        type im = int IntFinMap.map
        fun type_eq0 (im:im,ty1:Type,ty2:Type) : bool =
            case (#TypeDesc(findType ty1), #TypeDesc(findType ty2)) of
                (TYVAR (ref (NO_TY_LINK tvd1)), TYVAR (ref (NO_TY_LINK tvd2)))  =>
                (case IntFinMap.lookup im (#id tvd1) of
                     SOME i => i = #id tvd2 andalso #equality tvd1 = #equality tvd2
                   | NONE => false)
              | (ARROW (ty1,ty2), ARROW (ty1',ty2')) =>
                type_eq0 (im,ty1,ty1') andalso type_eq0 (im,ty2,ty2')
              | (RECTYPE rt1, RECTYPE rt2) =>
                let fun loop (rt1,rt2) =
                        case (findRecType rt1, findRecType rt2) of
                            (NILrec, NILrec) => true
                          | (ROWrec (l1,ty1,rt1), ROWrec (l2,ty2,rt2)) =>
                            l1 = l2 andalso type_eq0 (im,ty1,ty2) andalso loop (rt1,rt2)
                          | _ => false
                in loop (rt1,rt2)
                end
              | (CONSTYPE (tys1,tn1), CONSTYPE (tys2,tn2)) =>
                let fun loop (nil,nil) = true
                      | loop (ty1::tys1, ty2::tys2) = type_eq0 (im,ty1,ty2) andalso loop (tys1,tys2)
                      | loop _ = false
                in TyName.eq(tn1,tn2) andalso loop (tys1,tys2)
                end
              | _ => false
      in
        fun type_eq (ty1:Type,ty2:Type) : bool =
            type_eq0 (IntFinMap.empty,ty1,ty2)

        fun map2 f (nil,nil) = nil
          | map2 f (x::xs, y::ys) = f(x,y)::map2 f (xs,ys)
          | map2 f _ = nil

        fun typesch_eq ((tvs1,ty1):TyVar list * Type, (tvs2,ty2):TyVar list * Type) : bool =
            length tvs1 = length tvs2
            andalso
            let val l = map2 (fn (ref (NO_TY_LINK tvd1), ref (NO_TY_LINK tvd2)) =>
                                 (#id tvd1, #id tvd2)
                               | _ => die "typesch_hash"
                             ) (tvs1,tvs2)
                val im = IntFinMap.fromList l
            in type_eq0 (im,ty1,ty2)
            end
      end

      local
        type tyMap = Type vector

        fun tdToType td : Type =
            {TypeDesc=td,level=ref Level.GENERIC}

        (* Notice: level is GENERIC if level of a subpart is GENERIC *)
        fun pty_to_type0 (V: tyMap) (pty:pty) : Type =
            case pty of
                Tyvar {id,...} =>
                let val t = Vector.sub(V,id)
                            handle Subscript => die "pty_to_type0.id missing"
                in t
                end
              | Arrow(pty1,pty2) =>
                let val ty1 = pty_to_type0 V pty1
                    val ty2 = pty_to_type0 V pty2
                in {TypeDesc=ARROW(ty1,ty2),
                    level=ref(Int.min(!(#level ty1),
                                      !(#level ty2)))
                   }
                end
              | Rectype lptys =>
                let val (r,l) = List.foldr (fn ((lab,pty),(rt,lev)) =>
                                               let val ty = pty_to_type0 V pty
                                               in (ROWrec (lab,ty,rt),
                                                   Int.min(lev,!(#level ty)))
                                               end) (NILrec,Level.NONGENERIC) lptys
                in {TypeDesc=RECTYPE r,
                    level=ref l
                   }
                end
              | Constype (ptys, tn) =>
                let val (tys,l) = List.foldr (fn (pty,(tys,lev)) =>
                                               let val ty = pty_to_type0 V pty
                                               in (ty::tys,
                                                   Int.min(lev,!(#level ty)))
                                               end) (nil,Level.NONGENERIC) ptys
                in {TypeDesc=CONSTYPE (tys,tn),
                    level=ref l
                   }
                end
      in
        fun pty_to_type (ty:pty) : Type =
            pty_to_type0 (Vector.tabulate (0, fn _ =>raise Fail "impossible")) ty

        fun ptysch_to_typesch (ptvs:ptv list, pty:pty) : TyVar list * Type =
            let val l = map (fn {id,equality,overloaded} =>
                                TyVar.fresh0 { equality=equality
                                             , overloaded=overloaded
                                             , explicit=NONE
                                             }
                            ) ptvs
                val V = Vector.fromList (map (tdToType o TYVAR) l)
            in (l, pty_to_type0 V pty)
            end
      end

      local
        structure H = Pickle.Hash
        infix |>
        fun x |> f = f x
      in
        val pu_Type =
            Pickle.noshare (Pickle.convert0 (pty_to_type, type_to_pty) pu_pty)
            |> Pickle.newHashEq (fn ty => H.hash(type_hash ty H.init)) type_eq

        val pu_TypeScheme =
            Pickle.noshare(Pickle.convert0 (ptysch_to_typesch, typesch_to_ptysch) pu_ptysch)
            |> Pickle.newHashEq (fn tysch => H.hash(typesch_hash tysch H.init)) typesch_eq
      end

      val pu_TypeFcn =
          let fun to (tvs,t) = TYPEFCN {tyvars=tvs,tau=t}
              fun from (TYPEFCN {tyvars=tvs,tau=t}) = (tvs,t)
          in Pickle.convert0 (to,from) pu_TypeScheme
          end

      val pu_realisation =
          let val pu_TypeFcn' =
                  let fun toInt (TYNAME _) = 0
                        | toInt (EXPANDED _) = 1
                      fun fun_TYNAME _ =
                          Pickle.con1 TYNAME (fn TYNAME a => a | _ => die "pu_TypeFcn'.TYNAME")
                                      TyName.pu
                      fun fun_EXPANDED _ =
                          Pickle.con1 EXPANDED (fn EXPANDED a => a | _ => die "pu_TypeFcn'.EXPANDED")
                                      pu_TypeFcn
                  in Pickle.dataGen("StatObject.Picklers.TypeFcn'",toInt,[fun_TYNAME,fun_EXPANDED])
                  end
              fun to (SOME e) = Not_Id e
                | to NONE = Realisation_Id
              fun from (Not_Id e) = SOME e
                | from Realisation_Id = NONE
          in Pickle.convert (to,from)
                            (Pickle.optionGen(TyName.Map.pu TyName.pu pu_TypeFcn'))
          end

    (* mael 2022-01-24: Really, we should transform entire
       environments before starting serialisation; it seems we now
       apply Pickle.convert from-functions again and again for
       equality and hashing... *)

    end

    structure Type = struct
      open Type
      val pu = Picklers.pu_Type
    end

    structure TypeScheme = struct
      open TypeScheme
      val pu = Picklers.pu_TypeScheme
    end

    structure TypeFcn = struct
      open TypeFcn
      val pu = Picklers.pu_TypeFcn
    end

    structure Realisation = struct
      open Realisation
      val pu = Picklers.pu_realisation
    end

(*
   (* Test stuff *)
    local
      val a = TyVar.fresh_normal ()
      val _ = Level.push ()
      val a_type = Type.from_TyVar a
      and a'_type = Type.from_TyVar a
      val _ = Level.pop ()
      val tau1 = Type.from_pair(a_type,a'_type)
      val sigma1 = (Type.generalise {ov=false, imp=true, tau=tau1}; (Type.generic_tyvars tau1,tau1))

      val sigma2 =
	let
	  val tv = ref (NO_TY_LINK {id= 1, equality = false, rank=dummy_rank_ref,
					  overloaded = NONE, explicit=NONE, inst=ref NONE})
	  val ty  = {TypeDesc = TYVAR tv, level = ref Level.GENERIC}
	  val ty' = {TypeDesc = TYVAR tv, level = ref Level.GENERIC}
	  val r =   {TypeDesc =
		     RECTYPE (Type.RecType.add_field (ONE, ty)
			        (Type.RecType.add_field (TWO, ty')
				   Type.RecType.empty)),
		     level = ref Level.GENERIC}
	in
	  (Type.generic_tyvars r, r)
	end

      val _ =
	print(if TypeScheme.eq(sigma1,sigma2) then "***GENERALISE test succeeded***\n"
	      else "***GENERALISE test failed***\n")

      val tau3 = TypeScheme.instance sigma1
      val ty = Type.Int
      and ty' = Type.fresh_normal ()
      val tau4 =
	  Type.from_RecType (Type.RecType.add_field (ONE, ty)
			       (Type.RecType.add_field (TWO, ty')
				  Type.RecType.empty))
      val _ = Type.unify (tau3,tau4)
    in
      val _ =
	print(if Type.eq(ty',Type.Int) then "***INSTANCE UNIFY test succeeded***\n"
	      else "***INSTANCE UNIFY test failed***\n")
    end

   (* Test stuff *)
    local
      val a = TyVar.from_ExplicitTyVar(ExplicitTyVar.mk_TyVar "'a")
      and a' = TyVar.fresh_normal()

      val _ = Level.push ()
      val a_type = Type.from_TyVar a
      and a'_type = Type.from_TyVar a'
      val _ = case Type.unify (a_type, a'_type)
		of Type.UnifyOk => ()
		 | _ => print "*****Unify failed*****\n"
      val _ = Level.pop ()
      val tau1 = Type.from_pair(a_type,a'_type)
      val sigma1 = (Type.generalise {ov=false, imp=true, tau=tau1};
		    (Type.generic_tyvars tau1, tau1))
      val sigma2 =
	let
	  val tv = ref (NO_TY_LINK({id= 1, equality = false, rank=dummy_rank_ref,
					  overloaded = NONE, explicit=NONE, inst = ref NONE}))
	  val ty  = {TypeDesc = TYVAR tv, level = ref Level.GENERIC}
	  val ty' = {TypeDesc = TYVAR tv, level = ref Level.GENERIC}
	  val r =   {TypeDesc =
		     RECTYPE (Type.RecType.add_field (ONE, ty)
			        (Type.RecType.add_field (TWO, ty')
				   Type.RecType.empty)),
		     level = ref Level.GENERIC}
	in
	  (Type.generic_tyvars r, r)
	end
    in
      val _ =
	print(if TypeScheme.eq (sigma1,sigma2) then "***EXPLICIT GENERALISE succeeded***\n"
	      else "***EXPLICIT GENERALISE test failed***\n")
    end


   (* Test stuff *)
    local
      val a = TyVar.from_ExplicitTyVar(ExplicitTyVar.mk_TyVar "'a")
      and b = TyVar.from_ExplicitTyVar(ExplicitTyVar.mk_TyVar "'b")
      val _ = Level.push ()
      val a_type = Type.from_TyVar a
      and b_type = Type.from_TyVar b
      val _ = Level.pop ()

      val t = TyName.freshTyName {tycon=TyCon.mk_TyCon "t",
				  arity=2,
				  equality=false}
      val tau = {TypeDesc=CONSTYPE([a_type,b_type],t),
		 level = ref (Level.current ())}

      val theta1 = TypeFcn.from_TyName t
      val theta2 = TypeFcn.from_TyVars_and_Type([a,b],tau)
    in
      val _ =
	print(if TypeFcn.eq(theta1,theta2) then "***TYPEFCN EQUALITY test succeeded***\n"
	      else "***TYPEFCN EQUALITY test failed***\n")
    end



   (* Test stuff *)
    local
      val a = TyVar.from_ExplicitTyVar(ExplicitTyVar.mk_TyVar "'a")
      and b = TyVar.from_ExplicitTyVar(ExplicitTyVar.mk_TyVar "'_b")
      val _ = Level.push ()
      val a_type = Type.from_TyVar a
      and b_type = Type.from_TyVar b
      val _ = Level.pop ()
      val t = TyName.freshTyName {tycon=TyCon.mk_TyCon "t",
				  arity=2,
				  equality=false}
      val tau = {TypeDesc=CONSTYPE([a_type,b_type],t),
		 level = ref (Level.current ())}

      val theta1 = TypeFcn.from_TyVars_and_Type([a,b],tau)

      val theta2 = TypeFcn.from_TyName t

      val phi = Realisation.singleton(t,theta2)
      val theta1' = Realisation.on_TypeFcn phi theta1
    in
      val _ =
	print(if TypeFcn.eq(theta1,theta1') then "***TYREA TYPEFCN EQUALITY test succeeded***\n"
	      else "***TYREA TYPEFCN EQUALITY test failed***\n")
    end


    local
      val tv = TyVar.fresh_normal ()
      val tv' = TyVar.fresh_normal ()
      val tv'' = TyVar.fresh_normal ()
      val _ = Level.push()
      val tv_type = Type.from_TyVar tv     (* create the type 'a list * 'a -> 'a *)
      val tv_type' = Type.from_TyVar tv'
      val tv_type'' = Type.from_TyVar tv''
      val _ = Type.unify(tv_type, tv_type')
      val list_type = Type.mk_ConsType([tv_type], TyName.tyName_LIST)
      val arg = Type.from_RecType (Type.RecType.add_field (ONE, list_type)
				   (Type.RecType.add_field (TWO, tv_type')
				    Type.RecType.empty))
      val ty = Type.mk_FunType(arg,tv_type'')
      val _ = Type.unify(tv_type', tv_type'')
      val _ = Level.pop()
      val sigma = TypeScheme.close true (TypeScheme.from_Type ty)
      val _ = print ("sigma before instantiation = " ^ TypeScheme.string sigma ^ "\n")
      val inst = TypeScheme.instance sigma
      val _ = print ("sigma after instantiation = " ^ TypeScheme.string sigma ^ "\n")
      val _ = print ("instantiation = " ^ Type.string inst ^ "\n")
    in
    end
*)

  end (*StatObject*)
