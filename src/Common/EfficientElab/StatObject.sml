
functor StatObject (structure SortedFinMap : SORTED_FINMAP
		    structure Ident : IDENT
		    structure SCon : SCON
		    structure Lab : LAB
		    structure TyName : TYNAME
		    structure TyCon : TYCON
		      sharing type TyCon.tycon = TyName.tycon
		    structure ExplicitTyVar : TYVAR
		    structure Timestamp : TIMESTAMP
		    structure Flags : FLAGS
		    structure Report : REPORT
		    structure FinMap : FINMAP
		    structure FinMapEq : FINMAPEQ
		    structure PP : PRETTYPRINT
		      sharing type PP.Report = Report.Report
		      sharing type SortedFinMap.StringTree = PP.StringTree
		       = TyName.StringTree
		    structure Crash : CRASH
		      ) : STATOBJECT =
  struct

    open Edlib

    fun die s = Crash.impossible ("StatObject." ^ s)
    fun noSome NONE s = die s
      | noSome (SOME x) s = x
    fun is_Some NONE = false
      | is_Some (SOME x) = true
    fun map_opt f (SOME x) = SOME (f x)
      | map_opt f NONE = NONE
    fun pr s = TextIO.output (TextIO.stdOut, s)
    fun debug_print msg = if !Flags.DEBUG_TYPES then pr (msg ^ "\n") else ()
    val print_node = Report.print o PP.reportStringTree o PP.NODE
    fun pr_st st = PP.outputTree (print, st, 100)

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
     *)
    
    (*
     * Bound type variables in type schemes are always repr. by ordinary tyvars
     * (not explicit tyvars). A node n always has level = Level.GENERIC if there is 
     * a node in the tree which n is the root of which is a generic tyvar.
     *
     * Levels in type functions are always Level.GENERIC
     *
     *)

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
		 else Int.string i
    end (*Level*)

    val dummy_rank_ref = ref(Rank.current())  (* Used for bound type variables. *)

    datatype TypeDesc = 
        TYVAR of TyVar
      | ARROW of Type * Type
      | RECTYPE of RecType
      | CONSTYPE of Type list * TyName.TyName

    and TypeInstance = 
        NO_TYPE_INSTANCE of TyVarDesc        (* free or bound type variable *)
      | TYPE_INSTANCE of Type                (* instantiated type variable *)

    and TyVarDesc =
        ORDINARY of {id : int,
		     equality : bool,
		     rank: rank ref,
		     overloaded : overloaded}
	(* The rank field contains an updatable rank for the type variable. See TYNAME for
	 * further comments. *)
	(* The overloaded field contains a list of types that the overloaded
	 * tyvar may be instantiated to, e.g., [Type.Real, Type.Int,
	 * Type.Word], if the overloaded tyvar stands for a socalled  num,
	 * e.g., in the type scheme for +*)
      | EXPLICIT of ExplicitTyVar

    and overloaded = Overloaded of TyName.Set.Set
                   | Nonoverloaded

    and RecType = 
        NILrec
      | VARrec of RowVar
      | ROWrec of Lab.lab * Type * RecType   (* labels ordered during unification *)

    and RecInstance = 
        NO_REC_INSTANCE of int               (* free row variable           *)
                                             (* stamp only to ease printing *)
      | REC_INSTANCE of RecType              (* instantiated row variable *)

    withtype Type        = {TypeDesc : TypeDesc, level : level ref} 
         and ExplicitTyVar = ExplicitTyVar.SyntaxTyVar
	 and TyVar       = TypeInstance ref
	 and RowVar      = RecInstance ref

    type FunType     = Type
    type ConsType    = Type

    fun TyVarDesc_eq (ORDINARY {id=id1, ...}, ORDINARY {id=id2, ...}) = id1 = id2
      | TyVarDesc_eq (EXPLICIT ExplicitTyVar1, EXPLICIT ExplicitTyVar2)
	      = ExplicitTyVar1 = ExplicitTyVar2
      | TyVarDesc_eq _ = false

    fun findType ty =
          (case #TypeDesc ty of
	     TYVAR (tl as ref (TYPE_INSTANCE ty')) =>
	       (case #TypeDesc ty' of
		  TYVAR (tl' as ref (TYPE_INSTANCE ty'')) =>
		    (tl := TYPE_INSTANCE ty'';
		     findType ty'')
		| _ => ty')
	   | _ => ty)

    fun findRecType r =
          (case r of
	     VARrec (rl as ref (REC_INSTANCE r')) =>
	       let val r'' = findRecType r' in rl := REC_INSTANCE r''; r'' end
	   | _ => r)

    (*For prettyprinting and the like it's most convenient to be able
     to change a RecType into a (lab, Type) SortedMap with optional
     RowVar. *)

    fun sanitiseRecType r
	  : (Lab.lab, Type) SortedFinMap.map * RowVar option =
          (case findRecType r of
	     NILrec => (SortedFinMap.empty, NONE)
	   | VARrec rv => (SortedFinMap.empty, SOME rv)
	   | ROWrec(lab, tau, r') =>
	       let val (map, rvOpt) = sanitiseRecType r'
	       in
		 (SortedFinMap.add Lab.< (lab, tau, map), rvOpt)
	       end)


    datatype eq_significant = EQ_SIGNIFICANT | EQ_NOT_SIGNIFICANT

   (* Type printing, including association info for type variable names.
      One of these is created for each top-level item printed; it is
      passed around and side-effected. *)

    datatype TVNames = NAMES of {tv: int, letter: int} list ref
		     | NONAMES		(* NONAMES -> don't bother. *)
    fun newTVNames () = NAMES (ref [])



    structure TyVar = struct
      fun eq_free (tv,tv') = 
	    (*Equality of free type variables. Ordinary type variables
	     are equal if the references are equal; explicit tyvars if
	     the ExplicitTyVars are equal*)
	    tv = tv' orelse
	    (case (tv,tv') of
	       (ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar)),
		ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar'))) =>
	          ExplicitTyVar = ExplicitTyVar'
	     | _ => false)

      fun eq_bound eq_significant (tv, tv') =
	    (*Bound type variables are equal if their id is equal, and,
	     depending on eq_significant, if their equality attributes are equal.*)
	    (case (tv,tv') of 
	       (ref (NO_TYPE_INSTANCE (ORDINARY {id, equality, ...})),
		ref (NO_TYPE_INSTANCE (ORDINARY {id=id', equality=equality', ...}))) =>
	       id = id' andalso
	       (eq_significant=EQ_NOT_SIGNIFICANT orelse equality=equality')
	      | _ => false)
	 
      (*eq: eq should work on both free and bound type variables
       ---still a mess with this equality stuff*)
      val eq = fn pair => eq_free pair orelse eq_bound EQ_SIGNIFICANT pair

      fun equality (ref (NO_TYPE_INSTANCE (ORDINARY {equality, ...}))) = equality
	| equality (ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar))) = 
	    ExplicitTyVar.isEquality ExplicitTyVar
	| equality _ = die "TyVar.equality"

      fun get_overloaded (ref (NO_TYPE_INSTANCE (ORDINARY {overloaded, ...})))
	    = overloaded
	| get_overloaded (ref (NO_TYPE_INSTANCE _)) = Nonoverloaded
	| get_overloaded _ = die "TyVar.get_overloaded"

      fun is_overloaded0 (Overloaded _) = true
	| is_overloaded0 Nonoverloaded = false
      val is_overloaded  = is_overloaded0 o get_overloaded

      local val r = ref 0 in
      fun fresh0 {equality, overloaded} =
	    ref (NO_TYPE_INSTANCE (ORDINARY {id = (r := !r + 1 ; !r),
					     equality = equality,
					     rank=ref (Rank.current()),
					     overloaded = overloaded}))
      fun fresh_normal () = fresh0 {equality=false, overloaded=Nonoverloaded}
      fun fresh_overloaded tynames =
	    fresh0 {equality=false,
		    overloaded=Overloaded (TyName.Set.fromList tynames)}
      end

      fun refresh (ref (NO_TYPE_INSTANCE (ORDINARY {id, equality, overloaded, ...}))) =
	    fresh0 {equality = equality, overloaded = overloaded}
	| refresh (ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar))) =
	    fresh0 {equality = ExplicitTyVar.isEquality ExplicitTyVar,
		   overloaded = Nonoverloaded}
	| refresh _ = die "TyVar.refresh"

      val from_ExplicitTyVar = ref o NO_TYPE_INSTANCE o EXPLICIT
      fun to_ExplicitTyVar (ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar))) =
	    SOME ExplicitTyVar
	| to_ExplicitTyVar _ = NONE

      fun fresh_bound {id, equality, overloaded} = (ref o NO_TYPE_INSTANCE o ORDINARY)
	{id=id,equality=equality, overloaded=overloaded,rank=dummy_rank_ref}

      val ordA = String.ord "a"
      fun pretty_string names 
	(ref (NO_TYPE_INSTANCE (ORDINARY {equality, overloaded, id, ...}))) =
	    let val boring = 
	              if id < 0 then "S" ^ Int.string(~id) else "U" ^ Int.string id
	    in
		(if is_overloaded0 overloaded then "OVERLOADED" else "")
	      ^ (if equality then "''" else "'")
	      ^ (case names of
		   NAMES (L as ref L') =>
		     if !Flags.DEBUG_TYVARS then boring
		     else
		       (let val {letter, ...} =
			           List.first (fn {tv, ...} => tv = id) L'
			in
			  String.chr(ordA + letter)
			end
		        handle List.First _ =>
			  let val len = List.size L'
			  in
			    L := L' @ [{tv=id, letter=len}] ;
			    String.chr(ordA + len)
			  end)
		 | NONAMES => boring)
	    end
	| pretty_string _ (ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar))) =
	    ExplicitTyVar.pr_tyvar ExplicitTyVar
	| pretty_string a (ref (TYPE_INSTANCE ty)) =
	    (case #TypeDesc (findType ty) of
	       TYVAR tv => pretty_string a tv
	     | _ => "(instantiated)")

      val string = pretty_string NONAMES
      val layout = PP.LEAF o string
      fun debug_print from tyvar = 
	    if !Flags.DEBUG_TYPES then 
	      print_node {start=from ^ ": ", finish="", indent=0,
			  children=[layout tyvar], childsep=PP.NOSEP}
	    else ()

      (*Sets of type variables (compared as free tyvars, i.e. by reference, or 
       if explicit, by ExplicitTyVar)*)
      (*TODO 26/01/1997 14:17. tho.: foul ad hoc set functions.
       See also those in Environments.  They use a different sense of equality.*)

      fun memberTyVarSet x set =
	List.exists (fn y => eq_free(x,y)) set

      fun insertTyVarSet x set = 
	if memberTyVarSet x set then set else x::set

      fun unionTyVarSet(set1, set2) =
	  set1 @ 
	  List.all 
	    (fn x => not(memberTyVarSet x set1)) set2
      fun minusTyVarSet (set1, set2) =
	  List.all 
	  (fn x => not(memberTyVarSet x set2)) set1

      fun intersectTyVarSet (set1, set2) =
	List.all (fn x => memberTyVarSet x set1) set2
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



    structure Type = struct    
      local val r = ref 0 in
      fun freshRow () = VARrec (ref (NO_REC_INSTANCE (r := !r + 1 ; !r)))
      end

      fun layout ty =
	    let val ty = findType ty
	    in
	      (case #TypeDesc ty of
		 CONSTYPE ([], tyname) =>  TyName.layout tyname
	       | CONSTYPE (ty_list, tyname) =>
		   PP.NODE {start="(", finish=") " ^ TyName.pr_TyName tyname, indent=1,
			    children = map layout ty_list,
			    childsep = PP.LEFT ", "}
	       | RECTYPE r => RecType_layout r
	       | ARROW (ty, ty') =>
		   PP.NODE {start="(", finish=")", indent=1,
			    children=[layout ty, layout ty'],
			    childsep=PP.LEFT " -> "}
	       | TYVAR tv => TyVar.layout tv)
	    end
      and RecType_layout r = 
	    let
	      val (m, rv_opt) = sanitiseRecType r
	      val finish = (case rv_opt of
			      SOME rv => " ... " ^ pr_RowVar rv ^ "}"
			    | NONE => "}")
	    in
	      SortedFinMap.layoutMap  {start="{", eq=" : ", sep=", ", finish=finish}
		(PP.layoutAtom Lab.pr_Lab) layout m
	    end
      and pr_RowVar (ref (NO_REC_INSTANCE rho)) = "'r" ^ Int.string rho
	| pr_RowVar _ = die "pr_RowVar"

      val layout_with_level = fn ty => 
	    let val ty = findType ty in
	      PP.NODE {start="TypeDesc: ",
		       finish="\tlevel: " ^ Int.string (! (#level ty)),
		       indent=0, children=[layout ty], childsep=PP.NOSEP}
	    end


      local
	fun Type_eq0 eq_significant (ty,ty') =
	      let val (ty,ty') = (findType ty, findType ty')
	      in
		!(#level ty) = !(#level ty')
		andalso
		(case (#TypeDesc ty, #TypeDesc ty') of
		   (TYVAR (tv  as ref (NO_TYPE_INSTANCE _)),
		    TYVAR (tv' as ref (NO_TYPE_INSTANCE _))) =>
		      (if !(#level ty) = Level.GENERIC (*bound tyvars in type shceme*)
		       then TyVar.eq_bound eq_significant
		       else TyVar.eq_free) (tv,tv')
		  | (ARROW (ty1,ty1'), ARROW (ty2,ty2')) => 
		       Type_eq0 eq_significant (ty1,ty2)
		       andalso Type_eq0 eq_significant (ty1',ty2')
		  | (RECTYPE r1, RECTYPE r2) => RecType_eq0 eq_significant (r1,r2)
		  | (CONSTYPE (tys1,tyname1), CONSTYPE (tys2,tyname2)) => 
		       TyName.eq (tyname1, tyname2) andalso
		       TypeList_eq0 eq_significant (tys1,tys2)
		  | _ => false)
	      end

	and RecType_eq0 eq_significant (r1,r2) =
	      let val (r1,r2) = (findRecType r1, findRecType r2)
	      in
		(case (r1,r2) of
		   (NILrec, NILrec) => true
		 | (VARrec rho, VARrec rho') => rho = rho'
		 | (ROWrec (l1,ty1,r1'), ROWrec (l2,ty2,r2')) => 
		     l1 = l2
		     andalso Type_eq0 eq_significant (ty1,ty2)
		     andalso RecType_eq0 eq_significant (r1',r2')
		 | _ => false)
	      end

	and TypeList_eq0 eq_significant (tys1,tys2) =
	      List.foldL (fn (ty1,ty2) => fn b => 
			  b andalso Type_eq0 eq_significant (ty1,ty2))
	        true (ListPair.zip (tys1,tys2))
		   handle ListPair.Zip => false
      in
	val RecType_eq = RecType_eq0 EQ_SIGNIFICANT
	val eq = Type_eq0 EQ_SIGNIFICANT 
	           (*the eq_significant parameter has no significance :-) as 
		    there are no bound variables in a type*)
        val eq_equality_not_significant = Type_eq0 EQ_NOT_SIGNIFICANT
	      (*only used by TypeFcn.eq*)
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

	fun is_tuple_type (m, rv) = 
	      (SortedFinMap.matches
		 (fn (i, lab) => Lab.is_LabN(lab, i+1)) m,
	       rv)

	(*ziptypes l1 l2 = a list of type options; the list has the
	 same length as l2; it takes the form [SOME ty1', SOME ty2' ..., 
	 SOME ty_n', NONE, ..., NONE] where n' is min(length l1, length l2)*)

	fun ziptypes l1 [] = []
	  | ziptypes (x::xs) (y::ys)  = SOME x :: ziptypes xs ys
	  | ziptypes [] l2 = map (fn _ => NONE) l2

	(*fields_of_other_record (ty'_opt, fields) = a list of type
	 options of the same length of fields, consisting of types
	 taken from ty', if ty' is record type, and consisting of NONE
	 otherwise*)

	fun fields_of_other_record (ty'_opt : Type option, fields : Type list) =
	      case ty'_opt of 
		SOME {TypeDesc = RECTYPE r', ...} =>
		  let val r' = findRecType r'
		      val (m', rv') = sanitiseRecType r' 
		  in 
		      ziptypes (SortedFinMap.rangeSORTED m') fields
		  end
	      | _ => map (fn field => NONE) fields

	(*TyName_string_as_opt (tyname, tyname'_opt)  prints tyname as tyname'_opt, if 
	 the latter is present and the type names are the same. If the type names
	 are different, they are printed differently, even if they have the same
	 tycon*)

	fun full_works tyname = concat [TyName.pr_TyName tyname, "<", 
					 Int.string (TyName.id tyname) , ">"]
	fun TyName_string_as_opt (tyname, tyname'_opt) =
	     case tyname'_opt of 
	       SOME tyname' => 
		  if TyName.eq (tyname,tyname') then TyName.pr_TyName tyname 
		  else if TyName.tycon tyname = TyName.tycon tyname'
		       then full_works tyname
		       else TyName.pr_TyName tyname
	     | NONE => TyName.pr_TyName tyname

	fun pretty_string_as_opt names precedence
	  (ty : Type, ty'_opt : Type option) =
	      let val ty = findType ty
		  val ty'_opt = map_opt findType ty'_opt
	      in
		(case #TypeDesc ty of
		   TYVAR tv => TyVar.pretty_string names tv

		 | RECTYPE r =>		(* See if we can print it as `a * b * ...'
					 rather than `{1: a, 2: b, ...}' *)
		     let
		       val r = findRecType r
		       val (m, rv) = sanitiseRecType r
		     in
		       (case is_tuple_type(m, rv) of
			  (true, NONE) => (*A possible (t1 * t2 * ...) type, and
					   no rowvar. *)
			    print_tuple names precedence (m, ty'_opt)
			| _ => (*Have to do the general print.*)
			    let
			      val fields = SortedFinMap.rangeSORTED m
			      val fields' = fields_of_other_record (ty'_opt, fields)
			      val field_types = map (pretty_string_as_opt names 1)
					              (ListPair.zip (fields,fields'))
			      val labels = map Lab.pr_Lab (SortedFinMap.domSORTED m)
			      fun colon_between (lab, ty) = lab ^ ": " ^ ty
			    in
			      List.stringSep "{" "}" ", " colon_between
			        (ListPair.zip (labels,field_types))
			    end)
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
		               (case ty'_opt of 
				  SOME {TypeDesc = CONSTYPE(tys', tyname'), ...} => 
				    (ziptypes tys' tys, SOME tyname')
				| _ => (map (fn _ => NONE) tys, NONE))
		     in
		       (case (tys, tys'_opt) of
			  (nil,_) => TyName_string_as_opt (tyname, tyname'_opt)
			| ([ty], [ty']) =>
			    concat [pretty_string_as_opt names 4 (ty,ty'), " ",
				     TyName_string_as_opt (tyname, tyname'_opt)]
			| _ =>
			    concat [List.stringSep "(" ") " ", "
				       (pretty_string_as_opt names 1)
				          (ListPair.zip (tys,tys'_opt)), 
				     " ",
				     TyName_string_as_opt (tyname, tyname'_opt)]) 
		     end)
	      end

	and print_tuple names precedence (m, ty'_opt: Type option)  =
			    (* Careful: "{1=x}" does *not* print as "(x)", 
			       and "{ }" should be "unit". We
			       don't do this folding at all if there's a row var. *)
	      let
		val fields = SortedFinMap.rangeSORTED m
		val fields' = 
		      (case ty'_opt of 
			 SOME {TypeDesc = RECTYPE r', ...} =>
			   let val r' = findRecType r'
			       val (m', rv') = sanitiseRecType r' 
			   in (case (SortedFinMap.matches 
				       (fn (i,lab) => Lab.is_LabN (lab, i+1)) m',
				     rv') of
			         (true, NONE) =>
				   (*A possible (t1' * t2' *  ) type, and no rowvar:*)
				   ziptypes (SortedFinMap.rangeSORTED m') fields
			       | _ => map (fn field => NONE) fields)
			   end
		       | _ => map (fn field => NONE) fields)
	      in
		(case (fields, fields') of
		   (nil, _) => "unit"	(* Hard-wired *)
		 | ([x], [x']) => "{1: " ^ pretty_string_as_opt  names 1 (x,x') ^ "}"
		 | _ => parenthesize (3, precedence,
				      List.stringSep "" "" " * "
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
      fun debug_print from ty = 
	    if !Flags.DEBUG_TYPES then 
	      print_node {start=from ^ ": ", finish="", indent=0,
			  children=[layout_with_level ty],
			  childsep=PP.NOSEP}
	    else ()
      fun debug_print_RecType from r = 
	    if !Flags.DEBUG_TYPES then 
	      print_node {start=from ^ ": ", finish="", indent=0,
			  children=[RecType_layout r],
			  childsep=PP.NOSEP}
	    else ()
      fun from_TyVar tyvar = {TypeDesc = TYVAR tyvar, level = ref (Level.current ())}
      fun from_TyVar' level tyvar = 
	    if level = Level.GENERIC then  
	      (*The tyvar does not occur unguarded in any value
	       declaration, i.e. it must occur in a typbind, datbind,
	       valdesc, typdesc, or datdesc --- Def. Sec. 4.6*)
	      from_TyVar tyvar
	    else
	      (*The tyvar occurs unguarded in a value declaration
	       at level level, i.e. it occurs in the " : ty" of a typed exp or
	       typed pat or in the " of ty" of an exbind --- Def. Sec. 4.6*)
	      {TypeDesc = TYVAR tyvar, level = ref level}
      fun to_TyVar ty = 
	    (case #TypeDesc (findType ty) of
	       TYVAR tyvar => SOME tyvar
	     | _ => NONE)
      val fresh0 = from_TyVar o TyVar.fresh0
      val fresh_normal = from_TyVar o TyVar.fresh_normal
      fun from_RecType r = {TypeDesc = RECTYPE r, level = ref Level.NONGENERIC}
      fun to_RecType ty =
	    (case #TypeDesc (findType ty) of
	       RECTYPE t => SOME t
	     | _ => NONE)

      (*contains_row_variable tau = true iff there exists a row variable in tau*)
      fun RecType_contains_row_variable r =
	    (case findRecType r of
	       NILrec => false
	     | VARrec _ => true
	     | ROWrec (_, ty, r') => contains_row_variable ty
		                     orelse RecType_contains_row_variable r')
      and contains_row_variable t =
	    (case #TypeDesc (findType t) of 
	       TYVAR _ => false
	     | ARROW (t1, t2) => contains_row_variable t1
		          orelse contains_row_variable t2
	     | RECTYPE r => RecType_contains_row_variable r
	     | CONSTYPE (tylist, _) => List.exists contains_row_variable tylist)

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
	      (case sanitiseRecType r of (m, _) => SortedFinMap.domSORTED m)
	fun to_list r = 
	      let val m = #1 (sanitiseRecType r)
	      in
		ListPair.zip (SortedFinMap.domSORTED m, SortedFinMap.rangeSORTED m)
	      end handle ListPair.Zip => die "to_list"
	fun to_pair r =
	      (case sanitiseRecType r of
		 (m, NONE) =>
		   (case SortedFinMap.lookup m ONE of
		      NONE => die "Type.to_pair(L=?)"
		    | SOME tyL =>
			(case SortedFinMap.lookup m TWO of
			   NONE => die "Type.to_pair(R=?)"
			 | SOME tyR => (tyL, tyR)))
	       | (_, SOME _) => (*It's flexible: punt*)
		   die "Type.to_pair(flexible)")
	fun sort (r : RecType) : RecType =
	      let
		val (m,rho_opt) = sanitiseRecType r
		val dom = SortedFinMap.domSORTED m
		val range = SortedFinMap.rangeSORTED m
	      in
		List.foldR
		  (fn (lab,ty) => fn r => ROWrec(lab,ty,r))
		      (case rho_opt of
			 SOME rho => VARrec rho
		       | NONE => NILrec)
			   (ListPair.zip (dom,range))
	  end handle ListPair.Zip => die "RecType.sort"

      end (*RecType*)

      (*Find free and bound type variables*)
      local 
	datatype free_or_bound = FREE | BOUND

	val bucket = ref ([] : TyVar list)
	fun insert_tv eq tv = if List.exists (fn tv' => eq(tv,tv')) (!bucket) then ()
			      else bucket := (tv :: (!bucket))
	fun insert FREE tv = insert_tv TyVar.eq_free tv
	  | insert BOUND tv = insert_tv (TyVar.eq_bound EQ_SIGNIFICANT) tv

	fun tyvars0 f_b ty : unit = 
	  let val ty = findType ty 
	  in case #TypeDesc ty 
	       of TYVAR (tl as ref (NO_TYPE_INSTANCE _)) => 
		 (case f_b 
		    of FREE => if !(#level ty) = Level.GENERIC then () else insert f_b tl
		     | BOUND => if !(#level ty) = Level.GENERIC then insert f_b tl else ())
		| TYVAR (ref (TYPE_INSTANCE _))  => die "tyvars0"
		| RECTYPE r => RecType.fold (fn (ty, ()) => tyvars0 f_b ty) () (findRecType r)
		| ARROW (ty,ty') => (tyvars0 f_b ty; tyvars0 f_b ty')
		| CONSTYPE (types,_) => List.foldL (fn ty => fn () => tyvars0 f_b ty) () types
	  end

        fun tyvars1 f_b ty = (bucket := []; tyvars0 f_b ty; !bucket)

      in

	(* Some code in the compiler (compilation of primitives, e.g.)
	 * depend on the exact order the bound variables are
	 * extracted from a type - so expect things to blow up if you
	 * modify this very code.
	 *   This code for finding bound variables *must* also
	 * correspond to the fake_generalise code for finding bound
	 * variables. The fake_generalise code finds variables which
	 * will later be generalised. This certainly *is* ugly - and
	 * its a miracle that (or if) it works. 31/10/97-Martin *)
	
	val tyvars = tyvars1 FREE
	val generic_tyvars = tyvars1 BOUND 
      end

      fun tynames ty =
	    let val ty = findType ty
	    in 
	      case #TypeDesc ty of
		TYVAR _ => TyName.Set.empty 
	      | RECTYPE r =>
		  RecType.fold
		    (fn (ty, T) => TyName.Set.union (tynames ty) T)
		      TyName.Set.empty  r
	      | ARROW (ty, ty') => TyName.Set.union (tynames ty) (tynames ty')
	      | CONSTYPE (types, tyname) =>
		  List.foldL
		    (fn ty => fn T => TyName.Set.union (tynames ty) T)
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

      (*function types*)
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

      (*constructed types*)
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

      (*special constants*)
      val Int    = mk_ConsType ([], TyName.tyName_INT)
      val Real   = mk_ConsType ([], TyName.tyName_REAL)
      val String = mk_ConsType ([], TyName.tyName_STRING)
      val Bool   = mk_ConsType ([], TyName.tyName_BOOL)
      val Char   = mk_ConsType ([], TyName.tyName_CHAR)
      val Word8  = mk_ConsType ([], TyName.tyName_WORD8)
      val Word   = mk_ConsType ([], TyName.tyName_WORD)

      fun simple_scon ty = {type_scon = ty, overloading = NONE}
      fun of_scon (SCon.INTEGER _) = simple_scon Int
	| of_scon (SCon.STRING _) = simple_scon String
	| of_scon (SCon.REAL _) = simple_scon Real
	| of_scon (SCon.CHAR _) = simple_scon Char
	| of_scon (SCon.WORD i) = 
	if i > 255 then simple_scon Word
	else let val tv = TyVar.fresh_overloaded [TyName.tyName_WORD8, TyName.tyName_WORD]
	     in {type_scon=from_TyVar tv, overloading=SOME tv}
	     end 

      (*generalise and fake_generalise:*)

      local
	val ordinaryMap = ref ([] : TyVar list)
	val explicitMap = ref (FinMap.empty : (ExplicitTyVar,TyVarDesc) FinMap.map)
	val bound_id = ref 0;
	val escaping_tyvars = ref ([]: TyVar list)
	   (* type variables escaping due to the value polymorphism *)
	val generic_tyvars = ref ([]: TyVar list)
	  (* when faking, we just collect those type variables which could be 
	   * generalised in generic_tyvars, but do not actually generalise
	   * the variables, i.e. we do not modify the levels.
	   *)
	fun insert r tv = 
	  let 
	    (* We need to compare both with TyVar.eq_free and TyVar.eq_bound 
	     * because as fake_generalise does not modify the levels, insert
	     * can be called with type variables that are different according to 
	     * TyVar.eq_free, but which are equal according to TyVar.eq_bound 
	     * --- XXX this equality stuff is certainly a mess, and should be 
	     * cleaned up, c.f. the discussion of explicit type variables in 
	     * a comment in the beginning of this file.
	     * 14/03/1997 12:52. tho.: TyVar.eq_free \/ TyVar.eq_bound EQ_SIGNIFICANT
	     * is the same as TyVar.eq.
	     *)
	    fun mem x set = List.exists (fn y => TyVar.eq (x,y)) set
	    fun insert0 x set = if mem x set then set else x::set
	  in
	    r := insert0 tv (!r)
	  end
	val faker = ref false
	fun reset () = (bound_id := 0; 
			ordinaryMap := [];
			explicitMap := FinMap.empty;
			generic_tyvars := [];
			escaping_tyvars := [])
	fun next_bound_id () = (bound_id := !bound_id - 1; !bound_id)
	fun mk_bound_tyvar0 (tv as (ref (NO_TYPE_INSTANCE(EXPLICIT ExplicitTyVar)))) =
	      (case FinMap.lookup (!explicitMap) ExplicitTyVar of
		 NONE =>  let val tvdesc' = ORDINARY
		             {id = next_bound_id(),
			      equality = ExplicitTyVar.isEquality ExplicitTyVar,
			      rank=dummy_rank_ref,
			      overloaded = TyVar.get_overloaded tv}
			  in 
			    tv := NO_TYPE_INSTANCE tvdesc';
			    explicitMap := FinMap.add(ExplicitTyVar,tvdesc',!explicitMap)
			  end
	       | SOME tvdesc' => tv := NO_TYPE_INSTANCE tvdesc')

	  | mk_bound_tyvar0 (tv as (ref (NO_TYPE_INSTANCE(ORDINARY {equality, ...})))) =
	      if List.member tv (!ordinaryMap) then ()
	      else
		(ordinaryMap := tv::(!ordinaryMap);
		 tv := NO_TYPE_INSTANCE(ORDINARY{id = next_bound_id(),
						 equality = equality,
						 rank=dummy_rank_ref,
						 overloaded = TyVar.get_overloaded tv}))

	  | mk_bound_tyvar0 _ = die "mk_bound_tyvar0"

	fun mk_bound_tyvar tv level =
	      (insert generic_tyvars tv ;
	       if !faker then ()
	       else (level := Level.GENERIC ; mk_bound_tyvar0 tv))

	fun mk_escaping_tyvar tv level =
	      (insert escaping_tyvars tv ;
	       if !faker then ()
	       else level := Level.current ())
	      

	local

	fun generalise0 {ov, imp, tau} = 

	  (*generalise overloaded type variables  iff  ov and level > current_level.
	   generalise imperative type variables  iff imp and level > current_level.
	   Do not call generalise0 directly; use generalise1 instead, as it
	   initialises things correctly.*)

	  let val tau = findType tau
	      val {TypeDesc, level} = tau
	  in
	    debug_print "generalise0 IN " tau ;
	    if !level = Level.GENERIC then ()
	    else
	      (case TypeDesc of
		 TYVAR tv =>
		   if !level > Level.current ()
		   then if ov orelse not (TyVar.is_overloaded tv)
			then if imp then mk_bound_tyvar tv level
			     else mk_escaping_tyvar tv level
			else level := Level.current ()
		   else ()
	       | ARROW (tau1, tau2) => 
		    level := Int.min (generalise0 {ov=ov, imp=imp, tau=tau1}) 
		                     (generalise0 {ov=ov, imp=imp, tau=tau2})
	       | RECTYPE r => 
		    level := generaliseRecType {ov=ov, imp=imp, r=r}
	       | CONSTYPE (taus, tyname) => 
		    level := List.foldL 
		               Int.min Level.NONGENERIC
			           (map (fn tau => generalise0
					     {ov=ov, imp=imp, tau=tau}) taus)) ;
	    debug_print "generalise0 OUT" tau ;
	    ! (#level tau)
	  end

	and generaliseRecType {ov, imp, r} =
	      let val r = findRecType r 
	      in
		debug_print_RecType "generaliseRecType" r ;
		(case r of
		   NILrec => Level.NONGENERIC
		 | VARrec (ref (NO_REC_INSTANCE _)) => Level.NONGENERIC
		 | VARrec (ref (REC_INSTANCE _)) => die "generaliseRecType"
		 | ROWrec (l,tau,r') => 
		     Int.min (generalise0 {ov=ov, imp=imp, tau=tau})
	                     (generaliseRecType {ov=ov, imp=imp, r=r'}))
	      end
	in
	fun generalise1 {fake, ov, imp, tau} =
	      (reset () ; faker := fake ; generalise0 {ov=ov, imp=imp, tau=tau} ;
	       (!generic_tyvars, !escaping_tyvars))
	end (*local*)
      in
	fun generalise_and_return_escaping_tyvars {ov, imp, tau} =
	      #2 (generalise1 {fake=false, ov=ov, imp=imp, tau=tau})
	fun generalise {ov, imp, tau} =
	      (generalise_and_return_escaping_tyvars {ov=ov, imp=imp, tau=tau}; ())
	fun fake_generalise imp tau =
	      #1 (generalise1 {fake=true, ov=false, imp=imp, tau=tau})
      end (*local*)

      (* close imp tau = a list of those type variables of tau which are
       * allowed to be quantified.  For the compiler to work it is important
       * that fake_generalise returns type variables as generalise followed
       * by generic_tyvars (to follow).  If imp, then the expression with
       * type tau is non-expansive and type variables should be generalised.*)

      val close = fake_generalise
      

      fun copy ov imp ty =
	let
	  val seenB4 = ref (FinMapEq.empty : (TyVarDesc,Type) FinMapEq.map)
	  fun add_to_seenB4 (x,y) = seenB4 := FinMapEq.add TyVarDesc_eq (x,y,!seenB4)
	  fun lookup_seenB4 x = FinMapEq.lookup TyVarDesc_eq (!seenB4) x 

	  fun copyType ty = 
	    let
	      val _ = debug_print "copyType IN " ty
	      val copy = 
		let val ty = findType ty
		in
		  case findType ty of 
		    (fty as {TypeDesc = TYVAR (tv as ref (NO_TYPE_INSTANCE tvdesc)), level}) =>
		     (* copy type variable, if it will become generalised *)		       
		      if !level > Level.current () andalso imp
			andalso (ov orelse not (TyVar.is_overloaded tv))
			then
			  (case lookup_seenB4 tvdesc of 
			     NONE => 
			       let val ty = 
				 {TypeDesc = TYVAR (ref (NO_TYPE_INSTANCE tvdesc)),
				  level = ref (!level)} 
			       in add_to_seenB4(tvdesc,ty) ; ty
			       end
			   | SOME ty => ty)
			else fty
		  | {TypeDesc = TYVAR _,level} => 
		     die "copyType"
		  | {TypeDesc = ARROW(ty1,ty2), level} => 
		     {TypeDesc = ARROW(copyType ty1,copyType ty2),
		      level = ref (!level)}
		  | {TypeDesc = RECTYPE r, level} => 
		     {TypeDesc = RECTYPE (copyRecType r),
		      level = ref (!level)}
		  | {TypeDesc = CONSTYPE(tys,tyname),level} => 
		     {TypeDesc = CONSTYPE(map copyType tys,tyname),
		      level = ref (!level)}
		end
	    in
	      debug_print "copyType OUT" copy;
	      copy
	    end

	  and copyRecType r =
	    let val r = findRecType r
	    in case r of
		 NILrec => r
	       | VARrec _ => r
	       | ROWrec(l,ty,r') => ROWrec(l,copyType ty,copyRecType r')
	    end

	in
	  copyType ty
	end

      (*make_equality tau = SOME S iff S is a substitution that
       makes tau an equality type.  NONE otherwise:*)

      local
	exception NotEquality

	fun make_equality0 ty = 
	      (case #TypeDesc (findType ty) of
		 TYVAR (tv as (ref (NO_TYPE_INSTANCE (ORDINARY {overloaded, id, rank, ...})))) =>
		   tv := NO_TYPE_INSTANCE (ORDINARY {equality = true,
						     overloaded = overloaded,
						     rank = rank,
						     id = id})
	       | TYVAR (tv as ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar))) =>
		   if ExplicitTyVar.isEquality ExplicitTyVar then () else raise NotEquality
	       | TYVAR _ => die "make_equality"
	       | RECTYPE r => RecType.apply make_equality0 r
	       | CONSTYPE (ty_list, tyname) =>
		   if TyName.eq (tyname, TyName.tyName_REF) then ()
		     (*"ref" is a special case; take it out straight away,
		      otherwise we'll damage any tyvars within the arg to "ref".*)
		   else if TyName.equality tyname then
		     List.apply make_equality0 ty_list
			else raise NotEquality
	       | ARROW _ => raise NotEquality)
      in
	fun make_equality tau =
	      SOME (make_equality0 tau) handle NotEquality => NONE
      end


      datatype unify_result = UnifyOk (* of Substitution *)
	                    | UnifyFail 
                            | UnifyRankError of TyVar * TyName


      (*******
      Unify types (tau,tau') s.t. range (subst) intersection restr_tyvars = empty
      *******)

      fun restricted_unify (restricted_tyvars : TyVar list) (tau, tau') =
      let

	(********
	This local exception is raised when a unification error occurs
	********)

	exception Unify of string
	      and Rank of TyVar * TyName

	(********
	The `occurs' check for tyvars in tys; besides doing the occurs
	check the level of type variables in ty with level > lev is
	set to lev. Further, the rank of type variables in ty with
	rank > rnk is set to rnk. Also, since unification should fail
	if a type variable with rank r1 is unified with a type
	containing type names with ranks greater than r1, we check for
	this here and raise Unify if this is the case.
	********)

	fun occurs_tv_in_Type (lev: level) (rnk: rank) (tv: TyVar) (ty: Type): bool =
	  let
	    fun occursType ty = 
	      case findType ty of
		{TypeDesc = TYVAR tv', level} =>
		  (if !level > lev then level := lev else ();
		     (case tv'
			of ref (NO_TYPE_INSTANCE (ORDINARY {overloaded, id, rank, ...})) =>
			        if Rank.<=(rnk,!rank) then rank := rnk
				else ()
			 | ref (NO_TYPE_INSTANCE _) => ()
			 | _ => die "occursType") ;
		   tv = tv')
	      | {TypeDesc = ARROW(ty1, ty2), level} =>
		  occursType ty1 orelse occursType ty2
	      | {TypeDesc = CONSTYPE(tys, tn), level} =>
		  if Rank.<=(Rank.from_TyName tn, rnk) then
		    List.foldL (fn ty => fn b => b orelse occursType ty) false tys
		  else raise Rank(tv,tn)
	      | {TypeDesc = RECTYPE r, level} =>
		  RecType.fold (fn (ty, b) => b orelse occursType ty) false r
	  in
	    occursType ty
	  end

	fun occurs_rv_in_RecType(rv, r) =
	  case findRecType r of
	    NILrec => false
	  | VARrec rv' => (rv = rv')
	  | ROWrec(_, ty, r') => 
	      occurs_rv_in_Type(rv, ty)  orelse occurs_rv_in_RecType(rv, r')

	and occurs_rv_in_Type(rv, ty) =
	  case #TypeDesc (findType ty) of
	    TYVAR _ => false
	  | ARROW(ty1, ty2) => 
	      occurs_rv_in_Type(rv, ty1) orelse occurs_rv_in_Type(rv, ty2)
	  | RECTYPE r => occurs_rv_in_RecType(rv, r)
	  | CONSTYPE(tys, _) =>
	      List.foldL
	      (fn ty => fn result => occurs_rv_in_Type(rv, ty) orelse result)
	      false tys

	(*unify_with_overloaded_tyvar tynames1 tau = unify an overloaded
	 tyvar with tau.  `tynames1' is the set of allowed tynames,
	 i.e., the set of tynames that the tyvar may be unified with.  For
	 instance, + may get the type `alpha * alpha -> alpha', where alpha
	 is an overloaded tyvar, and then `tynames1' for alpha will
	 be {TyName.tyName_INT, TyName.tyName_WORD, TyName.tyName_REAL}.
	 Raise Unify if `tau' is neither a tyname in `tynames1',
	 nor another tyvar.  If `tau' is itself an overloaded tyvar, the
	 `overloaded' field of the resulting tyvar should be the intersection
	 of the `overloaded' fields on the two unified tyvars.  29/08/1997
	 15:02.  tho.  But if tau is not overloaded, we must not change the
	 set `tynames1'.  11/11/1997 16:52. tho.*)

	fun unify_with_overloaded_tyvar tynames1 tau = 
	      (case #TypeDesc (findType tau) of
		 TYVAR (tv as (ref (NO_TYPE_INSTANCE (ORDINARY
			  {equality, id, overloaded=Nonoverloaded, rank, ...})))) =>
		   tv := NO_TYPE_INSTANCE (ORDINARY
			   {equality = equality, id = id, rank = rank,
			    overloaded=Overloaded tynames1})
	       | TYVAR (tv as (ref (NO_TYPE_INSTANCE (ORDINARY
			  {equality, id, overloaded=Overloaded tynames2, rank, ...})))) =>
		   tv := NO_TYPE_INSTANCE (ORDINARY
			   {equality = equality, id = id, rank = rank,
			    overloaded=Overloaded (TyName.Set.intersect
						     tynames1 tynames2)})
	       | TYVAR (ref (NO_TYPE_INSTANCE (EXPLICIT _))) =>
		   raise Unify "unify_with_overloaded_tyvar: explicit tyvar"
	       | CONSTYPE (taus, tyname) =>
		   if List.exists (General.curry TyName.eq tyname)
		        (TyName.Set.list tynames1) then ()
		   else raise Unify "unify_with_overloaded_tyvar: not overloaded to this tyname"
	       | _ => raise Unify "unify_with_overloaded_tyvar: only overloaded to tynames")

	(********
	unify_with_tyvar: Check the attributes of an ordinary TyVar are satisfied
	*********
	We assume the `occurs' check has already been done
	********)

	fun unify_with_tyvar  
	  (tv as ref (NO_TYPE_INSTANCE
		      (ORDINARY {equality, overloaded, ...})), tau) =
	      (if !Flags.DEBUG_TYPES then
		 (TextIO.output(TextIO.stdOut,"unify_with_tyvar: linking tv = " ^ 
			 (TyVar.pretty_string NONAMES tv) ^  " to") ;
		 debug_print "" tau)
	       else () ;
	       let
		 val S =
		   if equality then (case make_equality tau of
				       SOME S => S
				     | NONE => raise Unify "unify_with_tyvar.1")
		   else Substitution.Id

		   val S' = (case overloaded of
			       Overloaded tynames =>
				 unify_with_overloaded_tyvar tynames
				   (Substitution.on (S, tau))
			     | Nonoverloaded => Substitution.Id)
	       in
		 if List.member tv restricted_tyvars
		 then raise Unify "unify_with_tyvar.2"
		 else tv := TYPE_INSTANCE tau
	       end)
	  | unify_with_tyvar _ = die "unify_with_tyvar"

	(********
	Check if we can safely unify an explicit TyVar and type
	--- assumes findType has been applied
	********)

	fun unifyExplicit 
	    (ty  as {TypeDesc = TYVAR (tv as ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar))),
		     level=level},
	     ty' as {TypeDesc = TYVAR (tv' as ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar'))),
		     level=level'}) =

	    if ExplicitTyVar = ExplicitTyVar' then () else raise Unify "unifyExplicit"
	      (* To save a little space one could link tv to ty' in the then case *)

	  | unifyExplicit 
	    (ty  as {TypeDesc = TYVAR (tv as ref (NO_TYPE_INSTANCE (EXPLICIT ExplicitTyVar))),
		     level=level},
	     ty' as {TypeDesc = TYVAR(tv' as ref (NO_TYPE_INSTANCE (ORDINARY r))),
		     level=level'}) =

	    (*Notice below that unification of overloaded type variable and
	     explicit type variable is not allowed*)

	     let
	       val {equality, overloaded, ...} = r
	     in
	       (case
		  (equality, ExplicitTyVar.isEquality ExplicitTyVar) of
		  (false,  _  ) => ()
		| (true , true) => ()
		| ( _   ,  _  ) => raise Unify "unifyExplicit.2";

		 if List.member tv' restricted_tyvars then raise Unify "unifyExplicit.3"
		 else 
		   if TyVar.is_overloaded tv' then raise Unify "unifyExplicit.4"
		   else
		     if !level' < !level then 

		       (*Test example: 
			  (fn x => let val f = fn (y: ''a) => x = y in 2 end);*)

		       (*This exception is raised if an explicit type variable is
			unified with a type variable with lower level, since then
			the side condition concerning free type variables on rule
			17 in the Definition is not satisfied*)

		       raise Unify "unifyExplicit.5"
		     else 
		       tv' := TYPE_INSTANCE ty)
	     end

	  | unifyExplicit (_, _) = raise Unify "unifyExplicit.5"

	(*unifyTyVar (tau, tau') = Check whether we can safely unify a TyVar
	 (the first tau) and a type (tau').  unifyTyVar assumes findType has
	 been applied, and that tau & tau' aren't equal*)

	fun unifyTyVar (ty, ty') = (if !Flags.DEBUG_TYPES then
				      TextIO.output(TextIO.stdOut,"unifyTyVar\n") else () ;
				    unifyTyVar0 (ty, ty'))
	and unifyTyVar0 (ty as {TypeDesc = TYVAR(ref(NO_TYPE_INSTANCE
						     (EXPLICIT _))),...}, 
			 ty') = unifyExplicit(ty,ty')

	  | unifyTyVar0
	      (ty  as {TypeDesc = TYVAR(tv as ref(NO_TYPE_INSTANCE(ORDINARY{rank=rank_ref as ref r, ...}))),
		       level=level},
	       ty' as {TypeDesc = TYVAR(tv' as ref(NO_TYPE_INSTANCE(ORDINARY{rank=rank_ref' as ref r', ...}))),
		       level=level'}) =
	      let val rank = Rank.min(r,r')
	      in rank_ref := rank ;
		 rank_ref' := rank ;
	          if List.member tv restricted_tyvars then
		    if List.member tv' restricted_tyvars then raise Unify "unifyTyVar0"
		    else (level := Int.min (!level) (!level') ;
			  unify_with_tyvar(tv',ty))
		  else
		    (level' := Int.min (!level) (!level') ;
		     unify_with_tyvar(tv,ty'))
	      end
	  | unifyTyVar0 (ty  as {TypeDesc = TYVAR(tv as ref(NO_TYPE_INSTANCE(ORDINARY {rank, ...}))),
				 level = level},
			 ty') =
		  if occurs_tv_in_Type (!level) (!rank) tv ty'
		    orelse List.member tv restricted_tyvars
		    then raise Unify "unifyTyVar0.2"
		  else unify_with_tyvar(tv, ty')

	  | unifyTyVar0 _ = die "unifyTyVar0"


	(********
	Unify two types
	********)

	fun unifyType(ty,ty') =
	  let 
	    val (ty,ty') = (findType ty,findType ty')
	  in
	    if !Flags.DEBUG_TYPES then 
	      print_node {start="unifyType: ", finish="",indent=0,
			  children=[layout_with_level ty,
				    layout_with_level ty'],
			  childsep=PP.RIGHT ", "}
	    else () ;
	    if eq(ty, ty') then ()
	    else
	      (case (#TypeDesc ty,#TypeDesc ty') of
		 (TYVAR _, _) => unifyTyVar (ty, ty')
	       | (_, TYVAR _) => unifyTyVar (ty', ty)
	       | (RECTYPE r, RECTYPE r') => unifyRecType(r, r')
	       | (ARROW pair, ARROW pair') => unifyFunType(pair, pair')
	       | (CONSTYPE pair, CONSTYPE pair') => unifyConsType(pair, pair')
	       | (_, _) => raise Unify "unifyType")
	  end

	and unifyRecType(r1, r2) = unifyRow(r1, r2)

	and extract (lab: Lab.lab, row: RecType): RecType =
	  let
	    val row = findRecType row
	    val _ =
	      if !Flags.DEBUG_FLEXRECORDS then
		print_node {start="extract: ", finish="", indent=0,
			    children=[PP.LEAF (Lab.pr_Lab lab),
				      layout_with_level {TypeDesc=RECTYPE row,
							 level=ref Level.NONGENERIC}],
			    childsep=PP.LEFT " from "}
	      else ()

	    fun rowVar NILrec = NONE
	      | rowVar(VARrec rv) = SOME rv
	      | rowVar(ROWrec(_, _, row')) = rowVar (findRecType row')

	    val result =
	      case rowVar row
		of NONE => raise Unify "extract"
		 | SOME rv =>
		     let
		       val littleRow = 
			 ROWrec (lab, fresh_normal (), freshRow ())
		     in
		       (unifyRowVar(rv,littleRow);
			row)
		     end
	  in
	    result
	  end

	and unifyRow(row1: RecType, row2: RecType): Substitution =
	  let 
	    val (row1,row2) = (RecType.sort row1, RecType.sort row2) 
	  in
	    if !Flags.DEBUG_FLEXRECORDS then
	      print_node {start="unifyRow(", finish=")", indent=0,
			  children=[layout_with_level {TypeDesc=RECTYPE row1,
						       level=ref Level.NONGENERIC},
				    layout_with_level {TypeDesc=RECTYPE row2,
						       level=ref Level.NONGENERIC}],
			  childsep=PP.RIGHT "; "} else () ;
	    unifyRow' (row1, row2)
	  end

	and unifyRowVar(rv, row) =
	  if occurs_rv_in_RecType(rv, row) then raise Unify "unifyRowVar"
	  else rv := REC_INSTANCE row

	and unifyRow'(row1: RecType, row2: RecType) =
	  (* Assumes findRecType and RecType.sort has been applied --- done in unifyRow *)
	  if RecType_eq (row1, row2) then ()
	  else
	    case (row1, row2)
	      of (NILrec, NILrec) => Substitution.Id
	    | (_, VARrec rho) => unifyRowVar(rho, row1)
	    | (VARrec rho, _) => unifyRowVar(rho, row2)

	    | (ROWrec(lab1, ty1, row1'), ROWrec(lab2, ty2, row2')) =>
		if lab1 = lab2 then
		  (unifyType(ty1, ty2); unifyRow(row1',row2'))
		else 
		  if Lab.<(lab1, lab2) then	(* Pad out row2, try again *)
		    (extract(lab1,row2); unifyRow(row1,row2))
		  else				(* Pad out row1, try again *)
		    (extract(lab2,row1);unifyRow(row1,row2))

	    | _ => raise Unify "unifyRow'"

	and unifyFunType((tau1, tau2), (tau3, tau4)) =
	  (unifyType(tau1, tau3); unifyType(tau2, tau4))

	and unifyConsType((ty_list, tyname), (ty_list', tyname')) =
	  if TyName.eq(tyname, tyname') then
	    (* Note that tyname=tyname' implies length(ty_list)=length(ty_list') *)
	    List.apply unifyType (ListPair.zip(ty_list, ty_list'))
	  else
	    raise Unify "unifyConsType"
      in
	(********
	Unify two types, handling the exception generated if we fail
	********)
	  (unifyType (tau,tau'); UnifyOk)
	  handle Unify s => UnifyFail
	       | Rank(tv,tn) => UnifyRankError(tv,tn)
      end

      val unify = restricted_unify nil

      fun instantiate_arbitrarily tyvar =
	    (case unify (from_TyVar tyvar, Int) of
	       UnifyOk => ()
	     | _ => die "instantiate_arbitrarily")

      (*Matching functions for compilation manager*)

      local
	fun match_Type (ty,ty0) : unit =
	      let val (ty, ty0) = (findType ty, findType ty0)
	      in case (#TypeDesc ty,#TypeDesc ty0) of
		(ARROW (tau,tau'), ARROW (tau0,tau0')) =>
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
	      (case (findRecType rect, findRecType rect0) of
		 (ROWrec (l,tau,rect), ROWrec (l0,tau0,rect0)) => 
		   if l <> l0 then ()
		   else (match_Type (tau,tau0) ; match_RecType (rect,rect0))
	       | _ => ())
      in
	val match = match_Type
      end (*local*)
    end (*Type*)



    type TypeScheme = TyVar list * Type

    structure TypeScheme = struct
      fun eq((_,tau1),(_,tau2)) = Type.eq(tau1,tau2)
      val to_TyVars_and_Type = fn x => x
      val from_Type = fn tau => ([],tau)
      val tyvars = fn (_,tau) => Type.tyvars tau
      val tynames = fn (_,tau) => Type.tynames tau
      val string = fn (_, tau) => Type.string tau
      fun pretty_string tvn (_, tau) = Type.pretty_string tvn tau
      val layout = fn (_, tau) => Type.layout tau
      fun debug_print s (_,tau) = Type.debug_print s tau

      exception InstanceError of string
      fun instance' (tyvars,ty) =
	  let fun fresh (tv as ref(NO_TYPE_INSTANCE tvdesc)) = (tvdesc, Type.from_TyVar(TyVar.refresh tv))
		| fresh _ = die "instance'.fresh"

	      val M : (TyVarDesc * Type) list = map fresh tyvars

	      fun lookup_M x = 
		let fun find [] = NONE
		      | find ((y,tau)::ys) = if TyVarDesc_eq(x,y) then SOME tau
					     else find ys
		in find M
		end
	      
	      fun instanceType ty = 
		    let val ty = findType ty in
		    if !(#level ty) <> Level.GENERIC then ty else
		    (case #TypeDesc ty 
		       of TYVAR (tv as ref (NO_TYPE_INSTANCE(tvdesc as (ORDINARY _)))) =>
			 (case lookup_M tvdesc 
			    of SOME ty' => ty'
			     | NONE => (*die*) raise InstanceError "instance'.instanceType.lookup")
			| TYVAR (ref (NO_TYPE_INSTANCE(EXPLICIT _))) => die "generic tyvar is explicit"
			| TYVAR (ref (TYPE_INSTANCE _)) => die "generic tyvar is instantiated"
			| ARROW (ty1,ty2) => Type.mk_Arrow (instanceType ty1, instanceType ty2)
			| RECTYPE r => Type.from_RecType (instanceRecType r)
			| CONSTYPE (tys,tyname) => Type.mk_ConsType (map instanceType tys,tyname))
		    end
	      and instanceRecType r =
		    let val r = findRecType r in
		    (case r of
		       NILrec => r
		     | VARrec rho => r
		     | ROWrec (l,ty,r') => ROWrec (l,instanceType ty,instanceRecType r'))
		    end
	      val inst = instanceType ty
	      val result = (inst,map #2 M)
	    in
	      Type.debug_print "***instance IN " ty;
	      Type.debug_print "***instance OUT" inst;
	      result
	    end

      fun instance sigma = #1 (instance' sigma)
      val instance'' = instance'

      fun generalises_Type (sigma, tau') : bool =
            (debug_print "TypeScheme.generalises_Type: sigma=" sigma ;
	     Type.debug_print "TypeScheme.generalises_Type: tau'=" tau' ;
	     let
	       val fv_sigma = tyvars sigma
	       val tau = (instance sigma)
		 handle InstanceError s => 
		   (print ("InstanceError." ^ s ^"\n");
		    print ("TypeScheme.generalises_Type.sigma = " ^ string sigma);
		    print ("tau = " ^ Type.string tau');
		    raise InstanceError (s ^ ".generalises_Type"))
	       val fv_tau' = Type.tyvars tau'
	     in
	       (* One must use restricted unify! Consider
		* ``generalises_Type(int->int, 'a->int)''.  This
		* should fail!! -- Martin *)

	       case Type.restricted_unify (TyVar.unionTyVarSet(fv_tau',fv_sigma)) (tau,tau') 
		 of Type.UnifyOk => true
		  | _ => false
	     end)

      fun generalises_TypeScheme (sigma, (betas,tau)) : bool =
	generalises_Type (sigma,tau) andalso
	(TyVar.intersectTyVarSet (betas,tyvars sigma) = [])

      (* The reason for copying in `close_overload' and `close'
       * is that generalisation modifies the type. *)

      fun close_overload tau =
	    (*with generalisation of overloaded tyvars (because of the
	     first `true' argument to `Type.generalise' below).*)
	    let val tau = Type.copy true false tau
	        val _ = Type.generalise {ov=true, imp=true, tau=tau}
		val tvs = Type.generic_tyvars tau 
	    in (tvs, tau)
	    end

      fun close_and_return_escaping_tyvars imp (_, tau) =
	    (*tyvars are discarded; we could check whether the list is empty. *)
	    (*if imp = true iff the expression tau is the type of is
	     non-expansive, and then type variables must be generalised.*)
	    let val tau = Type.copy false imp tau
	              (*(the `false' means no generalisation of overloaded tyvars.)*)
	        val escaping_tyvars = Type.generalise_and_return_escaping_tyvars
		      {ov=false, imp=imp, tau=tau}
		val tvs = Type.generic_tyvars tau 
	    in ((tvs, tau), escaping_tyvars)
	    end

      fun close imp sigma = #1 (close_and_return_escaping_tyvars imp sigma)

      (*violates_equality T sigma = false, iff, assuming the tynames in T
       admit equality, sigma admits equality, i.e., violates_equality T sigma
       = non((all t in T admit equality) => sigma admits equality).
       violates_equality is used when maximising equality in a TE.  T will be
       those datatypes in TE we tentatively assume to admit equality, and sigma
       will be the type scheme of a constructor.*)

      local
	fun violates_equality0 T tau =
	      (case #TypeDesc (findType tau) of
		 TYVAR (ref (NO_TYPE_INSTANCE (ORDINARY _))) => false
	       | TYVAR (ref (NO_TYPE_INSTANCE (EXPLICIT _))) =>  
		   die "violates_equality: EXPLICIT" 
	       | TYVAR (ref (TYPE_INSTANCE _)) => 
		   die "violates_equality: TYPE_INSTANCE"
	       | RECTYPE r =>
		   Type.RecType.fold
		     (fn (tau, res) => res orelse violates_equality0 T tau)
		       false r
	       | CONSTYPE (taus, tyname) =>
		   if TyName.equality tyname
		   orelse TyName.Set.member tyname T
		   then
		     List.foldL
		       (fn tau => fn res => res orelse violates_equality0 T tau)
			  false taus
		   else true
	       | ARROW _ => true)
      in
	fun violates_equality (T : TyName.Set.Set) (sigma : TypeScheme) : bool =
	      let val (_, tau) = to_TyVars_and_Type sigma
	      in
		(case Type.un_Arrow tau of
		   NONE => false                                 (*nullary constructor*)
		 | SOME (tau', _) => violates_equality0 T tau')  (*unary constructor*)
		              (* before an instance was created for tau' 30/10/97-Martin *)
       	      end
      end (*local*)

      fun match((_,tau1),(_,tau2)) : unit = Type.match(tau1,tau2)
    
    end (*TypeScheme*)


    (* Type functions are implemented almost as type schemes. However,
     * we keep track of bound variables of a type function
     * explicitly. This is necessary to check if two type functions
     * are equal (bound variables may not be reordered or dropped as
     * for type schemes) and to obtain the arity of a type function.
     *
     * -- Martin 
     *)

    datatype TypeFcn =  TYPEFCN of {tyvars : TyVar list, tau : Type}

    structure TypeFcn = struct

      fun layout (TYPEFCN {tyvars, tau}) = 
	    let val tau = findType tau
	    in
	      PP.NODE {start=List.stringSep "/\\(" "). " ", " TyVar.string tyvars,
		       finish="", indent=0, childsep=PP.NOSEP,
		       children=[Type.layout_with_level tau]}
	    end

      fun eq (TYPEFCN {tyvars, tau}, TYPEFCN {tyvars=tyvars', tau=tau'}) = 
	(Type.eq_equality_not_significant (tau, tau') andalso
	 List.forAll (TyVar.eq_bound EQ_NOT_SIGNIFICANT) (ListPair.zip (tyvars, tyvars')))
	handle ListPair.Zip => false
	
      val dummy_tv = TyVar.fresh_bound {id=0, equality=false, overloaded=Nonoverloaded}

      local fun eq_tyvar(tv,tv') = TyVar.eq_bound EQ_NOT_SIGNIFICANT (tv,tv') orelse
	                           TyVar.eq_free (tv,tv')
  	    fun insert_dummies(tyvars, tyvars') =    (* returns tyvars with those tyvars not in *)
	      let fun maybe_replace_with_dummy tv =  (* tyvars' replaced with dummy-tyvars. *)
		    let fun look [] = dummy_tv
			  | look (tv'::tvs) = if eq_tyvar (tv,tv') then tv else look tvs
		    in look tyvars'
		    end
	      in map maybe_replace_with_dummy tyvars
	      end
	    fun subst m tv = 
	      let fun look (tv'::tvs,tv''::tvs') = if eq_tyvar (tv,tv') then tv''
						   else look(tvs,tvs')
		    | look ([],[]) = tv
		    | look _ = die "from_TyVars_and_Type.look"
	      in look m
	      end
      in 
	fun from_TyVars_and_Type (tyvars : TyVar list, tau : Type) =
	  let val tyvars_tau = Type.tyvars tau
	      val tyvars' = insert_dummies(tyvars, tyvars_tau)   (* make those tyvars in tyvars that *)
	                                                         (* do not occur in tau into dummy_tv. *)
	      val tau' = Type.copy false true tau
	      val tyvars_tau' = Type.tyvars tau'
	      val tyvars'' = map (subst (tyvars_tau,tyvars_tau')) tyvars'
	  in Type.generalise {ov=false, imp=true, tau=tau'} ;
	     TYPEFCN {tyvars=tyvars'', tau=tau'}
	  end
      end
    
      fun apply (theta as (TYPEFCN {tyvars, tau}), taus : Type list) : Type =
	    let
	      val (tau', fresh_taus) = (TypeScheme.instance'' (tyvars,tau))
		handle TypeScheme.InstanceError s =>
		  (print ("InstanceError." ^ s ^ "\n");
		   print ("Applying type function: ");
		   (pr_st o layout) theta;
		   raise TypeScheme.InstanceError (s ^ "[TypeFcn.apply]"))
	      val _ = debug_print "TypeFcn.apply"

(*30/10/97-Martin
	      fun f (tv as ref (NO_TYPE_INSTANCE tvdesc)) = 
		       (case FinMapEq.lookup TyVarDesc_eq fresh_taus_map tvdesc of
			  NONE => Type.from_TyVar (TyVar.refresh tv)
			| SOME tau => tau)
		| f _ = die "TypeFcn.apply: f"
	      val fresh_taus = map f tyvars
*)
	    in
	      map Type.unify (ListPair.zip (fresh_taus, taus)) ;
	      tau'
	    end handle ListPair.Zip => die "TypeFcn.apply: zip"

      fun arity (TYPEFCN {tyvars, tau}) =  List.size tyvars

      (*admits_equality: We need only check if the type in an
       type function admits equality because the bound type
       variables have already been renamed to admit equality:*)
      fun admits_equality (theta as TYPEFCN {tyvars, tau}) : bool = 
	    (case Type.make_equality (TypeScheme.instance (tyvars,tau)) of
	       SOME _ => true
	     | NONE => false)
	       handle TypeScheme.InstanceError s =>
		 (print ("InstanceError." ^ s ^ "\n");
		  print ("admits_equality.type function: ");
		  (pr_st o layout)theta;
		  raise TypeScheme.InstanceError (s ^ "[TypeFcn.admits_equality]"))

      fun tynames (TYPEFCN {tyvars, tau}) = Type.tynames tau

      fun grounded (theta : TypeFcn, tynameset : TyName.Set.Set) : bool =
	    TyName.Set.isEmpty (TyName.Set.difference (tynames theta) tynameset)

      fun from_TyName (tyname : TyName) : TypeFcn =
	    let fun make_list 0 = []
		  | make_list x = let val tyvar = TyVar.fresh_normal ()
				  in tyvar :: make_list (x-1)
				  end
		val tyvars = make_list (TyName.arity tyname)
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
			of SOME tv' => TyVar.eq_bound EQ_NOT_SIGNIFICANT (tv,tv')
			 | NONE => false) andalso check(tvs,taus)
		  | check _ = false
	    in if check(tyvars,taus) then SOME t
	       else NONE
	    end
	   | _ => NONE

      val is_TyName = is_Some o to_TyName
      local val tyvar = TyVar.fresh_bound {id= ~1, equality=false,
					   overloaded=Nonoverloaded}
      in
	val bogus = TYPEFCN {tyvars = [tyvar],
			     tau = {TypeDesc=TYVAR tyvar, level = ref Level.GENERIC}}
      end

      fun pretty_string names (TYPEFCN {tyvars, tau}) = 
	    (case tyvars of
	       [] => {vars="", body=Type.pretty_string names tau}
	     | [tyvar] => {vars=TyVar.pretty_string names tyvar,
			   body=Type.pretty_string names tau}
	     | tyvars =>
		 {vars = List.stringSep "(" ")" ", " (TyVar.pretty_string names) tyvars,
		  body=Type.pretty_string names tau})
      fun pretty_string' names theta = #body (pretty_string names theta)

      fun debug_print from theta = 
	    if !Flags.DEBUG_TYPES then 
	      print_node {start=from ^ ": ", finish="", indent=0,
			  children=[layout theta], childsep=PP.NOSEP}
	    else ()
(*debug
      val eq = fn (tf, tf') =>
	 let val tmp = !Flags.DEBUG_TYPES
	 in print "Checking equality of type functions:\n";
	    Flags.DEBUG_TYPES := true;
	    debug_print "first" tf;
	    debug_print "second" tf'; 
	    Flags.DEBUG_TYPES := tmp;
	    eq(tf,tf')
	 end
*)	 

      fun match (TYPEFCN{tau,...}, TYPEFCN{tau=tau0,...}) : unit = Type.match(tau,tau0)

    end (*TypeFcn*)

    

   (* Test stuff *)
    local
      val a = TyVar.fresh_normal ()
      val _ = Level.push ()
      val a_type = Type.from_TyVar a
      and a'_type = Type.from_TyVar a
      val _ = Level.pop ()
      val tau1 = Type.from_pair(a_type,a'_type)
      val _ = Type.generalise {ov=false, imp=true, tau=tau1}

      val tau2 = 
	let
	  val tv = ref (NO_TYPE_INSTANCE(ORDINARY{id= ~1, equality = false, rank=dummy_rank_ref,
						  overloaded = Nonoverloaded}))
	  val ty  = {TypeDesc = TYVAR tv, level = ref Level.GENERIC}
	  val ty' = {TypeDesc = TYVAR tv, level = ref Level.GENERIC}
	  val r =   {TypeDesc = 
		     RECTYPE (Type.RecType.add_field (ONE, ty)
			        (Type.RecType.add_field (TWO, ty')
				   Type.RecType.empty)),
		     level = ref Level.GENERIC}
	in
	  r
	end
      val _ = Type.debug_print "GENERALISE Test -- tau1" tau1
      val _ = Type.debug_print "GENERALISE Test -- tau2" tau2 
      (* val _ = Flags.DEBUG_TYPES := false *)

      val _ = if false then 
	if Type.eq(tau1,tau2) then
	  TextIO.output(TextIO.stdOut,"***GENERALISE test \t\tsucceeded***\n")
	else
 	  TextIO.output(TextIO.stdOut,"***GENERALISE test did \t\t***not*** succeed***\n")
	      else ()

      val tau3 = TypeScheme.instance ([a],tau1)
      val ty = Type.Int 
      and ty' = Type.fresh_normal ()
      val tau4 = 
	  Type.from_RecType (Type.RecType.add_field (ONE, ty)
			       (Type.RecType.add_field (TWO, ty')
				  Type.RecType.empty))
      val _ = Type.unify (tau3,tau4)
    in
      val _ = if false then 
	if Type.eq(ty',Type.Int) then
	  TextIO.output(TextIO.stdOut,"***INSTANCE UNIFY test \t\tsucceeded***\n")
	else
	  TextIO.output(TextIO.stdOut,"***INSTANCE UNIFY test did \t\t***not*** succeed***\n")
	      else ()
    end


   (* Test stuff *)
    local
      val a = TyVar.from_ExplicitTyVar(ExplicitTyVar.mk_TyVar "'a") 
      and a' = TyVar.from_ExplicitTyVar(ExplicitTyVar.mk_TyVar "'a")
      val _ = if false then
	if TyVar.eq_free(a,a') then
	  TextIO.output(TextIO.stdOut,"***EXPLICIT EQUALITY test \tsucceeded***\n")
	else
	  TextIO.output(TextIO.stdOut,"***EXPLICIT EQUALITY test did \t***not*** succeed***\n")
	      else ()

      val _ = Level.push ()
      val a_type = Type.from_TyVar a
      and a'_type = Type.from_TyVar a'
      val _ = Level.pop ()
      val tau1 = Type.from_pair(a_type,a'_type)
      val _ = Type.generalise {ov=false, imp=true, tau=tau1}
      val tau2 = 
	let
	  val tv = ref (NO_TYPE_INSTANCE(ORDINARY{id= ~1, equality = false, rank=dummy_rank_ref,
						  overloaded = Nonoverloaded}))
	  val ty  = {TypeDesc = TYVAR tv, level = ref Level.GENERIC}
	  val ty' = {TypeDesc = TYVAR tv, level = ref Level.GENERIC}
	  val r =   {TypeDesc = 
		     RECTYPE (Type.RecType.add_field (ONE, ty)
			        (Type.RecType.add_field (TWO, ty')
				   Type.RecType.empty)),
		     level = ref Level.GENERIC}
	in
	  r
	end
      val _ = Type.debug_print "EXPLICIT GENERALISE Test -- tau1" tau1
      val _ = Type.debug_print "EXPLICIT GENERALISE Test -- tau2" tau2 
    in
      val _ = if false then
	if Type.eq (tau1,tau2) then
	  TextIO.output(TextIO.stdOut,"***EXPLICIT GENERALISE test \tsucceeded***\n")
	else
	  TextIO.output(TextIO.stdOut,"***EXPLICIT GENERALISE test did \t***not*** succeed***\n")
	      else ()
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

      val theta1 = TypeFcn.from_TyName t
      val theta2 = TypeFcn.from_TyVars_and_Type([a,b],tau)
      val _ = TypeFcn.debug_print "TYPEFCN Test -- theta1" theta1
      val _ = TypeFcn.debug_print "TYPEFCN Test -- theta2" theta2
    in
      val _ =
	if TypeFcn.eq(theta1,theta2) then ()
(*	  TextIO.output(TextIO.stdOut,"***TYPEFCN EQUALITY Test \t\tsucceeded***\n") *)
	else
	  TextIO.output(TextIO.stdOut,"***TYPEFCN EQUALITY Test did \t\t***not*** succeed***\n")
  
    end



    datatype TypeFcn' = TYNAME of TyName |  EXPANDED of TypeFcn
    datatype realisation = 
        Not_Id of TypeFcn' TyName.Map.map
      | Realisation_Id

    structure Realisation = struct
      (*correct_levels_Type: correct levels of non-tyvar nodes in
       a type.  Used by on_Type*)

      fun dom Realisation_Id = TyName.Set.empty
	| dom (Not_Id m) = TyName.Set.fromList(TyName.Map.dom m)

      fun correct_levels_Type ty = 
	    let val ty = findType ty 
	    in 
	      if !(#level ty) = Level.GENERIC then ()
	      else
		(case ty of
		   {TypeDesc = TYVAR tv, level} => ()
		 | {TypeDesc = ARROW (ty1,ty2), level} => 
		     level := Int.min (correct_levels_Type ty1) (correct_levels_Type ty2)
		 | {TypeDesc = RECTYPE r, level} => 
		     level := correct_levels_RecType r
		 | {TypeDesc = CONSTYPE (tys,tyname), level} => 
		     level := List.foldL 
		                Int.min Level.NONGENERIC (map correct_levels_Type tys)) ;
	      !(#level ty)
	    end

      and correct_levels_RecType r =
	    let val r = findRecType r 
	    in
	      (case r of
		 NILrec => Level.NONGENERIC
	       | VARrec (ref (NO_REC_INSTANCE _)) => Level.NONGENERIC
	       | VARrec (ref (REC_INSTANCE _)) => die "correct_levels_RecType"
	       | ROWrec (l,ty,r') => 
		   Int.min (correct_levels_Type ty) (correct_levels_RecType r'))
	    end

      val Id = Realisation_Id

      fun is_Id Realisation_Id = true   (* conservative test *)
	| is_Id _ = false

      fun singleton (t,theta) = Not_Id (TyName.Map.singleton(t,EXPANDED theta))

      fun from_T_and_theta (T, theta) =
	if TyName.Set.isEmpty T then Realisation_Id
	else Not_Id(TyName.Set.fold (fn t => fn acc => TyName.Map.add(t,EXPANDED theta,acc)) TyName.Map.empty T)

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
   	     SOME(Not_Id(TyName.Map.Fold(fn ((t, theta), acc) => 
					 case theta
					   of TYNAME t' => 
					     if TyName.Set.member t' (TyName.Set.fromList(TyName.Map.dom acc)) then raise Inverse
					     else TyName.Map.add(t', TYNAME t, acc)
					    | EXPANDED theta' => raise Inverse) TyName.Map.empty m))
	     handle Inverse => NONE 
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
	| on_TyName' (Not_Id m) t = case TyName.Map.lookup m t
				      of SOME theta => theta
				       | NONE => TYNAME t
 
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
			   handle TypeScheme.InstanceError s => 
			     (print ("InstanceError." ^ s ^"\n");
			      print ("on_Type.CONSTYPE t = " ^ TyName.pr_TyName t ^ "\n");
			      raise TypeScheme.InstanceError (s ^ "[on_Type]"))

			 in 
			   correct_levels_Type tau ;
			   tau
			 end
	       in
		 TypeFcn_apply' (theta, map (on_Type phi) tylist)
	       end)

      fun on_TypeScheme Realisation_Id scheme = scheme
	| on_TypeScheme phi (tyvars,tau) =
	let val _ = Level.push()
	    val (tau, taus) = TypeScheme.instance'' (tyvars,tau)
	    val tau = on_Type phi tau
	    val _ = Level.pop()
	    val tyvars' = map (fn tau =>
			       case Type.to_TyVar tau
				 of SOME tv => tv
				  | NONE => die "on_TypeFcn.1") taus
	    val TYPEFCN{tyvars,tau} = TypeFcn.from_TyVars_and_Type(tyvars',tau)
	in (tyvars, tau)
	end

      fun on_TypeFcn Realisation_Id typefcn = typefcn
	| on_TypeFcn phi (TYPEFCN {tyvars, tau}) =
	let val (tyvars,tau) = on_TypeScheme phi (tyvars,tau)
	in TYPEFCN{tyvars=tyvars,tau=tau}
	end

      fun on_TypeFcn' Realisation_Id typefcn' = typefcn'
	| on_TypeFcn' phi (TYNAME t) = on_TyName' phi t
	| on_TypeFcn' phi (EXPANDED theta) = EXPANDED (on_TypeFcn phi theta)

      fun on_Realisation Realisation_Id phi = phi
	| on_Realisation phi Realisation_Id = Realisation_Id
	| on_Realisation phi (Not_Id m) =
	Not_Id(TyName.Map.Fold (fn ((t,theta), acc) =>
				TyName.Map.add(t,on_TypeFcn' phi theta,acc)) TyName.Map.empty m)

      fun (Realisation_Id : realisation) oo (phi : realisation) : realisation = phi
	| phi oo Realisation_Id = phi
	| (phi1 as Not_Id m1) oo (phi2) = (case on_Realisation phi1 phi2
					     of Realisation_Id => phi1
					      | Not_Id m2 => Not_Id(TyName.Map.plus(m1, m2)))

      fun enrich (rea0, (rea,T)) =
	TyName.Set.fold (fn t => fn acc => acc andalso 
			 TypeFcn.eq(on_TyName rea0 t, on_TyName rea t)) true T

      fun eq (Realisation_Id, Realisation_Id) = true
	| eq (rea1,rea2) = 
	let val T = dom rea1
	in TyName.Set.eq T (dom rea2) andalso enrich (rea1,(rea2,T))
	end

      fun match (Realisation_Id, rea0) = ()
	| match (Not_Id m, rea0) =
	let fun convert (EXPANDED theta) = theta
	      | convert (TYNAME t) = TypeFcn.from_TyName t
	in TyName.Map.Fold(fn ((t, theta),_) => TypeFcn.match(convert theta,on_TyName rea0 t)) () m
	end

      fun layout_TypeFcn' (TYNAME t) = TyName.layout t
	| layout_TypeFcn' (EXPANDED theta) = TypeFcn.layout theta
      fun layout Realisation_Id = PP.LEAF "Id"
	| layout (Not_Id m) = TyName.Map.layoutMap {start="{",eq=" -> ", finish="}",sep=", "}
	TyName.layout layout_TypeFcn' m

    end (*Realisation*)


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
(*
      val tmp = !Flags.DEBUG_TYPES
      val _ = Flags.DEBUG_TYPES := true
      val _ = TypeFcn.debug_print "TYREA TYPEFCN Test -- theta1" theta1
      val _ = TypeFcn.debug_print "TYREA TYPEFCN Test -- theta2" theta2
      val _ = TypeFcn.debug_print "TYREA TYPEFCN Test -- theta1'" theta1'
      val _ = Flags.DEBUG_TYPES := tmp
*)
      val _ =
	if TypeFcn.eq(theta1,theta1') then ()
(*	  TextIO.output(TextIO.stdOut,"***TYREA TYPEFCN EQAULITY Test \t\tsucceeded\n") *)
	else
	  TextIO.output(TextIO.stdOut,"***TYREA TYPEFCN EQUALITY Test did \t\t***not*** succeed\n")

    in
      val _ = ()
    end
  end (*StatObject*)


