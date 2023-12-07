
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

    type regvar = RegVar.regvar
    type regvar_info = ParseInfo.ParseInfo * regvar

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
    type 'a uref = 'a URef.uref

    datatype TypeDesc =
        TYVAR of TyVar
      | ARROW of Type * regvar_info option uref * Type * regvar_info option uref
      | RECTYPE of RecType * regvar_info option uref
      | CONSTYPE of Type list * TyName.TyName * (ParseInfo.ParseInfo * regvar_info list) option uref

    and TyLink =
        NO_TY_LINK of TyVarDesc        (* type variable *)
      | TY_LINK of Type                (* type variable linked to type *)

    and RecType =
        NILrec
      | VARrec of {RowVar : RowVar, level : level ref}    (* a rowvar has a level *)
      | ROWrec of Lab.lab * Type * RecType   (* labels are ordered during unification *)

    and RecLink =
        NO_REC_LINK of {rho:int}         (* row variable; the stamp is here to ease printing *)
      | REC_LINK of {rt:RecType}         (* row variable linked to row *)

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
        of VARrec {RowVar = rl as ref (REC_LINK {rt=r'}), ...} =>
          let val r'' = findRecType r' in rl := REC_LINK {rt=r''}; r'' end
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
                     | NONAMES          (* NONAMES -> don't bother. *)
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
                  | ARROW(t1,rvi0,t2,rvi) => ARROW(norm_Type t1,rvi0,norm_Type t2,rvi)
                  | RECTYPE (rt,rvi) => RECTYPE(norm_RecType rt,rvi)
                  | CONSTYPE(ts,tn,rvi) => CONSTYPE(map norm_Type ts,tn,rvi)
        in {TypeDesc=TypeDesc,level=level}
        end

    and norm_RecType rt =
        case findRecType rt of
            NILrec => NILrec
          | VARrec _ => die "norm_RecType.uninstantiated rowvar"
          | ROWrec (l,t,rt) => ROWrec(l,norm_Type t, norm_RecType rt)

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
          "(" ^ String.concatWith "," (map string l) ^ ")"

    end (*TyVar*)

    type Substitution = unit
    infix oo
    structure Substitution = struct
      val Id = ()
      val bogus = Id
      fun (S1 : Substitution) oo (S2 : Substitution) : Substitution = ()
      fun on (S : Substitution, tau : Type) : Type = findType tau
      fun onScheme (S : Substitution, (tvs,regvars,tau)) = (tvs, regvars,findType tau)
    end (*Substitution*)

    structure Type =
      struct

        local val r = ref 0
        in fun freshRow () = VARrec {RowVar = ref (NO_REC_LINK {rho=(r := !r + 1 ; !r)}), level = ref (Level.current())}
        end

        fun pr_rvis (rvis : (ParseInfo.ParseInfo * regvar_info list) option URef.uref) : string =
            case URef.!! rvis of
                NONE => ""
              | SOME (_,[(_,rv)]) => "`" ^ RegVar.pr rv
              | SOME (_,rvs) => "`[" ^ String.concatWith "," (map (RegVar.pr o #2) rvs) ^ "]"

        fun layout ty =
          let val ty = findType ty
              val st =
                case #TypeDesc ty
                  of CONSTYPE ([], tyname, rvis) => PP.LEAF (TyName.pr_TyName tyname ^ pr_rvis rvis)
                   | CONSTYPE (ty_list, tyname, rvis) =>
                    PP.NODE {start="(", finish=") " ^ TyName.pr_TyName tyname ^ pr_rvis rvis, indent=1,
                             children = map layout ty_list,
                             childsep = PP.LEFT ", "}
                   | RECTYPE (r,rvopt_u) => RecType_layout r (Option.map #2 (URef.!! rvopt_u))
                   | ARROW (ty,rvopt0_u,ty',rvopt_u) =>
                     let val finish = case URef.!! rvopt_u of
                                          NONE => ")"
                                        | SOME (_,rv) => ")`" ^ RegVar.pr rv
                         val arrow = case URef.!! rvopt0_u of
                                          NONE => " -> "
                                        | SOME (_,rv) => " -" ^ RegVar.pr rv ^ "-> "
                     in PP.NODE {start="(", finish=finish, indent=1,
                                 children=[layout ty, layout ty'],
                                 childsep=PP.LEFT arrow}
                     end
                   | TYVAR tv => TyVar.layout tv
          in
            if !print_type_levels then PP.NODE{start="[", finish="]",childsep=PP.RIGHT ", ", indent=1,
                                               children=[st, PP.LEAF(Int.toString (!(#level ty)))]}
            else st
          end

        and RecType_layout r rvi =
          let val (m, rv_opt) = sanitiseRecType r
              val finish = case rv_opt
                             of SOME {RowVar=rv,...} => " ... " ^ pr_RowVar rv ^ "}"
                              | NONE => "}"
              val finish = case rvi of
                               NONE => finish
                             | SOME rv => finish ^ "`" ^ RegVar.pr rv
          in Lab.Map.layoutMap {start="{", eq=" : ", sep=", ", finish=finish}
            (PP.layoutAtom Lab.pr_Lab) layout m
          end

        and pr_RowVar (ref (NO_REC_LINK {rho})) = "'r" ^ Int.toString rho
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
                   | (ARROW (ty1,rvi,ty1',rvi1), ARROW (ty2,rvi',ty2',rvi2)) =>
                   Type_eq0 eq_significant (ty1,ty2) andalso Type_eq0 eq_significant (ty1',ty2') (*andalso RegVarInfo_eq0(rvi1,rvi2)*)
                   | (RECTYPE (r1,rvi1), RECTYPE (r2,rvi2)) => RecType_eq0 eq_significant (r1,r2) (*andalso RegVarInfo_eq0(rvi1,rvi2)*)
                   | (CONSTYPE (tys1,tyname1,rvis1), CONSTYPE (tys2,tyname2,rvis2)) =>
                   TyName.eq (tyname1, tyname2) andalso
                   TypeList_eq0 eq_significant (tys1,tys2)
                   | _ => false)
            end
(*
          and RegVarInfo_eq0 (rvi1,rvi2) =
              case (rvi1,rvi2) of
                  (NONE,NONE) => true
                | (SOME (_,rv1), SOME (_,rv2)) => RegVar.eq (rv1,rv2)
                | _ => false
*)
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

        fun parenthesize rvi (operator_precedence : int, context_precedence, s : string) =
            let val (has_regvar, finish) = case rvi of NONE => (false, ")")
                                                     | SOME (_,rv) => (true, ")`" ^ RegVar.pr rv)
            in if has_regvar orelse operator_precedence < context_precedence
               then concat ["(", s, finish]
               else s
            end

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
                SOME {TypeDesc = RECTYPE (r',_), ...} =>
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

        fun full_works (tyname,rvis) =
            let val (i,b) = TyName.id tyname
            in concat [TyName.pr_TyName tyname, "<",
                       Int.toString i , "-", b, ">"]
            end

        (* The (config:{repl:bool}) argument specifies whether the
           call is used for type-indexed pretty-printing of values in
           the REPL. In that case, pretty printing should not include
           regvars and type names should be printed with their
           internal stamps.
         *)

        type config = {repl:bool}

        fun TyName_string_as_opt (config:config) ((tn,rvis), tn'_opt) =
            if #repl config then TyName.pr_TyName_repl tn
            else let val str =
                         case tn'_opt of
                             SOME tn' =>
                             if TyName.eq (tn,tn') then TyName.pr_TyName tn
                             else if TyName.tycon tn = TyName.tycon tn'
                             then full_works (tn,rvis)
                             else TyName.pr_TyName tn
                           | NONE => TyName.pr_TyName tn
                 in str ^ pr_rvis rvis
                 end

        fun pretty_string_as_opt (config:config) names precedence (ty : Type, ty'_opt : Type option) =
          let val ty = findType ty
              val ty'_opt = Option.map findType ty'_opt
              val st_ty =
                  case #TypeDesc ty of
                      TYVAR tv => TyVar.pretty_string names tv

                    | RECTYPE (r,rvi) =>         (* See if we can print it as `a * b * ...'
                                                  * rather than `{1: a, 2: b, ...}' *)
                      let val r = findRecType r
                          val (m, rv) = sanitiseRecType r
                      in case (is_tuple_type m, rv) of
                             (true, NONE) => (* A possible (t1 * t2 * ...) type, and
                                              * no rowvar. *)
                             print_tuple config names precedence (m, ty'_opt)
                           | _ => (*Have to do the general print.*)
                           let val fields = Lab.Map.range m
                               val fields' = fields_of_other_record (ty'_opt, fields)
                               val field_types = map (pretty_string_as_opt config names 1)
                                                     (ListPair.zip (fields,fields'))
                               val labels = map Lab.pr_Lab (Lab.Map.dom m)
                               fun colon_between (lab, ty) = lab ^ ": " ^ ty
                               val finish = case URef.!! rvi of
                                                NONE => "}"
                                              | SOME (_,rv) =>
                                                if #repl config then "}"
                                                else "}`" ^ RegVar.pr rv
                           in ListUtils.stringSep "{" finish ", " colon_between
                                                  (ListPair.zip (labels,field_types))
                           end
                      end
                    | ARROW (t1,rvi0,t2,rvi) =>
                      let val arrow = case URef.!! rvi0 of
                                          NONE => " -> "
                                        | SOME (_,rv) =>
                                          if #repl config then " -> "
                                          else " -" ^ RegVar.pr rv ^ "-> "
                          fun default () =
                              pretty_string_as_opt config names 3 (t1, NONE) ^ arrow
                              ^ pretty_string_as_opt config names 2 (t2, NONE)
                          val inner =
                              case ty'_opt of
                                  SOME {TypeDesc = ARROW(t1', rvi0', t2',rvi'), ...} =>
                                  (case URef.!! rvi' of
                                       NONE =>
                                       (case URef.!! rvi0' of
                                            NONE => pretty_string_as_opt config names 3 (t1, SOME t1') ^ arrow
                                                    ^ pretty_string_as_opt config names 2 (t2, SOME t2')
                                          | SOME _ => default())
                                     | SOME _ => default())
                                | _ => default()
                      in case URef.!! rvi of
                             NONE => parenthesize NONE (2, precedence, inner)
                           | SOME (_,rv) =>
                             if #repl config then parenthesize NONE (2, precedence, inner)
                             else "(" ^ inner ^ ")`" ^ RegVar.pr rv
                      end
                    | CONSTYPE (tys, tyname, rvis) =>
                      let val (tys'_opt, tyname'_opt) =
                              case ty'_opt of
                                  SOME {TypeDesc = CONSTYPE(tys', tyname', _), ...} =>
                                  (ziptypes tys' tys, SOME tyname')
                                | _ => (map (fn _ => NONE) tys, NONE)
                      in
                        case (tys, tys'_opt) of
                            (nil,_) => TyName_string_as_opt config ((tyname,rvis), tyname'_opt)
                          | ([ty], [ty']) =>
                            concat [pretty_string_as_opt config names 4 (ty,ty'), " ",
                                    TyName_string_as_opt config ((tyname,rvis), tyname'_opt)]
                          | _ =>
                            concat [ListUtils.stringSep "(" ")" ", "
                                                        (pretty_string_as_opt config names 1)
                                                        (ListPair.zip (tys,tys'_opt)),
                                    " ",
                                    TyName_string_as_opt config ((tyname,rvis), tyname'_opt)]
                      end
          in
            if !print_type_levels andalso not(#repl config)
            then concat ["[", st_ty, ",", Int.toString (!(#level ty)), "]"]
            else st_ty
          end

        and print_tuple config names precedence (m, ty'_opt: Type option) =

          (* Careful: "{1=x}" does not print as "(x)", and "{ }"
           * should be "unit". We don't do this folding at all if
           * there's a row var. *)

              let
                val fields = Lab.Map.range m
                val (fields',rvi) =
                    case ty'_opt of
                        SOME {TypeDesc = RECTYPE (r',rvi), ...} =>
                        let val r' = findRecType r'
                            val (m', rv') = sanitiseRecType r'
                        in (case (is_tuple_type m', rv') of
                                (true, NONE) =>
                                (* A possible (t1' * t2' *  ) type, and no rowvar: *)
                                (ziptypes (Lab.Map.range m') fields,rvi)
                              | _ => (map (fn field => NONE) fields,rvi))
                        end
                      | _ => (map (fn field => NONE) fields,URef.uref NONE)
              in
                case (fields, fields') of
                    (nil, _) => "unit"   (* Hard-wired *)
                  | ([x], [x']) =>
                    let val s = "{1: " ^ pretty_string_as_opt config names 1 (x,x') ^ "}"
                    in case URef.!!rvi of
                           NONE => s
                         | SOME (_,rv) =>
                           if #repl config then s
                           else s ^ "`" ^ RegVar.pr rv
                    end
                  | _ => parenthesize (if #repl config then NONE else (URef.!! rvi))
                                      (3, precedence,
                                       ListUtils.stringSep "" "" " * "
                                                           (pretty_string_as_opt config names 4)
                                                           (ListPair.zip(fields, fields')))
              end
      in
        fun pretty_string_as_ty names (ty,ty') : string =
            pretty_string_as_opt {repl=false} names 1 (ty, SOME ty')
        fun pretty_string names ty : string =
            pretty_string_as_opt {repl=false} names 1 (ty, NONE)
        fun string_repl ty =
            pretty_string_as_opt {repl=true} NONAMES 1 (ty, NONE)
      end (*local*)

      val string_as_ty = pretty_string_as_ty NONAMES
      val string = pretty_string NONAMES

      fun from_TyVar tyvar = {TypeDesc = TYVAR tyvar, level = ref (Level.current ())}

      fun to_TyVar ty = case #TypeDesc (findType ty)
                          of TYVAR tyvar => SOME tyvar
                           | _ => NONE

      val fresh0 = from_TyVar o TyVar.fresh0
      val fresh_normal = from_TyVar o TyVar.fresh_normal
      fun from_RecType (r,rvi:regvar_info option) = {TypeDesc = RECTYPE (r,URef.uref rvi), level = ref Level.NONGENERIC}

      fun to_RecType ty =
          case #TypeDesc (findType ty) of
              RECTYPE (t,rvi) => SOME (t,URef.!! rvi)
            | _ => NONE

      (* contains_row_variable tau; true iff there exists a row
       * variable in tau. *)

      fun contains_row_variable t =
        case #TypeDesc (findType t)
          of TYVAR _ => false
           | ARROW (t1, _, t2, _) => contains_row_variable t1 orelse contains_row_variable t2
           | RECTYPE (r,_) => RecType_contains_row_variable r
           | CONSTYPE (tylist, _, _) => List.exists contains_row_variable tylist

      and RecType_contains_row_variable r =
        case findRecType r
          of NILrec => false
           | VARrec _ => true
           | ROWrec (_, ty, r') => contains_row_variable ty orelse RecType_contains_row_variable r'


      structure RecType = struct
        val empty = NILrec              (* "{}" *)
        val dotdotdot = freshRow        (* "{...}" *)

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
                | RECTYPE (r,_) => RecType.fold (fn (ty, ()) => tyvars0 f_b ty) () (findRecType r)
                | ARROW (ty,_,ty',_) =>     (* For the compilation of value constructors, we
                                             * extract the tyvars of ty' before those of ty.
                                             * Martin-15/11/1998 *)
                    (tyvars0 f_b ty'; tyvars0 f_b ty)

                | CONSTYPE (types,_,_) => foldr (fn (ty,()) => tyvars0 f_b ty) () types
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
                  | RECTYPE (r,_) => RecType.fold
                   (fn (ty, T) => TyName.Set.union (tynames ty) T)
                   TyName.Set.empty  r
                 | ARROW (ty, _, ty', _) => TyName.Set.union (tynames ty) (tynames ty')
                 | CONSTYPE (types, tyname, _) => foldl
                   (fn (ty, T) => TyName.Set.union (tynames ty) T)
                   (TyName.Set.singleton tyname) types
            end

      fun from_pair (ty,ty') =
            {TypeDesc = RECTYPE (RecType.add_field (ONE, ty)
                                 (RecType.add_field (TWO, ty') RecType.empty), URef.uref NONE),
             level = ref Level.NONGENERIC}
      fun from_triple (tau1, tau2, tau3) =
            {TypeDesc = RECTYPE (RecType.add_field (ONE, tau1)
                                 (RecType.add_field (TWO, tau2)
                                  (RecType.add_field (THREE, tau3) RecType.empty)), URef.uref NONE),
             level = ref Level.NONGENERIC}
      val Unit = {TypeDesc = RECTYPE (RecType.empty,URef.uref NONE), level = ref Level.NONGENERIC}


      (* Function types *)

      fun from_FunType fty = fty
      fun to_FunType ty =
          case #TypeDesc (findType ty) of
              ARROW _ => SOME ty
            | _ => NONE

      fun mk_FunType (ty,rvi0,ty',rvi) =
          {TypeDesc = ARROW (ty,URef.uref rvi0,ty',URef.uref rvi), level = ref Level.NONGENERIC}

      fun un_FunType ty =
          case #TypeDesc (findType ty) of
              ARROW (ty,rvi0,ty',rvi) => SOME (ty,URef.!! rvi0,ty',URef.!! rvi)
            | _ => NONE

      (* Constructed types *)

      fun from_ConsType cty = cty
      fun to_ConsType ty =
          case #TypeDesc (findType ty) of
              CONSTYPE _ => SOME ty
            | _ => NONE

      fun mk_ConsType (typel, name, rvis) =
          {TypeDesc = CONSTYPE (typel,name, URef.uref rvis), level = ref Level.NONGENERIC}

      fun un_ConsType ty =
          case #TypeDesc (findType ty) of
              CONSTYPE (typel,name,rvis) => SOME (typel,name,URef.!! rvis)
            | _ => NONE

      val Exn = mk_ConsType ([], TyName.tyName_EXN, NONE)

      fun is_Exn ty =
          case #TypeDesc (findType ty) of
              CONSTYPE ([], name, _) => TyName.eq (name, TyName.tyName_EXN)
            | _ => false

      fun mk_Arrow (ty,rvi0,ty',rvi) =
          {TypeDesc = ARROW (ty,URef.uref rvi0,ty',URef.uref rvi), level = ref Level.NONGENERIC}

      fun un_Arrow ty =
          case #TypeDesc (findType ty) of
              ARROW (t, rvi0, t', rvi) => SOME (t, URef.!! rvi0, t', URef.!! rvi)
            | _ => NONE

      fun is_Arrow ty =
          case #TypeDesc (findType ty) of
              ARROW _ => true
            | _ => false

      fun mk_Ref t = mk_ConsType ([t], TyName.tyName_REF, NONE)


      (* Special constants *)

      val tag_values = Flags.is_on0 "tag_values"
      val values_64bit = Flags.is_on0 "values_64bit"

      fun mk_ConsType0 tn = mk_ConsType ([], tn, NONE)


      val Int31        = mk_ConsType0 TyName.tyName_INT31
      val Int32        = mk_ConsType0 TyName.tyName_INT32
      val Int63        = mk_ConsType0 TyName.tyName_INT63
      val Int64        = mk_ConsType0 TyName.tyName_INT64
      val IntInf       = mk_ConsType0 TyName.tyName_INTINF
      fun IntDefault () =
        case (tag_values(), values_64bit()) of
            (true,  true)  => Int63
          | (true,  false) => Int31
          | (false, true)  => Int64
          | (false, false) => Int32

      val Real         = mk_ConsType0 TyName.tyName_REAL
      val String       = mk_ConsType0 TyName.tyName_STRING
      val Bool         = mk_ConsType0 TyName.tyName_BOOL
      val Char         = mk_ConsType0 TyName.tyName_CHAR

      val Word8        = mk_ConsType0 TyName.tyName_WORD8
      val Word31       = mk_ConsType0 TyName.tyName_WORD31
      val Word32       = mk_ConsType0 TyName.tyName_WORD32
      val Word63       = mk_ConsType0 TyName.tyName_WORD63
      val Word64       = mk_ConsType0 TyName.tyName_WORD64
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
                 | ARROW (tau1, _, tau2, _) =>
                     level := Int.min (generalise {ov=ov, imp=imp, tau=tau1},
                                       generalise {ov=ov, imp=imp, tau=tau2})
                 | RECTYPE (r,_) =>
                     level := generaliseRecType {ov=ov, imp=imp, r=r}
                 | CONSTYPE (taus, tyname, _) =>
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
             | RECTYPE (r,_) => RecType.apply make_equality0 r
             | CONSTYPE (ty_list, tyname, _) =>
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

      fun restricted_unify {unify_regvars : bool,
                            restricted_tyvars : TyVar list}
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
                 | {TypeDesc = ARROW(ty1, _, ty2, _), ...} => occursType ty1 orelse occursType ty2
                 | {TypeDesc = CONSTYPE(tys, tn, _), ...} => foldl (fn (ty, b) => b orelse occursType ty) false tys
                 | {TypeDesc = RECTYPE (r,_), ...} => RecType.fold (fn (ty, b) => b orelse occursType ty) false r
          in occursType ty
          end

        fun occurs_rv_in_RecType (rv, r) =
          case findRecType r
            of NILrec => false
             | VARrec {RowVar=rv',...} => (rv = rv')
             | ROWrec(_, ty, r') => occurs_rv_in_Type(rv, ty) orelse occurs_rv_in_RecType(rv, r')

        and occurs_rv_in_Type (rv, ty) =
          case #TypeDesc (findType ty)
            of TYVAR _ => false
             | ARROW(ty1, _, ty2, _) => occurs_rv_in_Type(rv, ty1) orelse occurs_rv_in_Type(rv, ty2)
             | RECTYPE (r,_) => occurs_rv_in_RecType(rv, r)
             | CONSTYPE(tys, _, _) => (foldl (fn (ty, result) => occurs_rv_in_Type(rv, ty) orelse result)
                                             false tys)

        (* Decrease the levels of type variables and row variables in a type *)

        fun decr_level_Type (lev : level) (ty : Type) : unit =
          let val ty = findType ty
          in case ty
               of {TypeDesc = TYVAR _, level} => if !level > lev then level := lev else ()
                | {TypeDesc = ARROW(ty1, _, ty2, _), ...} => (decr_level_Type lev ty1; decr_level_Type lev ty2)
                | {TypeDesc = CONSTYPE(tys, tn, _), ...} => app (decr_level_Type lev) tys
                | {TypeDesc = RECTYPE (r,_), ...} => decr_level_RecType lev r
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
                | ARROW(ty1, _, ty2, _) => (decr_rank_Type p ty1; decr_rank_Type p ty2)
                | CONSTYPE(tys, tn, _) => (if Rank.<=(Rank.from_TyName tn, rnk) then app (decr_rank_Type p) tys
                                           else raise Rank(tv,tn))
                | RECTYPE (r,_) => decr_rank_RecType p r
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
             | CONSTYPE (taus, tyname, _) =>
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

        fun unifyRegVar (rvi,rvi') =
            if unify_regvars then
              URef.unify (fn (NONE,rvopt) => rvopt
                           | (rvopt,NONE) => rvopt
                           | (SOME rv, SOME rv') =>
                             if RegVar.eq(#2 rv,#2 rv') then SOME rv
                             else raise Unify "diffent explicit region variables") (rvi,rvi')
            else ()

        fun unifyRegVars (rvis,rvis') =
            if unify_regvars then
              URef.unify (fn (NONE,rvsopt) => rvsopt
                           | (rvsopt,NONE) => rvsopt
                           | (SOME (i,rvs), SOME (_,rvs')) =>
                             if ListPair.allEq (fn (rv,rv') => RegVar.eq(#2 rv,#2 rv')) (rvs,rvs')
                             then SOME (i,rvs)
                             else raise Unify "diffent lists of explicit region variables")
                         (rvis,rvis')
            else ()

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
               | (RECTYPE (r,rvi), RECTYPE (r',rvi')) =>
                 ( unifyRecType(r, r')
                 ; unifyRegVar(rvi,rvi')
                 )
               | (ARROW (t1,rvi0,t2,rvi), ARROW (t1',rvi0',t2',rvi')) =>
                 ( unifyFunType((t1,t2),(t1',t2'))
                 ; unifyRegVar(rvi,rvi')
                 ; unifyRegVar(rvi0,rvi0')
                 )
               | (CONSTYPE (ts,tn,rvis), CONSTYPE (ts',tn',rvis')) =>
                 ( unifyConsType((ts,tn), (ts',tn'))
                 ; unifyRegVars(rvis,rvis')
                 )
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
           else rv := REC_LINK {rt=row})

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
                  if Lab.<(lab1, lab2) then     (* Pad out row2, try again *)
                    (extract(lab1,row2); unifyRow(row1,row2))
                  else                          (* Pad out row1, try again *)
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

      fun unify {unify_regvars:bool} (ty1,ty2) =
          restricted_unify {restricted_tyvars=nil,unify_regvars=unify_regvars} (ty1,ty2)

      (* Matching functions for compilation manager *)

      local
        fun match_Type (ty,ty0) : unit =
          let val (ty, ty0) = (findType ty, findType ty0)
          in case (#TypeDesc ty,#TypeDesc ty0)
               of (ARROW (tau,_,tau',_), ARROW (tau0,_,tau0',_)) =>
                 (match_Type(tau,tau0); match_Type (tau',tau0'))
                | (RECTYPE (rect,_), RECTYPE (rect0,_)) =>
                  match_RecType (rect,rect0)
                | (CONSTYPE (taus,tn,_), CONSTYPE (taus0,tn0,_)) =>
                 (match_Types (taus, taus0) ; TyName.match (tn, tn0))
                | _ => ()
          end

        and match_Types ([],_) = ()
          | match_Types (_,[]) = ()
          | match_Types (tau::taus, tau0::taus0) =
          (match_Type (tau, tau0) ; match_Types (taus, taus0))

        and match_RecType (rect,rect0) =
          case (findRecType rect, findRecType rect0)
            of (ROWrec (l,tau,rect), ROWrec (l0,tau0,rect0)) =>
              if l <> l0 then ()
              else (match_Type (tau,tau0) ; match_RecType (rect,rect0))
             | _ => ()
      in
        val match = match_Type
      end (*local*)

      (* add_regvars may raise Fail msg *)
      fun add_regvars (irvs:ParseInfo.ParseInfo * regvar_info list) (tau:Type) : Type =
          let val {level,TypeDesc} = findType tau
              fun mk td = {level=level,TypeDesc=td}
              fun unfys (NONE,rvsopt) = rvsopt
                | unfys (rvsopt,NONE) = rvsopt
                | unfys (SOME (i,rvis),SOME (_,rvis')) =
                  if ListPair.allEq (fn ((_,rv),(_,rv')) => RegVar.eq(rv,rv')) (rvis,rvis')
                  then SOME (i,rvis)
                  else raise Fail "already annotated with explicit regions"
              fun unfy (NONE,rvopt) = rvopt
                | unfy (rvopt,NONE) = rvopt
                | unfy (SOME (i,rv),SOME(i',rv')) =
                  if RegVar.eq(rv,rv') then SOME(i,rv)
                  else raise Fail "already annotated with a region"
              fun get_rvopt () =
                  case irvs of
                      (_,[rv]) => SOME rv
                    | _ => raise Fail "expecting a single region"
          in case TypeDesc of
                 CONSTYPE(ts,tn,irvsopt_u) =>
                 mk(CONSTYPE(ts,tn,URef.uref(unfys(URef.!!irvsopt_u,SOME irvs))))
               | RECTYPE(rt,rvopt_u) =>
                 mk(RECTYPE(rt,URef.uref(unfy(URef.!!rvopt_u,get_rvopt()))))
               | ARROW(t1,rvopt0_u,t2,rvopt_u) =>
                 mk(ARROW(t1,rvopt0_u,t2,URef.uref(unfy(URef.!!rvopt_u,get_rvopt()))))
               | _ => raise Fail "type variable"
          end

      fun remove_regvars (regvars:RegVar.regvar list) (t:Type) : Type =
          case regvars of
              nil =>  t
            | _ =>
              let fun remrvi rvi =
                      case URef.!! rvi of
                          NONE => (rvi,false)
                        | SOME (_,rv) => if List.exists (fn r => RegVar.eq (r,rv)) regvars
                                         then (URef.uref NONE,true)
                                         else (rvi,false)
                  fun remrvis rvis =
                      case URef.!! rvis of
                          NONE => (rvis,false)
                        | SOME (_,rvs) => if List.exists (fn r => List.exists (fn (_,rv) => RegVar.eq (r,rv)) rvs)
                                                         regvars
                                          then (URef.uref NONE,true)
                                          else (rvis,false)
                  fun remt t : Type * bool =
                      let val t as {TypeDesc,level} = findType t
                          fun mkRecType (rt,rvi) : Type * bool =
                              ({TypeDesc=RECTYPE(rt,rvi),level=level},true)
                          fun mkArrow (t1,rvi0,t2,rvi) : Type * bool =
                              ({TypeDesc=ARROW(t1,rvi0,t2,rvi),level=level},true)
                          fun mkConstype (ts,tn,rvis) : Type * bool =
                              ({TypeDesc=CONSTYPE(ts,tn,rvis),level=level},true)
                      in case TypeDesc of
                             RECTYPE(rt,rvi) =>
                             let val (rt',b1) = remrt rt
                                 val (rvi',b2) = remrvi rvi
                             in if b1 orelse b2 then mkRecType (rt',rvi')
                                else (t,false)
                             end
                           | ARROW(tau1,rvi0,tau2,rvi) =>
                             let val (tau1',b1) = remt tau1
                                 val (tau2',b2) = remt tau2
                                 val (rvi',b3) = remrvi rvi
                                 val (rvi0',b4) = remrvi rvi0
                             in if b1 orelse b2 orelse b3 orelse b4 then mkArrow(tau1',rvi0',tau2',rvi')
                                else (t,false)
                             end
                           | CONSTYPE(ts,tn,rvis) =>
                             let val (ts',b1) = remts ts
                                 val (rvis',b2) = remrvis rvis
                             in if b1 orelse b2 then mkConstype(ts',tn,rvis')
                                else (t,false)
                             end
                           | TYVAR _ => (t,false)
                      end
                  and remts ts : Type list * bool =
                      let val tbs = List.map remt ts
                          val b = List.exists #2 tbs
                      in if b then (List.map #1 tbs,true)
                         else (ts,false)
                      end
                  and remrt rt : RecType * bool =
                      let val rt = findRecType rt
                      in case rt of
                             NILrec => (rt,false)
                           | VARrec _ => (rt,false)
                           | ROWrec (lab,t,rt') =>
                             let val (t',b1) = remt t
                                 val (rt',b2) = remrt rt'
                             in if b1 orelse b2 then (ROWrec(lab,t',rt'),true) else (rt,false)
                             end
                      end
              in #1(remt t)
              end

      fun contains_regvars (t:Type) : bool =
          let fun check ur =
                  case URef.!! ur of
                      SOME _ => true
                    | NONE => false
          in case #TypeDesc (findType t) of
                 RECTYPE(rt,rvi) => check rvi orelse contains_regvars_rt rt
               | ARROW(t,rvi0,t',rvi) => check rvi orelse check rvi0 orelse contains_regvars t orelse contains_regvars t'
               | TYVAR _ => false
               | CONSTYPE(ts,_,rvis) => List.foldl (fn (t,acc) => acc orelse contains_regvars t) (check rvis) ts
          end

      and contains_regvars_rt (rt:RecType) : bool =
          case findRecType rt of
              NILrec => false
            | VARrec _ => false
            | ROWrec (_,t,rt) => contains_regvars t orelse contains_regvars_rt rt

    end (*Type*)



    type TypeScheme = TyVar list * regvar list * Type

    structure TypeScheme =
      struct

        (* Prettyprinting of type schemes *)

        fun pretty_string tvn (_, _, tau) = Type.pretty_string tvn tau
        fun pr_regvars [] = ""
          | pr_regvars rs = "(" ^ String.concatWith "," (map RegVar.pr rs) ^ ")"
        fun layout (alphas, regvars, tau) =
          PP.NODE{start="all" ^ TyVar.pr_tyvars alphas ^ pr_regvars regvars ^ ".",finish="",
                  indent=1,childsep=PP.NOSEP,children=[Type.layout tau]}
        fun string (alphas, regvars, tau) = "all" ^ TyVar.pr_tyvars alphas ^ pr_regvars regvars ^ "." ^ Type.string tau
        fun debug_string s sigma = (s ^ ": sigma= " ^ string sigma ^ "\n")

        (* instance_with_types (sigma, taus); instantiate sigma with
         * taus for the bound type variables. Is it a good idea to
         * update the levels on the fly? ME 1998-11-20 *)

        fun instance_with_types (([],[],tau),[]) = tau
          | instance_with_types ((tvs,regvars,tau),taus) =
            let val tau = Type.remove_regvars regvars tau
            fun instanceType ty =
              let val ty = findType ty
                  val {TypeDesc, level} = ty
              in if !level <> Level.GENERIC then ty
                 else case TypeDesc
                        of TYVAR (tv as ref (NO_TY_LINK {inst=ref(SOME ty),...})) => ty
                         | TYVAR (tv as ref (NO_TY_LINK {inst=ref NONE,...})) =>
                          die "instanceType.generic tyvar not instantiated"
                         | TYVAR (ref (TY_LINK _)) => die "instanceType.findType doesn't work"
                         | ARROW (ty1,rvi0,ty2,rvi) => Type.mk_Arrow (instanceType ty1, URef.!! rvi0, instanceType ty2, URef.!! rvi)
                         | RECTYPE (r,rvi) => Type.from_RecType (instanceRecType r,URef.!! rvi)
                         | CONSTYPE (tys,tyname,rvis) => Type.mk_ConsType (map instanceType tys,tyname, URef.!! rvis)
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

        fun instance' (sigma as (tyvars,_,ty)) =
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

        (* explicit region variables are ignored in comparisons (currently!) *)

        fun eq (([],_,ty1), ([],_,ty2)) = Type.eq(ty1,ty2)
          | eq (sigma1 as (tvs1,_,_), sigma2 as (tvs2,_,_)) =
          length tvs1 = length tvs2 andalso
          let fun fresh (tv as ref(NO_TY_LINK tvdesc)) = Type.from_TyVar(TyVar.refresh tv)
                | fresh _ = die ("eq.fresh: tysch= " ^ string sigma1)
              val types = map fresh tvs1
              val ty1 = instance_with_types (sigma1, types)
              val ty2 = instance_with_types (sigma2, types)
          in Type.eq(ty1,ty2)
          end

        fun to_TyVars_and_Type x = x
        fun from_Type tau = ([],[],tau)

        (* Functions for finding free type variables and free type
         * names in a type scheme *)

        fun tyvars (_,_,tau) = Type.tyvars tau
        fun tynames (_,_,tau) = Type.tynames tau

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

            case Type.restricted_unify {restricted_tyvars=TyVar.unionTyVarSet(fv_tau',fv_sigma),
                                        unify_regvars=false} (tau,tau')
              of Type.UnifyOk => true
               | _ => false
          end

        fun generalises_TypeScheme (sigma, (betas,_,tau')) : bool =
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

            case Type.restricted_unify {restricted_tyvars=TyVar.unionTyVarSet(fv_tau',fv_sigma),
                                        unify_regvars=false} (tau,tau')
              of Type.UnifyOk => true
               | _ => false
          end

        (* close_overload tau; close a type and do not avoid
         * generalisation of overloaded type variables; this function
         * is used for creating type schemes for primitives. *)

        fun close_overload tau =
          let val tvs = (Type.generalise {ov=true, imp=true, tau=tau};
                         Type.generic_tyvars tau)
          in (tvs, nil, tau)
          end

        (* close imp sigma; close a type scheme. The tyvars list
         * should be empty. The boolean imp should be false if the
         * valbind that tau is the type of is expansive. *)

        fun close imp ([], regvars, tau) =
          let val tvs = (Type.generalise {ov=false, imp=imp, tau=tau};
                         Type.generic_tyvars tau)
          in (tvs, regvars, tau)
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
             | RECTYPE (r,_) =>
              (Type.RecType.fold (fn (tau, res) => res orelse violates_equality0 T tau)
               false r)
             | CONSTYPE (taus, tyname, _) =>
              (if TyName.equality tyname orelse TyName.Set.member tyname T then
                 foldl (fn (tau, res) => res orelse violates_equality0 T tau)
                 false taus
               else true)
             | ARROW _ => true
      in
        fun violates_equality (T : TyName.Set.Set) (sigma : TypeScheme) : bool =
              let val (_, _, tau) = to_TyVars_and_Type sigma
              in case Type.un_Arrow tau
                   of NONE => false                                (*nullary constructor*)
                    | SOME (tau', _, _, _) => violates_equality0 T tau'  (*unary constructor*)
              end
      end (*local*)

      (* Matching for the recompilation manager *)

      fun match ((_,_,tau1),(_,_,tau2)) : unit = Type.match(tau1,tau2)

      fun close_regvars R sigma =
          case sigma of
              (tvs,nil,t) => (tvs,R,t)
            | _ => die "close_regvars: type scheme already abstract over a set of regvars"

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
          TypeScheme.eq((tyvars,nil,tau),(tyvars',nil,tau'))

        fun from_TyVars_and_Type (tyvars : TyVar list, tau : Type) =
          (Type.generalise {ov=false, imp=true, tau=tau};
           TYPEFCN {tyvars=tyvars, tau=tau})

        fun apply (theta as (TYPEFCN {tyvars, tau}), taus : Type list) : Type =
          TypeScheme.instance_with_types ((tyvars,nil,tau),taus)

        fun arity (TYPEFCN {tyvars, tau}) = length tyvars

        (* admits_equality: We need only check if the type in an type
         * function admits equality because the bound type variables
         * have already been renamed to admit equality. *)

        fun admits_equality (theta as TYPEFCN {tyvars, tau}) : bool =
          case Type.make_equality (TypeScheme.instance (tyvars,nil,tau))
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
              val tau = Type.mk_ConsType (map Type.from_TyVar tyvars, tyname, NONE)
              val _ = Level.pop ()
          in from_TyVars_and_Type (tyvars, tau)
          end

        fun to_TyName (TYPEFCN {tyvars, tau}) : TyName option =
          case Type.un_ConsType tau
            of SOME (taus,t,_) =>
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
                | {TypeDesc = ARROW (ty1,_,ty2,_), level} =>
                 level := Int.min (correct_levels_Type ty1, correct_levels_Type ty2)
                | {TypeDesc = RECTYPE (r,_), level} =>
                 level := correct_levels_RecType r
                | {TypeDesc = CONSTYPE (tys,tyname,_), level} =>
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
           | {TypeDesc = RECTYPE (r,rvi), level} =>
               {TypeDesc = RECTYPE (Type.RecType.map (on_Type phi) r,rvi),
                level = ref (!level)}
           | {TypeDesc = ARROW(ty1,rvi0,ty2,rvi), level} =>
               {TypeDesc = ARROW(on_Type phi ty1, rvi0, on_Type phi ty2, rvi),
                level = ref (!level)}
           | {TypeDesc = CONSTYPE(tylist,t,rvis),level} =>
               (*definition pg 19. sec. 4.4 Types and Type Functions
                beta-conversion carried out after substituting in type
                functions for type names*)
               let val theta = on_TyName' phi t

                   fun TypeFcn_apply' (TYNAME t', tau_list) =
                         {TypeDesc = CONSTYPE (tau_list,t',rvis),
                          level = ref (!level)}
                     | TypeFcn_apply' (EXPANDED theta, tau_list) =          (* memo: figure out what to do with rvis - push it down into tau... *)
                         let val tau = (TypeFcn.apply (theta, tau_list))
                         in correct_levels_Type tau ;
                           tau
                         end
               in
                 TypeFcn_apply' (theta, map (on_Type phi) tylist)
               end)


      fun on_TypeScheme Realisation_Id scheme = scheme
        | on_TypeScheme phi (sigma as (tyvars,regvars,tau)) =
          if List.exists TyVar.is_overloaded tyvars then sigma  (* type schemes for overloaded identifiers are rigid *)
          else let val tau = on_Type phi tau
               in (tyvars,regvars,tau)
               end

      fun on_TypeFcn Realisation_Id theta = theta
        | on_TypeFcn phi (theta as TYPEFCN {tyvars, tau}) =
        TYPEFCN{tyvars=tyvars,tau=on_Type phi tau}   (* NOTE: arity of theta should be preserved, which differ
                                                      * from the case for type schemes ; mael-2007-11-07 *)
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
           | Not_Id m2 => Not_Id(TyName.Map.plus(m1, m2))

      fun enrich (rea0, (rea,T)) =
        TyName.Set.fold (fn t => fn acc => acc andalso
                         TypeFcn.eq(on_TyName rea0 t, on_TyName rea t)) true T

      fun eq (Realisation_Id, Realisation_Id) = true       (* conservative check, thus eq is a bad word for it;
                                                            * - better now ; mael 2004-04-06 *)
        | eq (rea1,rea2) =
        let val T = TyName.Set.union (dom rea1) (dom rea2)
        in enrich (rea1,(rea2,T))
        end
      fun match (Realisation_Id, rea0) = ()
        | match (Not_Id m, rea0) =
        let fun convert (EXPANDED theta) = theta
              | convert (TYNAME t) = TypeFcn.from_TyName t
        in TyName.Map.Fold(fn ((t, theta),_) => TypeFcn.match(convert theta,on_TyName rea0 t)) () m
        end
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
        | Arrow of pty * regvar_info option * pty * regvar_info option
        | Rectype of (Lab.lab * pty) list * regvar_info option
        | Constype of pty list * TyName.TyName * (ParseInfo.ParseInfo * regvar_info list) option

      type ptysch = ptv list * RegVar.regvar list * pty

      val pu_ptv : ptv Pickle.pu =
          Pickle.convert (fn (id,equality,overloaded) => {id=id,equality=equality,overloaded=overloaded},
                          fn {id,equality,overloaded} => (id,equality,overloaded))
                         (Pickle.tup3Gen0(Pickle.int,
                                          Pickle.bool,
                                          Pickle.optionGen(TyName.Set.pu TyName.pu)))

      fun pu_const (v:'a) : 'a Pickle.pu = Pickle.convert (fn () => v, fn _ => ()) Pickle.unit

      val pu_pos : LexBasics.pos Pickle.pu =
          pu_const LexBasics.DUMMY

      val pu_source_info : SourceInfo.SourceInfo Pickle.pu =
          Pickle.convert0 (fn (l,r) => SourceInfo.from_positions l r,
                           SourceInfo.to_positions) (Pickle.pairGen0(pu_pos,pu_pos))
      val pu_parse_info : ParseInfo.ParseInfo Pickle.pu =
          Pickle.convert0 (ParseInfo.from_SourceInfo, ParseInfo.to_SourceInfo) pu_source_info
      val pu_regvar_info : regvar_info Pickle.pu =
          Pickle.pairGen0(pu_parse_info,RegVar.pu)
      val pu_regvar_info_opt : regvar_info option Pickle.pu =
          Pickle.optionGen pu_regvar_info
      val pu_regvar_info_list_opt : (ParseInfo.ParseInfo * regvar_info list) option Pickle.pu =
          Pickle.optionGen (Pickle.pairGen0(pu_parse_info,Pickle.listGen pu_regvar_info))
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
                              (Pickle.tup4Gen0(pu_pty, pu_regvar_info_opt, pu_pty, pu_regvar_info_opt))
              fun fun_Rectype pu_pty =
                  Pickle.con1 Rectype (fn Rectype a => a | _ => die "pu_pty.Rectype")
                              (Pickle.pairGen0(Pickle.listGen(Pickle.pairGen0(Lab.pu,pu_pty)),
                                               pu_regvar_info_opt))
              fun fun_Constype pu_pty =
                  Pickle.con1 Constype (fn Constype a => a | _ => die "pu_pty.Constype")
                              (Pickle.tup3Gen0(Pickle.listGen pu_pty, TyName.pu, pu_regvar_info_list_opt))
          in Pickle.dataGen ("PickleType.pu_pty",toInt,
                             [fun_Tyvar,fun_Arrow,
                              fun_Rectype,fun_Constype])
          end

      val pu_ptysch : ptysch Pickle.pu =
          Pickle.tup3Gen(Pickle.listGen pu_ptv, Pickle.listGen RegVar.pu, pu_pty)

      local
        type ptvMap = pty IntFinMap.map

        fun type_to_pty0 (S: ptvMap) (ty:Type) : pty =
            case #TypeDesc(findType ty) of
                TYVAR (ref (NO_TY_LINK tvd)) =>
                (case IntFinMap.lookup S (#id tvd) of
                     SOME pty => pty
                   | NONE => die "type_to_pty0: lookup")
              | TYVAR _ => die "type_to_pty0.TYVAR link: impossible"
              | ARROW (ty1,rvi0,ty2,rvi) => Arrow(type_to_pty0 S ty1, URef.!! rvi0, type_to_pty0 S ty2, URef.!! rvi)
              | RECTYPE (rt,rvi) => let fun collect (rt,acc) =
                                            case findRecType rt of
                                                NILrec => rev acc
                                              | ROWrec (l,ty,rt) => collect (rt,(l,type_to_pty0 S ty)::acc)
                                              | VARrec _ => die "type_to_pty0:VARrec"
                                    in Rectype (collect (rt,nil),URef.!! rvi)
                                    end
              | CONSTYPE (tys,tn,rvis) => Constype (map (type_to_pty0 S) tys,tn,URef.!! rvis)
      in
        fun type_to_pty (ty:Type) : pty =
            type_to_pty0 IntFinMap.empty ty
        fun typesch_to_ptysch ((tyvars,regvars,ty): TyVar list * RegVar.regvar list * Type) : ptysch =
            let val l =
                    mapi (fn (tyvar as ref (NO_TY_LINK tvd), i) =>
                             (#id tvd, {id=i,
                                        equality= #equality tvd,
                                        overloaded= #overloaded tvd})
                           | _ => die "typesch_to_ptysch"
                         ) tyvars
                val S = IntFinMap.fromList (map (fn (id,tv) => (id,Tyvar tv)) l)
            in (map #2 l, regvars, type_to_pty0 S ty)
            end
      end

      local
        structure H = Pickle.Hash
        type hm = word IntFinMap.map
        fun parseinfo_hash0 (pi:ParseInfo.ParseInfo) acc =
            H.string (PP.flatten1 (ParseInfo.layout pi)) acc
        fun rvi_hash0 rvi acc =
            case rvi of
                NONE => acc
              | SOME (i,rv) => parseinfo_hash0 i (H.string (RegVar.pr rv) acc)
        fun rvis_hash0 rvis acc =
            case rvis of
                NONE => acc
              | SOME (i,rvis) =>
                let fun loop nil acc = acc
                      | loop ((i,rv)::rvis) acc = H.comb (loop rvis) (parseinfo_hash0 i (H.string (RegVar.pr rv) acc))
                in loop rvis (parseinfo_hash0 i acc)
                end
        fun type_hash0 (hm:hm) (ty:Type) (acc:H.acc) : H.acc =
            case #TypeDesc(findType ty) of
                TYVAR (ref (NO_TY_LINK tvd)) =>
                H.comb (fn acc => case IntFinMap.lookup hm (#id tvd) of
                                      SOME w => H.word w acc
                                    | NONE => die "type_hash: lookup") acc
              | TYVAR _ => die "type_hash.TYVAR link: impossible"
              | ARROW (ty1,rvi0,ty2,rvi) =>
                rvi_hash0 (URef.!! rvi)
                          (rvi_hash0 (URef.!! rvi0)
                                     (H.comb (type_hash0 hm ty2) (H.comb (type_hash0 hm ty1) (H.comb (H.word 0w1377) acc))))
              | RECTYPE (rt,rvi) =>
                let fun loop rt acc =
                        case findRecType rt of
                            NILrec => acc
                          | ROWrec (l,ty,rt) => H.comb (loop rt) (H.comb (H.string (Lab.pr_Lab l)) (H.comb (type_hash0 hm ty) acc))
                          | VARrec _ => die "type_hash:VARrec"
                in rvi_hash0 (URef.!! rvi) (loop rt (H.word 0w4513 acc))
                end
              | CONSTYPE (tys,tn,rvis) =>
                let fun loop nil acc = acc
                      | loop (ty::tys) acc = H.comb (loop tys) (H.comb (type_hash0 hm ty) acc)
                in rvis_hash0 (URef.!! rvis) (loop tys (H.int (#1(TyName.id tn)) acc))
                end

        fun regvars_hash nil (acc:H.acc) : H.acc = acc
          | regvars_hash (rv::xs) acc = H.comb (H.string (RegVar.pr rv)) (regvars_hash xs acc)
      in
        fun type_hash (ty:Type) (acc:H.acc) : H.acc =
            type_hash0 IntFinMap.empty ty acc

        fun typesch_hash ((tyvars,regvars,ty): TyVar list * RegVar.regvar list * Type) (acc:H.acc) : H.acc =
            let val l =
                    mapi (fn (tyvar as ref (NO_TY_LINK tvd), i) =>
                             (#id tvd, Word.fromInt i)
                           | _ => die "typesch_hash"
                         ) tyvars
                val hm = IntFinMap.fromList l
            in regvars_hash regvars (type_hash0 hm ty acc)
            end
      end

      local (* all type schemes are closed, which leads to a simple implementation *)
        structure H = Pickle.Hash
        type im = int IntFinMap.map
        fun rvi_eq (rvi1,rvi2) =
            case (rvi1,rvi2) of
                (NONE,NONE) => true
              | (SOME(_,rv1),SOME(_,rv2)) => RegVar.eq(rv1,rv2)
              | _ => false
        fun rvis_eq (rvis1,rvis2) =
            case (rvis1,rvis2) of
                (NONE,NONE) => true
              | (SOME (_,rvis1), SOME (_,rvis2)) => ListPair.allEq (fn ((_,rv1),(_,rv2)) => RegVar.eq(rv1,rv2)) (rvis1,rvis2)
              | _ => false
        fun type_eq0 (im:im,ty1:Type,ty2:Type) : bool =
            case (#TypeDesc(findType ty1), #TypeDesc(findType ty2)) of
                (TYVAR (ref (NO_TY_LINK tvd1)), TYVAR (ref (NO_TY_LINK tvd2)))  =>
                (case IntFinMap.lookup im (#id tvd1) of
                     SOME i => i = #id tvd2 andalso #equality tvd1 = #equality tvd2
                   | NONE => false)
              | (ARROW (ty1,rvi0,ty2,rvi), ARROW (ty1',rvi0',ty2',rvi')) =>
                type_eq0 (im,ty1,ty1') andalso type_eq0 (im,ty2,ty2')
                andalso rvi_eq(URef.!! rvi,URef.!! rvi')
                andalso rvi_eq(URef.!! rvi0,URef.!! rvi0')
              | (RECTYPE (rt1,rvi1), RECTYPE (rt2,rvi2)) =>
                let fun loop (rt1,rt2) =
                        case (findRecType rt1, findRecType rt2) of
                            (NILrec, NILrec) => true
                          | (ROWrec (l1,ty1,rt1), ROWrec (l2,ty2,rt2)) =>
                            l1 = l2 andalso type_eq0 (im,ty1,ty2) andalso loop (rt1,rt2)
                          | _ => false
                in loop (rt1,rt2) andalso rvi_eq(URef.!! rvi1,URef.!! rvi2)
                end
              | (CONSTYPE (tys1,tn1,rvis1), CONSTYPE (tys2,tn2,rvis2)) =>
                let fun loop (nil,nil) = true
                      | loop (ty1::tys1, ty2::tys2) = type_eq0 (im,ty1,ty2) andalso loop (tys1,tys2)
                      | loop _ = false
                in TyName.eq(tn1,tn2) andalso loop (tys1,tys2)
                   andalso rvis_eq(URef.!! rvis1,URef.!! rvis2)
                end
              | _ => false
      in
        fun type_eq (ty1:Type,ty2:Type) : bool =
            type_eq0 (IntFinMap.empty,ty1,ty2)

        fun map2 f (nil,nil) = nil
          | map2 f (x::xs, y::ys) = f(x,y)::map2 f (xs,ys)
          | map2 f _ = nil

        fun typesch_eq ((tvs1,regvars1,ty1):TyVar list * RegVar.regvar list * Type,
                        (tvs2,regvars2,ty2):TyVar list * RegVar.regvar list * Type) : bool =   (* disregard regvars *)
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

        type pos0 = {file:string,left:int*int,right:int*int}
        fun info_from_pos0 {file,left:int*int,right:int*int} : ParseInfo.ParseInfo =
            let fun to_pos (l,c) = LexBasics.POSITION(fn() => {file=file,line=l,column=c,getLine=fn _ => "**no line info**"})
                val si = SourceInfo.from_positions (to_pos left) (to_pos right)
            in ParseInfo.from_SourceInfo si
            end
        fun pos0_from_info (pi:ParseInfo.ParseInfo) : pos0 =
            let val (left,right) = SourceInfo.to_positions(ParseInfo.to_SourceInfo pi)
                val rfile = ref "no file"
                fun to_line_column p =
                    case p of
                        LexBasics.POSITION f => let val {file,line,column,...} = f()
                                                in rfile := file ; (line,column)
                                                end
                      | _ => (0,0)
            in {left=to_line_column left,right=to_line_column right,file= !rfile}
            end
        (* Notice: level is GENERIC if level of a subpart is GENERIC *)
        fun pty_to_type0 (V: tyMap) (pty:pty) : Type =
            case pty of
                Tyvar {id,...} =>
                let val t = Vector.sub(V,id)
                            handle Subscript => die "pty_to_type0.id missing"
                in t
                end
              | Arrow(pty1,rvi0,pty2,rvi) =>
                let val ty1 = pty_to_type0 V pty1
                    val ty2 = pty_to_type0 V pty2
                in {TypeDesc=ARROW(ty1,URef.uref rvi0,ty2,URef.uref rvi),
                    level=ref(Int.min(!(#level ty1),
                                      !(#level ty2)))
                   }
                end
              | Rectype (lptys,rvi) =>
                let val (r,l) = List.foldr (fn ((lab,pty),(rt,lev)) =>
                                               let val ty = pty_to_type0 V pty
                                               in (ROWrec (lab,ty,rt),
                                                   Int.min(lev,!(#level ty)))
                                               end) (NILrec,Level.NONGENERIC) lptys
                in {TypeDesc=RECTYPE(r,URef.uref rvi),
                    level=ref l
                   }
                end
              | Constype (ptys, tn, rvis) =>
                let val (tys,l) = List.foldr (fn (pty,(tys,lev)) =>
                                               let val ty = pty_to_type0 V pty
                                               in (ty::tys,
                                                   Int.min(lev,!(#level ty)))
                                               end) (nil,Level.NONGENERIC) ptys
                in {TypeDesc=CONSTYPE (tys,tn,URef.uref rvis),
                    level=ref l
                   }
                end
      in
        fun pty_to_type (ty:pty) : Type =
            pty_to_type0 (Vector.tabulate (0, fn _ =>raise Fail "impossible")) ty

        fun ptysch_to_typesch (ptvs:ptv list, regvars:RegVar.regvar list, pty:pty)
            : TyVar list * RegVar.regvar list * Type =
            let val l = map (fn {id,equality,overloaded} =>
                                TyVar.fresh0 { equality=equality
                                             , overloaded=overloaded
                                             , explicit=NONE
                                             }
                            ) ptvs
                val V = Vector.fromList (map (tdToType o TYVAR) l)
            in (l, regvars, pty_to_type0 V pty)
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
          let fun to (tvs,_,t) = TYPEFCN {tyvars=tvs,tau=t}
              fun from (TYPEFCN {tyvars=tvs,tau=t}) = (tvs,nil,t)
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
