(*TyVar, tau in Type, FunType, ConsType, RecType, sigma in
 TypeScheme, theta in TypeFcn, Definition 1997, fig. 10, p. 16;
 phi in realisation, �5.2, p. 29.  Also Level and Substitution.*)

signature STATOBJECT =
  sig
    (*types provided by this module:*)
    type level
    type TVNames
    type TyVar
    type Type
    type FunType
    type ConsType
    type RecType
    type Substitution
    type TypeScheme
    type TypeFcn
    type realisation(*tyrea*)

    (*types from other modules:*)
    type ExplicitTyVar (*the type of type variables explicit in the source*)
    type TyName = TyName.TyName
    type StringTree = TyName.Set.StringTree
    type lab
    type scon
    type strid

    type regvar_info = ParseInfo.ParseInfo * RegVar.regvar

    structure ExplicitTyVarMap : MONO_FINMAP where type dom = ExplicitTyVar

    (*Level: for an explanation of type inference using `let levels'
     see Martin Elsman: A Portable Standard ML Implementation.
     Master's Thesis, Tech. Univ. of Denmark, Dept. of C. S. 1994.*)

    structure Level :
      sig
	val push                    : unit -> unit
	val pop                     : unit -> unit
	val GENERIC                 : level
	val current                 : unit -> level
        val pr                      : level -> string
      end

    (*Association list for the printing of type variables:*)
    val newTVNames                  : unit -> TVNames

    structure TyVar :
      sig
	val eq                      : TyVar * TyVar -> bool
	val lt                      : TyVar * TyVar -> bool
        val id                      : TyVar -> int
	val equality                : TyVar -> bool
	val fresh_normal            : unit -> TyVar
	val fresh_overloaded        : TyName list -> TyVar
	  (*fresh_overloaded bla = get a socalled overloaded tyvar that is overloaded
	   to the types indicated by `bla'.*)
	val from_ExplicitTyVar      : ExplicitTyVar -> TyVar
	val is_overloaded           : TyVar -> bool
	val resolve_overloaded      : TyVar -> TyName.Set.Set
	val string                  : TyVar -> string
	val string'                 : (Type -> string) -> TyVar -> string
	val pretty_string           : TVNames -> TyVar -> string
	val layout                  : TyVar -> StringTree

	(*TODO 26/01/1997 14:17. tho.: ugly ad hoc set functions:*)
	val intersectTyVarSet       : TyVar list * TyVar list -> TyVar list
	val unionTyVarSet           : TyVar list * TyVar list -> TyVar list

(*	val pu                      : TyVar Pickle.pu *)
      end (*TyVar*)

    structure Type :
      sig
	val eq                      : Type * Type -> bool
	val fresh_normal            : unit -> Type (*fresh_normal () = a fresh tyvar*)
	val tyvars                  : Type -> TyVar list
	val tynames                 : Type -> TyName.Set.Set
	val string                  : Type -> string
	val string_repl             : Type -> string                (* for use with type-indexed value printing in the REPL *)

	val pretty_string           : TVNames -> Type -> string
	val string_as_ty            : Type * Type -> string
	      (*The second type is a guide for printing*)
	val pretty_string_as_ty     : TVNames -> (Type*Type) -> string
	val layout                  : Type -> StringTree
	val from_TyVar              : TyVar -> Type
	val to_TyVar                : Type -> TyVar option

	(*record types*)
	val from_RecType            : RecType * regvar_info option -> Type
	val to_RecType              : Type -> (RecType * regvar_info option) option
	val contains_row_variable   : Type -> bool
	    (*contains_row_variable rho = true iff there exists a
	     row variable in the type rho*)
        val add_regvars             : ParseInfo.ParseInfo * regvar_info list -> Type -> Type (* raises Fail msg on error *)
        val remove_regvars          : RegVar.regvar list -> Type -> Type
        val contains_regvars        : Type -> bool (* used by CompileDec *)

	structure RecType :
	  sig
	    val empty               : RecType			(* "{}" *)
	    val dotdotdot           : unit -> RecType    	(* "{...}" *)
	    val add_field           : lab * Type -> RecType -> RecType
	    val sorted_labs         : RecType -> lab list (* Needed by compiler. *)
	    val to_list             : RecType -> (lab * Type) list
	          (*needed by compiler. the returned list is sorted
		   (non-ascending) with respect to Lab.<*)
	    val to_pair             : RecType -> Type * Type
	  end
	val from_pair               : Type * Type -> Type
	val from_triple             : Type * Type * Type -> Type
	val Unit                    : Type

	(*function types*)
	val from_FunType            : FunType -> Type
	val to_FunType              : Type -> FunType option
	val mk_FunType              : Type * regvar_info option * Type * regvar_info option -> FunType
	val un_FunType              : FunType -> (Type * regvar_info option * Type * regvar_info option) option

	(*constructed types*)
	val from_ConsType           : ConsType -> Type
	val to_ConsType             : Type -> ConsType option
	val mk_ConsType             : Type list * TyName * (ParseInfo.ParseInfo * regvar_info list) option -> ConsType
	val un_ConsType             : ConsType -> (Type list * TyName * (ParseInfo.ParseInfo * regvar_info list) option) option

	val Exn                     : Type
	val is_Exn                  : Type -> bool
	val mk_Arrow                : Type * regvar_info option * Type * regvar_info option -> Type
	val un_Arrow                : Type -> (Type * regvar_info option * Type * regvar_info option) option
	val is_Arrow                : Type -> bool
	val mk_Ref                  : Type -> Type

	(* Special constants *)
	val Int31                   : Type
	val Int32                   : Type
	val Int63                   : Type
	val Int64                   : Type
	val IntInf                  : Type
	val IntDefault              : unit -> Type   (* Either Int31 or Int32,
						      * dependent on tagging; used
						      * for resolving overloading. *)
	val Real                    : Type
	val Bool                    : Type           (* needed for initial TE and VE *)
	val Word8                   : Type
	val Word31                  : Type
	val Word32                  : Type
	val Word63                  : Type
	val Word64                  : Type
	val WordDefault             : unit -> Type   (* Either Word31 or Word32,
						      * dependent on tagging; used
						      * for resolving overloading. *)
	val String                  : Type
	val Char                    : Type

	val of_scon                 : scon -> {type_scon: Type, overloading : TyVar option}

	datatype unify_result = UnifyOk (* of Substitution *)
                              | UnifyFail of string
                              | UnifyRankError of TyVar * TyName

	val unify : {unify_regvars:bool} -> Type * Type -> unify_result

	val match : Type * Type -> unit   (* for compilation manager *)

	val pu    : Type Pickle.pu

      end (*Type*)



    structure TypeScheme :
      sig
	val eq                      : TypeScheme * TypeScheme -> bool
	val to_TyVars_and_Type      : TypeScheme -> TyVar list * RegVar.regvar list * Type      (* for the compiler *)
	(*Make a type into a typescheme with no bound variables:*)
	val from_Type               : Type -> TypeScheme
	val tyvars                  : TypeScheme -> TyVar list
	val tynames                 : TypeScheme -> TyName.Set.Set
	val string                  : TypeScheme -> string
	val pretty_string           : TVNames -> TypeScheme -> string
	val layout                  : TypeScheme -> StringTree

	(* Get an instance of a TypeScheme; instance' also gives
	 * the list of types to which the generic type variables of the type
	 * scheme have been instantiated to.*)

	val instance                : TypeScheme -> Type
	val instance'               : TypeScheme -> Type * Type list
	val generalises_TypeScheme  : TypeScheme * TypeScheme -> bool
	val generalises_Type        : TypeScheme * Type -> bool

	(* close imp sigma = generalise generic type variables in
	 * sigma except overload tyvars; used by Environment. The bool
	 * should be true for generalisation proper and false if the
	 * type scheme stems from a valbind that is expansive. *)

	val close : bool -> TypeScheme -> TypeScheme

	(* close_overload tau = generalise generic type variables also
	 * overloaded tyvars. *)

	val close_overload : Type -> TypeScheme

	(*violates_equality T sigma = false, iff, assuming the tynames in
	 T admit equality, sigma admits equality, i.e., violates_equality
	 T sigma = non((all t in T admit equality) => sigma admits
	 equality).  violates_equality is used when maximising equality in
	 a TE (in TE.maximise_TE_equality).  T will be those datatypes in
	 TE we tentatively assume to admit equality, and sigma will be the
	 type scheme of a constructor.*)

	val violates_equality : TyName.Set.Set -> TypeScheme -> bool

	(*for compilation manager:*)
	val match : TypeScheme * TypeScheme -> unit

        val close_regvars : RegVar.regvar list -> TypeScheme -> TypeScheme

	val pu : TypeScheme Pickle.pu

      end (*TypeScheme*)



    structure Substitution :
      sig
	val Id                      : Substitution
	val oo                      : Substitution * Substitution -> Substitution
	val on                      : Substitution * Type  -> Type
	val onScheme                : Substitution * TypeScheme -> TypeScheme
      end (*Substitution*)



    structure TypeFcn :
      sig
	val eq                      : TypeFcn * TypeFcn -> bool
	val from_TyVars_and_Type    : TyVar list * Type   -> TypeFcn
	val apply                   : TypeFcn * Type list -> Type
	val arity                   : TypeFcn -> int
	val admits_equality         : TypeFcn -> bool
	val grounded                : TypeFcn * TyName.Set.Set -> bool
	val from_TyName             : TyName  -> TypeFcn
	val to_TyName               : TypeFcn -> TyName option
	val is_TyName               : TypeFcn -> bool
	val tynames                 : TypeFcn -> TyName.Set.Set

	(*pretty_string returns two strings. This is because
	 something like

           type ('a, 'b) Foo = int

         maps Foo to "/\('a, 'b).int" and we need to take this apart to get
	 the correct printout.  pretty_string' will print it as the
	 last-mentioned string.*)

	val pretty_string : TVNames -> TypeFcn -> {vars: string, body: string}
	val pretty_string' : TVNames -> TypeFcn -> string
	val layout : TypeFcn -> StringTree

	(*for compilation manager:*)
	val match : TypeFcn * TypeFcn -> unit

	val pu : TypeFcn Pickle.pu

      end (*TypeFcn*)



    (*Realisation --- used during elaboration to apply a realisation on
     recorded type information.  Notice there is a Realisation structure
     in Environments as well.  It extends this Realisation structure.*)

    structure Realisation :
      sig
	val on_TyName               : realisation -> TyName -> TypeFcn
	val on_TyName_set           : realisation -> TyName.Set.Set -> TyName.Set.Set
	val on_Type                 : realisation -> Type -> Type
	val on_TypeFcn              : realisation -> TypeFcn -> TypeFcn
	val on_TypeScheme           : realisation -> TypeScheme -> TypeScheme
	val Id                      : realisation
	val is_Id                   : realisation -> bool
	val oo                      : realisation * realisation -> realisation
	val singleton               : TyName * TypeFcn -> realisation

	(*from_T_and_tyname (T, t0) = the realisation {t |-> t0 | t in T} *)
	val from_T_and_tyname       : TyName.Set.Set * TyName -> realisation
	val restrict                : TyName.Set.Set -> realisation -> realisation
	val restrict_from           : TyName.Set.Set -> realisation -> realisation
	val renaming                : TyName.Set.Set -> realisation
	val renaming'               : TyName.Set.Set -> TyName.Set.Set * realisation
	val inverse                 : realisation -> realisation option
	val enrich                  : realisation * (realisation * TyName.Set.Set) -> bool
	val match                   : realisation * realisation -> unit
	val dom                     : realisation -> TyName.Set.Set
	val eq                      : realisation * realisation -> bool
	val layout                  : realisation -> StringTree
	val pu                      : realisation Pickle.pu
	val tynamesRng              : realisation -> TyName.Set.Set
      end (*Realisation*)

  end;
