(*TyVar, tau in Type, FunType, ConsType, RecType, sigma in
 TypeScheme, theta in TypeFcn, Definition 1997, fig. 10, p. 16;
 phi in realisation, §5.2, p. 29.  Also Level and Substitution.*)

(*$STATOBJECT : TYNAME*)

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
    type StringTree
    type ExplicitTyVar (*the type of type variables explicit in the source*)
    structure TyName : TYNAME
    type TyName sharing type TyName = TyName.TyName
	        sharing type StringTree = TyName.Set.StringTree
    type lab 
    type scon
    type strid



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
	val equality                : TyVar -> bool
	val fresh_normal            : unit -> TyVar
	val fresh_overloaded        : TyName list -> TyVar
	val from_ExplicitTyVar      : ExplicitTyVar -> TyVar
	val to_ExplicitTyVar        : TyVar -> ExplicitTyVar Option
	val is_overloaded           : TyVar -> bool
	val string                  : TyVar -> string
	val pretty_string           : TVNames -> TyVar -> string
	val layout                  : TyVar -> StringTree

	(*TODO 26/01/1997 14:17. tho.: ugly ad hoc set functions:*)
	val intersectTyVarSet       : TyVar list * TyVar list -> TyVar list
	val unionTyVarSet           : TyVar list * TyVar list -> TyVar list
      end (*TyVar*)



    structure Type :
      sig
	val eq                      : Type * Type -> bool
	val fresh_normal            : unit -> Type (*fresh_normal () = a fresh tyvar*)
	val tyvars                  : Type -> TyVar list
	val tynames                 : Type -> TyName.Set.Set
	val string                  : Type -> string
	val pretty_string           : TVNames -> Type -> string
	val string_as_ty            : Type * Type -> string
	      (*The second type is a guide for printing*)
	val pretty_string_as_ty     : TVNames -> (Type*Type) -> string
	val layout                  : Type -> StringTree
	val from_TyVar              : TyVar -> Type
	val from_TyVar'             : level -> TyVar -> Type
	      (*used to elaborate explicit tyvar's*)
	val to_TyVar                : Type -> TyVar Option

	(*record types*)
	val from_RecType            : RecType -> Type
	val to_RecType              : Type -> RecType Option
	val contains_row_variable   : Type -> bool
	    (*contains_row_variable rho = true iff there exists a 
	     row variable in the type rho*)
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
	val to_FunType              : Type -> FunType Option
	val mk_FunType              : Type * Type -> FunType
	val un_FunType              : FunType -> (Type * Type) Option

	(*constructed types*)
	val from_ConsType           : ConsType -> Type
	val to_ConsType             : Type -> ConsType Option
	val mk_ConsType             : Type list * TyName -> ConsType
	val un_ConsType             : ConsType -> (Type list * TyName) Option

	val Exn                     : Type
	val is_Exn                  : Type -> bool
	val mk_Arrow                : Type * Type -> Type
	val un_Arrow                : Type -> (Type * Type) Option
	val is_Arrow                : Type -> bool
	val mk_Ref                  : Type -> Type

	(*special constants*)
	val Int                     : Type
	val Real                    : Type
	val Bool                    : Type   (*needed for initial TE and VE*)
	val String                  : Type
	val Char                    : Type
	val Word                    : Type
	val of_scon                 : scon -> Type

	(*close imp tau = a list of those type variables of tau which are
	 allowed to be quantified.*)

	val close                   : bool -> Type -> TyVar list

	datatype unify_result = UnifyOk (* of Substitution *)
                              | UnifyFail 
                              | UnifyRankError of TyVar * TyName

	val unify                   : Type * Type -> unify_result
	val instantiate_arbitrarily : TyVar -> unit
             (*instantiate_arbitrarily tyvar = instantiate tyvar
	      to some arbitrary type (int).  used by ElabTopdec.elab_topdec
	      when tyvar is free in a topdec.*)

	(*for compilation manager:*)
	val match : Type * Type -> unit
      end (*Type*)



    structure TypeScheme :
      sig
	val eq                      : TypeScheme * TypeScheme -> bool
	val to_TyVars_and_Type      : TypeScheme -> TyVar list * Type
	(*Make a type into a typescheme with no bound variables:*)
	val from_Type               : Type -> TypeScheme
	val tyvars                  : TypeScheme -> TyVar list
	val tynames                 : TypeScheme -> TyName.Set.Set
	val string                  : TypeScheme -> string
	val pretty_string           : TVNames -> TypeScheme -> string
	val layout                  : TypeScheme -> StringTree

	(*Get an instance of a TypeScheme; instance'' also gives
	 the list of types which the generic type variables of the type
	 scheme have been instantiated to.*)

	val instance                : TypeScheme -> Type
	val instance''              : TypeScheme -> Type * Type list  
	val generalises_TypeScheme  : TypeScheme * TypeScheme -> bool
	val generalises_Type        : TypeScheme * Type -> bool

	(*close imp sigma = generalise generic type variables in sigma except
	  overload tyvars; used by Environment.
	  close_and_return_escaping_tyvars is similar, except it also returns
	  a list of tyvars that were not generalised but could have been
	  generalised if there were no value polymorphism restriction.  If
	  there are any such tyvars, we give a type error `Provide type
	  annotation for <ids>' where <ids> are the identifiers containing
	  one of the tyvars in their type.  See ElabDec.elab_dec (VALdec
	  ...)*)

	val close                   : bool -> TypeScheme -> TypeScheme
	val close_and_return_escaping_tyvars
	                            : bool -> TypeScheme -> TypeScheme * TyVar list

	(*close_overload tau = generalise generic type variables also
	 overloaded tyvars.*)

	val close_overload : Type -> TypeScheme

	(*violates_equality T sigma = false, iff, assuming the tynames in
	 T admit equality, sigma admits equality, i.e., violates_equality
	 T sigma = non((all t in T admit equality) => sigma admits
	 equality).  violates_equality is used when maximising equality in
	 a TE (in TE.maximise_TE_equality).  T will be those datatypes in
	 TE we tentatively assume to admit equality, and sigma will be the
	 type scheme of a constructor.*)

	val violates_equality       : TyName.Set.Set -> TypeScheme -> bool

	(*for compilation manager:*)
	val match : TypeScheme * TypeScheme -> unit

      end (*TypeScheme*)



    structure Substitution :
      sig
	val Id                      : Substitution
	val bogus                   : Substitution
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
	val to_TyName               : TypeFcn -> TyName Option
	val is_TyName               : TypeFcn -> bool
	val bogus                   : TypeFcn
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
	val from_T_and_theta        : TyName.Set.Set * TypeFcn -> realisation
	val restrict                : TyName.Set.Set -> realisation -> realisation
	val enrich                  : realisation * (realisation * TyName.Set.Set) -> bool
	val layout                  : realisation -> StringTree
      end (*Realisation*)

  end;

