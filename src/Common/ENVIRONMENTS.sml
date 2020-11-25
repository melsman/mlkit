(*VE in VarEnv, TyStr, TE in TyEnv, SE in StrEnv, E in Env, C in
 Context, Definition 1997, fig. 10, p. 16.  And explicit
 tyvars, and constructor_map*)

signature ENVIRONMENTS =
  sig
    (*types provided by this module:*)
    type VarEnv
    type TyStr
    type TyEnv
    type StrEnv
    type Env
    type Context
    type constructor_map

    (*types from other modules:*)
    type TyName = TyName.TyName
    type TyVar
    type Type
    type TypeScheme
    type Substitution
    type TypeFcn
    type level
    type realisation
    eqtype id
    type strid
    type tycon = TyName.tycon
    type ExplicitTyVar
    type longid
    type longtycon
    type longstrid
    type ty (*syntactic class of explicit type annotations*)
    type pat (*syntactic class of patterns*)
    type valbind (*syntactic class of valbinds*)
    type StringTree = TyName.Set.StringTree
    type Report


    val ExplicitTyVarsTy : ty -> ExplicitTyVar EqSet.Set
    val unguarded_valbind : valbind -> ExplicitTyVar list
      (*TODO 26/01/1997 15:37. tho.: both functions above ought
       to return the same type, list or set.
       yes, and they oughta be in DecGrammar instead of here.*)


                       (*value environments*)

    structure VE :
      sig
	(*a VarEnv maps id's to range's:*)
	datatype range = LONGVAR   of TypeScheme
	               | LONGCON   of TypeScheme
	               | LONGEXCON of Type
	val empty                : VarEnv
	val bogus                : VarEnv
	val singleton_var        : id  * TypeScheme -> VarEnv
	val singleton_con        : id * TypeScheme * id list -> VarEnv
	val singleton_excon      : id  * Type -> VarEnv
	val plus                 : VarEnv  * VarEnv -> VarEnv
	val lookup               : VarEnv -> id -> range option
	val dom                  : VarEnv -> id EqSet.Set
	val is_empty             : VarEnv -> bool
	val eq                   : VarEnv * VarEnv -> bool (*up to bound vars*)
	val fold                 : (range -> 'a -> 'a) -> 'a -> VarEnv -> 'a
	val Fold                 : (id * range -> 'b -> 'b) -> 'b -> VarEnv -> 'b
	val apply                : (id * range -> unit) -> VarEnv -> unit

	(*CEfold f a VE = will crash if there is anything else than
	 constructors in VE; fold f over the constructors in VE.
	 CEFold is similar.*)
	val CEfold               : (TypeScheme -> 'a -> 'a) -> 'a -> VarEnv -> 'a
	val CEFold               : (id * TypeScheme -> 'a -> 'a) -> 'a -> VarEnv -> 'a

	val on                   : Substitution * VarEnv -> VarEnv
	val tynames              : VarEnv -> TyName.Set.Set
	val restrict             : VarEnv * id list -> VarEnv
	val close                : VarEnv -> VarEnv
	val layout               : VarEnv -> StringTree
	val report               : (id * range -> Report) * VarEnv -> Report
	val ids_with_tyvar_in_type_scheme : VarEnv -> TyVar -> id list

	val pu                   : VarEnv Pickle.pu
      end (*VE*)


                        (*type structures*)

    structure TyStr :
      sig
	val from_theta_and_VE    : TypeFcn * VarEnv -> TyStr
	val to_theta_and_VE      : TyStr -> TypeFcn * VarEnv
	val to_theta             : TyStr -> TypeFcn
	val to_VE                : TyStr -> VarEnv
	val eq                   : TyStr * TyStr -> bool
	val shares               : TyStr * TyStr -> bool
	val tynames              : TyStr -> TyName.Set.Set
	val layout               : TyStr -> StringTree
	val pu                   : TyStr Pickle.pu
      end (*TyStr*)


                       (*type environments*)

    structure TE :
      sig
	val empty                : TyEnv
	val bogus                : TyEnv
	val singleton            : tycon * TyStr -> TyEnv
	val plus                 : TyEnv * TyEnv -> TyEnv
	val lookup               : TyEnv -> tycon -> TyStr option
	val dom                  : TyEnv -> tycon EqSet.Set
	val fold                 : (TyStr -> 'b -> 'b) -> 'b -> TyEnv -> 'b
	val Fold                 : (tycon * TyStr -> 'c -> 'c) -> 'c -> TyEnv -> 'c
	val apply                : (tycon * TyStr -> unit) -> TyEnv -> unit

	(*init is used by initial_TE in ElabDec and ElabTopdec to make the
	 initial TE used when elaborating datbind's and datdesc's:*)

	val init                 : ExplicitTyVar list -> tycon -> TyEnv
	val init'                : ExplicitTyVar list -> tycon -> TyName * TyEnv
	val report               : {tyEnv : TyEnv, bindings : bool} -> Report
	val tynames              : TyEnv -> TyName.Set.Set
	val layout               : TyEnv -> StringTree

	val pu                   : TyEnv Pickle.pu
      end (*TE*)


                     (*structure environments*)

    structure SE :
      sig
	val empty                : StrEnv
	val singleton            : strid * Env -> StrEnv
	val plus                 : StrEnv * StrEnv -> StrEnv
	val lookup               : StrEnv -> strid -> Env option
	val dom                  : StrEnv -> strid EqSet.Set
	val fold                 : (Env -> 'b  -> 'b) -> 'b -> StrEnv -> 'b
	val Fold                 : (strid * Env -> 'c -> 'c) -> 'c -> StrEnv -> 'c
	val apply                : (strid * Env -> unit) -> StrEnv -> unit
	val map                  : (Env -> Env) -> StrEnv -> StrEnv
	val report               : (strid * Env -> Report) * StrEnv -> Report
	val layout               : StrEnv -> StringTree

	val pu                   : StrEnv Pickle.pu
      end (*SE*)


                          (*environments*)

    structure E :
      sig
	val mk                   : StrEnv * TyEnv * VarEnv * RegVar.regvar list -> Env
	val un                   : Env -> StrEnv * TyEnv * VarEnv * RegVar.regvar list
        val from_R               : RegVar.regvar list -> Env
        val to_R                 : Env -> RegVar.regvar list
	val from_VE_and_TE       : (VarEnv * TyEnv) -> Env
	val from_VE              : VarEnv -> Env
	val to_VE                : Env -> VarEnv
	val from_TE              : TyEnv  -> Env
	val to_TE                : Env -> TyEnv
	val from_SE              : StrEnv -> Env
	val to_SE                : Env -> StrEnv
	val plus                 : Env   * Env -> Env
	val lookup_strid         : Env -> strid -> Env option
	val lookup_strids        : Env -> strid list -> Env option
	val lookup_tycon         : Env -> tycon -> TyStr option
	val lookup_longid        : Env -> longid -> VE.range option
	val lookup_longtycon     : Env -> longtycon -> TyStr option
	val lookup_longstrid     : Env -> longstrid -> Env option
	val on                   : Substitution * Env -> Env
	val empty                : Env
	val initial              : unit -> Env
	val bogus                : Env
	val tyvars               : Env -> TyVar list
	val tyvars'              : Env -> (id * TyVar list) list
	val tynames              : Env -> TyName.Set.Set
	val layout               : Env -> StringTree

	(* Support for recompilation *)
	val restrict             : Env * {longvids:longid list, longtycons:longtycon list,
					  longstrids: longstrid list} -> Env
	val match                : Env * Env -> unit
	val enrich               : Env * Env -> bool   (* strong enrichment *)
	val eq                   : Env * Env -> bool
	val pu                   : Env Pickle.pu
      end (*E*)



                            (*context*)

    structure C :   (* No T-component *)
      sig
	val plus_VE              : Context * VarEnv   -> Context
	val plus_U               : Context * ExplicitTyVar list -> Context
	val plus_U'              : Context * ExplicitTyVar list -> TyVar list * Context
	val plus_E               : Context * Env      -> Context
	val plus_TE              : Context * TyEnv    -> Context
	val plus_VE_and_TE       : Context * (VarEnv * TyEnv) -> Context
	val to_U                 : Context -> ExplicitTyVar list
        val to_R                 : Context -> RegVar.regvar list
	val ExplicitTyVar_lookup : Context -> ExplicitTyVar -> Type option
	val from_E               : Env -> Context
	val on                   : Substitution * Context  -> Context

	val lookup_longid        : Context -> longid -> VE.range option
	val lookup_longtycon     : Context -> longtycon -> TyStr option
	val lookup_longstrid     : Context -> longstrid -> Env option
	val lookup_tycon         : Context -> tycon -> TyStr option

	(*Additional function for taking a longid (which must represent a
	 constructor) to a complete list of its fellow constructors. Needed
	for compilation and exhaustiveness checking. Returns the cons in
	a canonical order. Note: assumed to succeed (i.e. you must do the
	lookup_longid first).*)

	val lookup_fellow_constructors : Context -> longid -> id list

	(* C.close is the function Clos defined p. 20, only used in
	 * rule 15, i.e., in ElabDec.elab_dec (C, VALdec ...). *)

	val close                : Context * valbind * VarEnv -> VarEnv

	val dom_pat              : Context * pat * bool -> id list
              (*dom_pat (C, pat) = the list of id's bound by pat---i.e.,
	       only variables and not constructors appearing in pat;
	       therefore C is needed to get the identifier status of id's. In valrec
	       bindings, the constructor and exception constructor status may be
	       overwritten. If the boolean is true, identifiers with constructor status are
	       also included in the result.
	       *)

	val layout               : Context -> StringTree

	val pu                   : Context Pickle.pu
      end (*C*)


                        (*constructor_map*)

    (*the result of a conbind or a condesc is a constructor_map
     which maps the constructors of the conbind (condesc) to
     their type schemes.  A VE wants to map each constructor to its
     fellow constructors, therefore constructors of a conbind
     (condesc) are first collected in a constructor_map; only when
     all constructors are recorded, can the constructor_map be
     converted to a VE.  20/01/1997 16:13. tho.*)

    structure constructor_map :
      sig
	val empty                : constructor_map
	val add                  : id -> TypeScheme -> constructor_map -> constructor_map
	val in_dom               : id -> constructor_map -> bool
	val to_VE                : constructor_map -> VarEnv
      end


                          (*realisations*)

    (*used during elaboration to apply a realisation on recorded
     type information.  There is also a Realisation structure in
     StatObject with some of the same functions, which is rather
     unsatisfactory.  Probably there are better ways to organise
     this, but they are not obvious.  30/01/1997 16:38. tho.*)

    structure Realisation :
      sig
	val on_TyName            : realisation -> TyName -> TypeFcn
	val on_TyName_set        : realisation -> TyName.Set.Set -> TyName.Set.Set
	val on_Type              : realisation -> Type -> Type
	val on_TypeFcn           : realisation -> TypeFcn -> TypeFcn
	val on_TypeScheme        : realisation -> TypeScheme -> TypeScheme
	val on_VarEnv            : realisation -> VarEnv -> VarEnv
	val on_TyStr             : realisation -> TyStr -> TyStr
	val on_TyEnv             : realisation -> TyEnv -> TyEnv
	val on_StrEnv            : realisation -> StrEnv -> StrEnv
	val on_Env               : realisation -> Env -> Env
	val Id                   : realisation
	val is_Id                : realisation -> bool
	val oo                   : realisation * realisation -> realisation
	val singleton            : TyName * TypeFcn -> realisation

	(*from_T_and_tyname (T, t0) = the realisation {t |-> t0 | t in T} *)
	val from_T_and_tyname    : TyName.Set.Set * TyName -> realisation

	val restrict             : TyName.Set.Set -> realisation -> realisation
	val restrict_from        : TyName.Set.Set -> realisation -> realisation
	val inverse              : realisation -> realisation option

	(* enrich(phi0,(phi,T)) : phi(t) = phi0(t), for each t in T *)
	val enrich               : realisation * (realisation * TyName.Set.Set) -> bool

	val match                : realisation * realisation -> unit


	val dom                  : realisation -> TyName.Set.Set      (* not quite the Supp.. *)
	val eq                   : realisation * realisation -> bool

	(*renaming T = a realisation that maps each tyname in T
	 to a fresh tyname:*)

	val renaming             : TyName.Set.Set -> realisation
	val renaming'            : TyName.Set.Set -> TyName.Set.Set * realisation
	val layout               : realisation -> StringTree

	val pu                   : realisation Pickle.pu

      end (*Realisation*)

    val ABS : TyEnv * Env -> TyName list * Env * realisation
	  (* The realisation returned maps abstract type names into
	   * type names for the datbind. ABS also returns the set of new
	   * generated names. *)

    (*maximise_equality_in_VE_and_TE (VE, TE) = maximise equality in
     TE.  Only used by ElabDec, rule 19 and 20, and ElabTopdec, rule
     71.  The side condition in rules 19 and 71 demands that TE
     maximises equality.  In the implementation, the maximisation is
     done after the elaboration of the datbind or the datdesc, and
     means that the equality attributes of tynames in TE are changed.
     The result of elaborating a datbind or a datdesc is not only a
     TE, but also a VE, and therefore the realisation must also be
     applied to the VE:*)

    val maximise_equality_in_VE_and_TE : VarEnv * TyEnv -> VarEnv * TyEnv


    (* Restricter to restrict environments; We better share the code
     * used for elaboration environment restriction and compilation
     * environment restriction -- Martin *)

    datatype restricter = Restr of {strids: (strid * restricter) list,
				    vids: id list, tycons: tycon list}
                        | Whole

    val create_restricter : {longstrids: longstrid list, longtycons: longtycon list,
			     longvids: longid list} -> restricter
  end;
