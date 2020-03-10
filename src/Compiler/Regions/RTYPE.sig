
signature RTYPE =
sig
  type LambdaType
  type tyvar
  type tyname

  type cone
  type effect
  type arroweffect = effect
  type place = effect
  type runType

  type Type
  type mu = Type * place

  val mkTYVAR    : tyvar -> Type
  val mkCONSTYPE : tyname * mu list * place list * arroweffect list -> Type
  val mkRECORD   : mu list -> Type
  val mkFUN      : mu list * arroweffect * mu list -> Type

  val unTYVAR    : Type -> tyvar option
  val unCONSTYPE : Type -> (tyname * mu list * place list * arroweffect list) option
  val unRECORD   : Type -> mu list option
  val unFUN      : Type -> (mu list * arroweffect * mu list) option

  val exnType    : Type
  val int31Type  : Type
  val int32Type  : Type
  val word8Type  : Type
  val word31Type : Type
  val word32Type : Type
  val boolType   : Type
  val realType   : Type
  val f64Type    : Type
  val stringType : Type
  val unitType   : Type

  val unboxed    : Type -> bool
  val runtype    : Type -> runType

  val isF64Type  : Type -> bool

  (* ann_mu(mus)acc is a list of all the places and arrow effects that occur
   * in mus consed onto acc; word regions are not included in the result. *)
  val ann_mus    : mu list -> effect list -> effect list

  val freshType  : (tyname -> (int*runType list*int)option) (* lookup function *)
                   -> (LambdaType * cone -> Type * cone)
                      * (LambdaType * cone -> mu * cone)
  val unify_ty   : Type * Type -> cone -> cone
  val unify_mu   : mu * mu -> cone -> cone
  val unify_mus  : mu list * mu list -> cone -> cone

  type sigma and il
  val type_to_scheme : Type -> sigma
  val bv             : sigma -> tyvar list * place list * effect list
  val FORALL         : tyvar list * place list * effect list * Type -> sigma
  val drop_alphas    : sigma -> sigma
  val insert_alphas  : tyvar list * sigma -> sigma
  val mk_il          : Type list * place list * effect list -> il
  val un_il          : il -> Type list * place list * effect list
  val ann_sigma      : sigma -> effect list -> effect list   (* ann_sigma(sigma)acc is a list of all the
							      * places and arrow effects that occur in
							      * type of sigma, consed onto acc; word regions
							      * are not included in the result. *)
  val free_puts   : sigma -> effect list
  val frv_mu      : mu -> place list
  val frv_sigma   : sigma -> place list   (* free region variables of sigma; word regions not included. *)
  val ferv_sigma  : sigma -> effect list  (* free effect and region variables of sigma; word regions not included. *)
  val inst        : sigma * il -> cone -> Type * cone

  type delta_phi
  val instClever  : sigma * il -> cone -> Type * cone * (effect * delta_phi)list

  val regEffClos     : cone * int * effect * Type -> cone * sigma
  val generalize_all : cone * int * tyvar list * Type -> cone * sigma

  val alpha_rename   : sigma * cone -> sigma
  val alpha_equal    : sigma * sigma -> cone -> bool

  (* matchSchemes and the transformer produced by it can
   * raise FAIL_MATCH, if the type schemes do not match or the instantiation
   * lists are in disarray; neither should happen, but check for it
   * nonetheless. *)
  exception FAIL_MATCH of string
  val matchSchemes  : sigma * sigma -> il * cone -> il * cone

  (* The following two functions are only used when spreading ccalls (in
   * SpreadExpression---see also the comment there):
   *
   * [sigma_for_c_function tyvars mu B] returns a region type scheme
   * corresponding to the ML type scheme that was freshMu'ed to get mu
   * and has bound tyvars tyvars.
   *
   * [c_function_effects mu] returns the `rhos_for_result' to be
   * annotated on a ccall with return type-and-place mu; see comment
   * in MUL_EXP.
   *)

  val sigma_for_c_function : tyvar list -> mu -> cone -> sigma * cone
  val c_function_effects   : sigma * mu -> (place * int option) list

  type StringTree
  (* the boolean in the following functions should be true iff on want to
     omit region information *)
  val mk_layout      : bool -> (Type -> StringTree) * (mu -> StringTree)
  val mk_lay_sigma   : bool -> sigma -> StringTree
  val mk_lay_sigma'  : bool -> (tyvar list * place list * effect list * Type) -> StringTree
  val mk_lay_sigma'' : ('b -> StringTree option) -> bool ->
                         (tyvar list * 'b list * effect list * Type) -> StringTree

  (* Picklers *)
  val pu_Type  : Type Pickle.pu
  val pu_mu    : mu Pickle.pu
  val pu_sigma : sigma Pickle.pu
end
