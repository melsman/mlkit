
signature RTYPE =
sig
  type cone and LambdaType  and effect  and arroweffect and tyname and place and runType and tyvar 
  sharing type place= effect
  sharing type arroweffect = effect

  datatype Type = 
      TYVAR of tyvar
    | CONSTYPE of tyname * (Type*place) list * place list * arroweffect list
    | RECORD of (Type*place) list
    | FUN of (Type*place)list * arroweffect * (Type*place)list 

  val exnType: Type 
  val intType: Type
  val boolType: Type
  val realType: Type
  val stringType: Type
  val unitType: Type

  (* ann_mu(mus)acc is a list of all the places and arrow effects that occur 
   * in mus consed onto acc; word regions are not included in the result. *)
  val ann_mus: (Type * place)list -> effect list -> effect list
  val runtype: Type -> runType

  val freshType: (tyname -> (int*runType list*int)option) (* lookup function *)
                             ->(LambdaType * cone -> Type * cone) 
                             * (LambdaType * cone -> (Type*place) * cone) 
  val unify_ty: Type * Type -> cone -> cone
  val unify_mu: (Type*place) * (Type*place) -> cone -> cone
  val unify_mus: (Type*place)list * (Type*place)list -> cone -> cone

  type sigma and il
  val type_to_scheme: Type -> sigma
  val bv: sigma -> tyvar list * place list * effect list
  val FORALL: tyvar list * place list * effect list * Type -> sigma
  val drop_alphas : sigma -> sigma
  val insert_alphas: tyvar list * sigma -> sigma
  val mk_il: Type list * place list * effect list -> il
  val un_il: il -> Type list * place list * effect list 
  val ann_sigma: sigma -> effect list -> effect list         (* ann_sigma(sigma)acc is a list of all the 
							      * places and arrow effects that occur in 
							      * type of sigma, consed onto acc; word regions
							      * are not included in the result. *)

  val free_puts: sigma -> effect list
  val frv_mu : Type * place -> place list
  val frv_sigma: sigma -> place list   (* free region variables of sigma; word regions not included. *)
  val ferv_sigma: sigma -> effect list  (* free effect and region variables of sigma; word regions not included. *)
  val inst: sigma * il -> cone -> Type * cone
  type delta_phi
  val instClever : sigma * il -> cone -> Type * cone * (effect * delta_phi)list

  val regEffClos: cone * int * effect * Type -> cone * sigma * string option
  val effClos   : cone * int * effect * Type -> cone * sigma * string option
  val generalize_all: cone * int * tyvar list * Type -> cone * sigma * string option
(*
  val generalize_all_and_provide_secondary: cone * int * tyvar list * Type -> cone * sigma * string option
*)
  val alpha_rename: sigma * cone -> sigma
  val alpha_rename': (place list * effect list * Type) * cone -> sigma
  val alpha_equal: sigma * sigma -> cone -> bool
  
  exception FAIL_MATCH of string
  (* matchSchemes and the transformer produced by it can
     raise FAIL_MATCH, if the type schemes do not match or the instantiation
     lists are in disarray; neither should happen, but check for it 
     nonetheless. *)
  val matchSchemes: sigma * sigma -> il * cone -> il * cone
(*                    'a * 'b list * 'b list -> 'a * 'b list * 'b list *)

  (*the following two functions are only used when spreading ccalls (in
   SpreadExpression---see also the comment there):

   sigma_for_c_function tyvars mu B = a region type scheme corresponding to
   the ML type scheme that was freshMu'ed to get mu and has bound tyvars
   `tyvars'.

   c_function_effects mu = the `rhos_for_result' to be annotated on a ccall
   with return type-and-place mu; see comment in MUL_EXP.*)

  val sigma_for_c_function : tyvar list -> (Type * place) -> cone -> sigma * cone
  val c_function_effects : (Type * place) -> (place * int option) list

  type StringTree
  (* the boolean in the following functions should be true iff on want to 
     omit region information *)
  val mk_layout: bool -> (Type -> StringTree) * (Type*place -> StringTree)
  val mk_lay_sigma: bool -> sigma -> StringTree
  val mk_lay_sigma': bool -> (tyvar list * place list * effect list * Type) -> StringTree
  val mk_lay_sigma'': ('b -> StringTree option) -> bool -> 
                      (tyvar list * 'b list * effect list * Type) -> StringTree
end;


