
signature LAMBDA_BASICS =
  sig

    type LambdaExp and LambdaPgm

    (* generic lambda expression traversers *)
    val passTD   : (LambdaExp -> LambdaExp) -> LambdaExp -> LambdaExp
    val passBU   : (LambdaExp -> LambdaExp) -> LambdaExp -> LambdaExp
    val foldTD   : ('a -> LambdaExp -> 'a) -> 'a -> LambdaExp -> 'a
    val map_lamb : (LambdaExp -> LambdaExp) -> LambdaExp -> LambdaExp  (* map on subexpressions *)
    val app_lamb : (LambdaExp -> unit) -> LambdaExp -> unit            (* app to subexpressions *)

    datatype tail = TAIL | NOTAIL
    val app_lamb_tail : (tail -> LambdaExp -> unit) -> tail -> LambdaExp -> unit (* app to subexpressions *)

    (* generate a new instance of a lambda expression; rename bound variables *)
    val new_instance : LambdaExp -> LambdaExp

    (* find exported identifiers of a lambda expression *)
    type excon and lvar
    val exports : LambdaExp -> lvar list * excon list

    (* Annotate tail call on APP constructs *)
    val annotate_tail_calls : LambdaExp -> LambdaExp

    (* substitutions *)
    exception TypeVariableCapture

    type subst and tyvar and Type
    val mk_subst     : (unit -> string) -> tyvar list * Type list -> subst
    val on_Type      : subst -> Type -> Type
    val on_LambdaExp : subst -> LambdaExp -> LambdaExp

    val contains_f64Type  : Type -> bool
    val contains_f64Types : Type list -> bool

    (* Equality of types, but disregarding regvar information *)
    val eq_Type          : Type * Type -> bool
    val eq_Types         : Type list * Type list -> bool
    val eq_sigma         : (tyvar list * Type) * (tyvar list * Type) -> bool
    val eq_sigma_with_il : (tyvar list * Type * Type list) *
                           (tyvar list * Type * Type list) -> bool    (* see COMPILER_ENV*)

    val match_sigma      : (tyvar list * Type) * Type -> subst

    val freevars                    : LambdaExp -> lvar list * excon list
    val freevars_not_call_invariant : (lvar*lvar list) -> LambdaExp -> lvar list * excon list

    val close : LambdaPgm -> LambdaPgm


    (* Normalization of type schemes so that type schemes only bind type
     * variables that occur in the body of the type scheme. CompileDec.sml
     * doesn't ensure this property, so we fix the LambdaExp terms after
     * compilation into LambdaExp; see CompileToLamb.sml *)

    structure Normalize : sig
      type env and StringTree
      val plus     : env * env -> env
      val empty    : env
      val initial  : env
      val restrict : env * lvar list -> env
      val enrich   : env * env -> bool
      val layout   : env -> StringTree
      val pu       : env Pickle.pu
      val norm     : env * LambdaPgm -> LambdaPgm * env
    end
  end
