(*$LAMBDA_BASICS: *)
signature LAMBDA_BASICS =
  sig

    type LambdaExp

    (* generic lambda expression traversers *)
    val passTD : (LambdaExp -> LambdaExp) -> LambdaExp -> LambdaExp
    val passBU : (LambdaExp -> LambdaExp) -> LambdaExp -> LambdaExp
    val foldTD : ('a -> LambdaExp -> 'a) -> 'a -> LambdaExp -> 'a
    val map_lamb : (LambdaExp -> LambdaExp) -> LambdaExp -> LambdaExp  (* map on subexpressions *)
    val app_lamb : (LambdaExp -> unit) -> LambdaExp -> unit            (* app to subexpressions *)

    (* generate a new instance of a lambda expression; rename bound variables *)
    val new_instance : LambdaExp -> LambdaExp


    (* find exported identifiers of a lambda expression *)
    type excon and lvar
    val exports : LambdaExp -> lvar list * excon list


    (* substitutions *)
    exception TypeVariableCapture

    type subst and tyvar and Type
    val mk_subst : string -> tyvar list * Type list -> subst
    val on_Type : subst -> Type -> Type
    val on_LambdaExp : subst -> LambdaExp -> LambdaExp

    val eq_Type : Type * Type -> bool
    val eq_Types : Type list * Type list -> bool
    val eq_sigma : (tyvar list * Type) * (tyvar list * Type) -> bool
    val eq_sigma_with_il : (tyvar list * Type * Type list) * 
                           (tyvar list * Type * Type list) -> bool    (* see COMPILER_ENV*)

    val match_sigma : (tyvar list * Type) * Type -> subst

  end