
(*$LAMBDA_STAT_SEM: *)

signature LAMBDA_STAT_SEM =
  sig
    type LambdaPgm
    type env
    type lvar and TyName and con and excon

    val initial : env
    val empty : env
    val plus : env * env -> env
    val enrich : env * env -> bool
    val restrict : env * {lvars:lvar list,
			  tynames:TyName list,
			  cons: con list,
			  excons: excon list} -> env

    val type_check : {env: env, 
		      pgm: LambdaPgm,
		      letrec_polymorphism_only: bool} -> env

    type StringTree
    val layout_env : env -> StringTree

  end