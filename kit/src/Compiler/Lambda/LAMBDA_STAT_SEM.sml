
signature LAMBDA_STAT_SEM =
  sig
    type LambdaPgm
    type env and top_env
    type lvar and TyName and con and excon

    val initial : top_env
    val empty : env
    val topify : env -> top_env
    val plus : env * env -> env
    val plus' : top_env * top_env -> top_env
    val enrich : top_env * top_env -> bool
    val restrict : top_env * {lvars:lvar list,
			      tynames:TyName list,
			      cons: con list,
			      excons: excon list} -> env

    val type_check : {env: env, 
		      pgm: LambdaPgm,
		      letrec_polymorphism_only: bool} -> env

    type StringTree
    val layout_env : env -> StringTree
    val layout_top_env : top_env -> StringTree
  end