
signature OPT_LAMBDA =
  sig
    type LambdaExp
    type LambdaPgm
    type env
    type Type
    type tyvar
    type lvar
    type TyName
    type con

    val enable_arm64_optimisations : {integerArgs:int, floatArgs:int} -> unit

    val initial : env
    val empty : env
    val plus : env * env -> env

    val restrict : env * lvar list * con list * TyName list -> env * lvar list * con list * TyName list
    val enrich : env * env -> bool

    type statenv
    val optimiseWithTypes : statenv * env * LambdaPgm -> LambdaPgm * env
    val optimise: env * LambdaPgm -> LambdaPgm * env

    type StringTree
    val layout_env : env -> StringTree

    val pu : env Pickle.pu
  end;
