
signature OPT_LAMBDA =
  sig
    type LambdaPgm
    type env
    type Type
    type tyvar
    type lvar
    type TyName
    type con

    val initial : env
    val empty : env
    val plus : env * env -> env

    val restrict : env * lvar list * con list * TyName list -> env * lvar list * con list * TyName list
    val enrich : env * env -> bool

    val optimise: env * LambdaPgm -> LambdaPgm * env

    type StringTree
    val layout_env : env -> StringTree

    val pu : env Pickle.pu
  end;
