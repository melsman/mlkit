
signature OPT_LAMBDA =
  sig
    type LambdaPgm
    type env and top_env
    type Type
    type tyvar
    type lvar
      
    val topify : env -> top_env
    val initial : top_env
    val empty : env
    val plus : env * env -> env
    val plus' : top_env * top_env -> top_env

    val restrict : top_env * lvar list -> env
    val enrich : top_env * top_env -> bool

    val optimise: env * LambdaPgm -> LambdaPgm * env

    type StringTree
    val layout_env : env -> StringTree
    val layout_top_env : top_env -> StringTree

  end;
