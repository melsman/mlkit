
signature ELIMINATE_EQ =
  sig
    type env and top_env and lvar and TyName

    val empty : env
    val topify : env -> top_env
    val initial : top_env
    val plus : env * env -> env
    val plus' : top_env * top_env -> top_env

    val restrict : top_env * {lvars:lvar list,tynames:TyName list} -> lvar list * env
      (* restrict(env,{lvars,tynames}) returns lvars in the range of the restricted tynameenv,
       * together with a restricted environment. *)

    val match : env * top_env -> env
    val enrich : top_env * top_env -> bool 

    type LambdaPgm
    val elim_eq : env * LambdaPgm -> LambdaPgm * env

    type StringTree
    val layout_env : env -> StringTree
    val layout_top_env : top_env -> StringTree
  end



    
