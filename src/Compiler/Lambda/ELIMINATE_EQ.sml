(*$ELIMINATE_EQ: *)

signature ELIMINATE_EQ =
  sig
    type env and lvar and TyName

    val empty : env
    val initial : env
    val plus : env * env -> env

    val restrict : env * {lvars:lvar list,tynames:TyName list} -> lvar list * env
      (* restrict(env,{lvars,tynames}) returns lvars in the range of the restricted tynameenv,
       * together with a restricted environment. *)

    val match : env * env -> env
    val enrich : env * env -> bool 

    type LambdaPgm
    val elim_eq : env * LambdaPgm -> LambdaPgm * env

    type StringTree
    val layout_env : env -> StringTree
  end



    
