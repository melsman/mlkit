(* Type variables - Definition v3 page ?? *)

(*$TyVar: CRASH TYVAR*)
functor TyVar(structure Crash: CRASH): TYVAR =
  struct
    datatype SyntaxTyVar = TYVAR of string

    val mk_TyVar = TYVAR
    fun pr_tyvar(TYVAR str) = str

    fun isEquality (TYVAR s) = (case explode s of
				  #"'" :: #"'" :: ss => true
				| _ => false)
  end;
