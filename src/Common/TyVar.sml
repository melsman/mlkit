(* Explicit type variables *)

functor TyVar(structure Crash: CRASH): TYVAR =
  struct
    datatype SyntaxTyVar = TYVAR of string

    val mk_TyVar = TYVAR
    fun pr_tyvar(TYVAR str) = str

    fun isEquality (TYVAR s) = (case explode s of
				  #"'" :: #"'" :: ss => true
				| _ => false)

    val pu = 
	Pickle.convert (TYVAR, fn TYVAR s => s) 
	Pickle.string
  end
