(* Explicit type variables *)

structure SyntaxTyVar: SYNTAX_TYVAR =
  struct
    datatype SyntaxTyVar = TYVAR of string

    val mk_TyVar = TYVAR
    fun pr_tyvar(TYVAR str) = str

    fun isEquality (TYVAR s) = (case explode s of
				  #"'" :: #"'" :: ss => true
				| _ => false)

    fun lt (TYVAR s1, TYVAR s2) = s1 < s2

    val pu =
	Pickle.convert (TYVAR, fn TYVAR s => s)
	               Pickle.string

    structure Map = OrderFinMap(struct type t = SyntaxTyVar
				       val lt = lt
				end)

  end
