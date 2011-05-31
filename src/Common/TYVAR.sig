(* Explicit type variables *)

signature TYVAR = 
  sig 
    eqtype SyntaxTyVar

    val mk_TyVar: string -> SyntaxTyVar	(* NEW PARSER *)
    and pr_tyvar: SyntaxTyVar -> string

    val isEquality: SyntaxTyVar -> bool

    val pu : SyntaxTyVar Pickle.pu
  end;
