(* type variables - Definition v3 page 4 *)

(*$TYVAR*)
signature TYVAR = 
  sig 
    eqtype SyntaxTyVar

    val mk_TyVar: string -> SyntaxTyVar	(* NEW PARSER *)
    and pr_tyvar: SyntaxTyVar -> string

    val isEquality: SyntaxTyVar -> bool
  end;
