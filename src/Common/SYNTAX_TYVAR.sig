(* Explicit type variables *)

signature SYNTAX_TYVAR =
  sig
    eqtype SyntaxTyVar

    val mk_TyVar: string -> SyntaxTyVar
    val pr_tyvar: SyntaxTyVar -> string

    val isEquality: SyntaxTyVar -> bool

    val lt : SyntaxTyVar * SyntaxTyVar -> bool

    val pu : SyntaxTyVar Pickle.pu

    structure Map : MONO_FINMAP where type dom = SyntaxTyVar

  end
