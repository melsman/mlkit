(*$BASIS *)

signature BASIS =
  sig
    type InfixBasis and StaticBasis and CompileBasis and Basis
    type ids

    val initialB : Basis
    val emptyB   : Basis

    val Inf_in_B  : InfixBasis   -> Basis
    and Stat_in_B : StaticBasis  -> Basis
    and Comp_in_B : CompileBasis -> Basis

    val Inf_of_B  : Basis -> InfixBasis
    and Stat_of_B : Basis -> StaticBasis
    and Comp_of_B : Basis -> CompileBasis

    val B_plus_B : Basis * Basis -> Basis
    val eq : Basis * Basis -> bool
    val restrict : Basis * ids -> Basis
    val enrich : Basis * Basis -> bool

    type ExpBasis and name

    val mk_ExpBasis : name list * Basis -> ExpBasis
    val de_ExpBasis : ExpBasis -> name list * Basis
    val match : ExpBasis * ExpBasis -> ExpBasis
    val B_plus_Bexp : Basis * ExpBasis -> Basis
    val Bexp_plus_Bexp : ExpBasis * ExpBasis -> ExpBasis

    type StringTree
    val layout_Basis : Basis -> StringTree
    and layout_InfixBasis : InfixBasis -> StringTree
    and layout_StaticBasis : StaticBasis -> StringTree
    and layout_CompileBasis : CompileBasis -> StringTree
  end