(*$OPACITY_ELIM: TYNAME *)

signature OPACITY_ELIM =
  sig
    structure TyName : TYNAME

    type realisation    
    type topdec

    val plus : realisation * realisation -> realisation
    val enrich : realisation * (realisation * TyName.Set.Set) -> bool
    val restrict : realisation * TyName.Set.Set -> realisation
    val empty : realisation
    val initial : realisation

    (* Eliminate opaque signature constraints by translating them into
     * transparent signature constraints; this is fine since we have
     * already done elaboration at this stage. One can prove that
     * opaque signature constraints only limit what programs
     * elaborate. The translation here also alter type information
     * recorded during elaboration. Martin Elsman 13/10/97 *)

    val opacity_elimination : realisation * topdec -> topdec * realisation
    (* post elab topdec *)

    type StringTree
    val layout : realisation -> StringTree

  end