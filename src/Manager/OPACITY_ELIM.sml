
signature OPACITY_ELIM =
  sig
    structure TyName : TYNAME
    structure OpacityEnv : OPACITY_ENV
      sharing OpacityEnv.TyName = TyName

    type realisation    
    type topdec
    type opaq_env = OpacityEnv.opaq_env

    (* Eliminate opaque signature constraints by translating them into
     * transparent signature constraints; this is fine, because, we have
     * already done elaboration at this stage. One can prove that
     * opaque signature constraints only limit what programs
     * elaborate. The translation here also alter type information
     * recorded during elaboration. Martin Elsman 13/10/97 *)

    val opacity_elimination : opaq_env * topdec -> topdec * opaq_env
    (* post elab topdec *)

  end