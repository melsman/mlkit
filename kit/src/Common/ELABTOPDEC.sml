(* Elaborate a topdec *)

(*$ELABTOPDEC*)
signature ELABTOPDEC =
  sig
    type StaticBasis

    type PreElabTopdec and PostElabTopdec

    val elab_topdec: StaticBasis * PreElabTopdec -> StaticBasis * PostElabTopdec

    type StringTree
    val layoutStaticBasis: StaticBasis -> StringTree
  end;
