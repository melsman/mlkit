(* Elaborate a topdec *)

(*$ELABTOPDEC*)
signature ELABTOPDEC =
  sig
    type StaticBasis

    type PreElabTopdec and PostElabTopdec

    type prjid (*= string*)

    val elab_topdec: prjid * StaticBasis * PreElabTopdec -> StaticBasis * PostElabTopdec

    type StringTree
    val layoutStaticBasis: StaticBasis -> StringTree
  end;
