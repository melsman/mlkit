(* Elaborate a topdec *)

signature ELABTOPDEC =
  sig
    type StaticBasis

    type PreElabTopdec and PostElabTopdec

    type absprjid  (* absolute project identifier *)

    val elab_topdec: absprjid * StaticBasis * PreElabTopdec 
                     -> StaticBasis * PostElabTopdec

    type StringTree
    val layoutStaticBasis: StaticBasis -> StringTree
  end;
