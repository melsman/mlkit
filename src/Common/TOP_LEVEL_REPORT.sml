(* Top-level reporting: ties static and dynamic basis together, generates
   a report of bindings. *)

(*$TOP_LEVEL_REPORT*)
signature TOP_LEVEL_REPORT =
  sig
    type InfixBasis and ElabBasis
    type Report

    val report: {infB: InfixBasis, elabB: ElabBasis, bindings: bool} -> Report
  end;
