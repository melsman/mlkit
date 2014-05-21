(* Top-level reporting: ties static and dynamic basis together, generates
   a report of bindings. *)

signature TOP_LEVEL_REPORT =
  sig
    type InfixBasis and ElabBasis
    type Report

    val report: {infB: InfixBasis, elabB: ElabBasis, bindings: bool} -> Report
  end;
