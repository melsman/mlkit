(* Top-level reporting: ties static and dynamic basis together, generates
   a report of bindings. *)

(*$TOP_LEVEL_REPORT*)
signature TOP_LEVEL_REPORT =
  sig
    type Basis
    type Report

    val report: {basis: Basis, bindings: bool} -> Report
  end;
