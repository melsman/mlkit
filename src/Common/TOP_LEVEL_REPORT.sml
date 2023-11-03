(* Top-level reporting: ties static and dynamic basis together, generates
   a report of bindings. *)

signature TOP_LEVEL_REPORT =
  sig
    type InfixBasis and ElabBasis
    type Report
    type renderer = StrId.strid list * Ident.id * StatObject.TypeScheme -> string

    val report : {infB: InfixBasis, elabB: ElabBasis, render: renderer option} -> Report
  end
