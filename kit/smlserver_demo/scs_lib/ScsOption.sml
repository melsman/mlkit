(* $Id$ *)

signature SCS_OPTION =
  sig
    val isNONE : 'a option -> bool
  end

structure ScsOption :> SCS_OPTION =
  struct
    val isNONE = not o Option.isSome
  end
