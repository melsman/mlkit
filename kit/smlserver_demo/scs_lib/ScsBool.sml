(* $Id$ *)

signature SCS_BOOL = 
  sig
    val opt_p : bool option -> bool
  end

structure ScsBool :> SCS_BOOL =
  struct

    fun opt_p (SOME p) = p
      | opt_p NONE     = false

  end
