
signature INT_MODULES =
  sig
    type IntBasis and topdec and modcode

    type absprjid

    val interp : absprjid * IntBasis * topdec * string -> IntBasis * modcode    
    (* Can effect repository. The string is the string-rep of
     * the funid for the unit. *)

  end
