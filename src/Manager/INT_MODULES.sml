
signature INT_MODULES =
  sig
    type IntBasis and topdec and modcode

    type prjid = string

    val interp : prjid * IntBasis * topdec * string -> IntBasis * modcode   (* Can effect repository.
									     * The string is the string-rep of
									     * the funid for the unit. *)
  end
