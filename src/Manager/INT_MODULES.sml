(*$INT_MODULES*)

signature INT_MODULES =
  sig
    type IntBasis and topdec and modcode
    val interp : IntBasis * topdec * string -> IntBasis * modcode   (* Can effect repository.
								     * The string is the string-rep of
								     * the funid for the unit. *)
    val reset : unit -> unit
    val commit : unit -> unit
  end
