signature REG_EXP =
  sig
    exception RegExp of string

    val regExpBool  : string -> string -> bool
    val regExp : string -> string -> string list option

    val pp_mt : string -> string -> string (* For debug only *)
  end
