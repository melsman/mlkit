(* Structure identifiers *)

signature STRID =
  sig
    eqtype strid
    eqtype longstrid

    val mk_StrId: string -> strid		(* NEW PARSER *)
    val mk_LongStrId: string list -> longstrid	(* NEW PARSER *)
    val inventStrId: unit -> strid		(* NEW PARSER *)
    val longStrIdOfStrId: strid -> longstrid	(* NEW PARSER *)

    val invented_StrId : strid -> bool

    val implode_longstrid : strid list * strid -> longstrid
    and explode_longstrid : longstrid -> strid list * strid
					(* MEMO: elsewhere we use the
					   name `decompose' for this kind
					   of thing. *)

    val pr_StrId: strid -> string
    val pr_LongStrId: longstrid -> string

   (* Needed for top-level printing: *)
    val < : strid * strid -> bool

    val pu : strid Pickle.pu
    val pu_longstrid : longstrid Pickle.pu
  end
