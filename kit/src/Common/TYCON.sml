(* type constructors - Definition v3 page 4 *)

(*$TYCON*)
signature TYCON =
  sig
    eqtype strid
    eqtype tycon
    eqtype longtycon

    val mk_TyCon: string -> tycon		(* NEW PARSER *)
    val mk_LongTyCon: string list -> longtycon	(* NEW PARSER *)

    val implode_LongTyCon : strid list * tycon -> longtycon
    and explode_LongTyCon : longtycon -> strid list * tycon

    val tycon_INT    : tycon
    val tycon_WORD   : tycon
    and tycon_REAL   : tycon
    and tycon_STRING : tycon
    and tycon_CHAR   : tycon
    and tycon_EXN    : tycon
    and tycon_REF    : tycon
    and tycon_BOOL   : tycon
    and tycon_LIST   : tycon
    and tycon_UNIT   : tycon

    val pr_TyCon : tycon -> string
    val pr_LongTyCon : longtycon  -> string

    val < : tycon * tycon -> bool		(* Needed to order
						   top-level printout. *)
  end;
