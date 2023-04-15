(* type constructors - Definition (Rev) page 4 *)

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
    val tycon_INT31  : tycon
    val tycon_INT32  : tycon
    val tycon_INT63  : tycon
    val tycon_INT64  : tycon
    val tycon_INTINF : tycon
    val tycon_WORD   : tycon
    val tycon_WORD8  : tycon
    val tycon_WORD31 : tycon
    val tycon_WORD32 : tycon
    val tycon_WORD63 : tycon
    val tycon_WORD64 : tycon
    val tycon_REAL   : tycon
    val tycon_F64    : tycon  (* Internal *)
    val tycon_F256   : tycon  (* Internal *)
    val tycon_STRING : tycon
    val tycon_CHAR   : tycon
    val tycon_EXN    : tycon
    val tycon_REF    : tycon
    val tycon_BOOL   : tycon
    val tycon_LIST   : tycon
    val tycon_FRAG   : tycon
    val tycon_ARRAY  : tycon
    val tycon_VECTOR : tycon
    val tycon_CHARARRAY : tycon
    val tycon_FOREIGNPTR : tycon
    val tycon_UNIT   : tycon

    val pr_TyCon : tycon -> string
    val pr_LongTyCon : longtycon  -> string

    val < : tycon * tycon -> bool		(* Needed to order
						   top-level printout. *)

    (*is_'true'_'nil'_'it'_etc are used to enforce some syntactic
     restrictions (Definition, §2.9 & §3.5).*)

    val is_'true'_'nil'_etc : tycon -> bool
    val is_'it' : tycon -> bool

    val pu : tycon Pickle.pu

    structure Map : MONO_FINMAP where type dom = tycon

  end
