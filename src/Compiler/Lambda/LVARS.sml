(* Lambda variables *)

(*$LVARS*)
signature LVARS =
  sig

    (* Lambda variables are based on names which may be `matched'. In
     * particular, if two lambda variables, lv1 and lv2, are
     * successfully matched, eq(lv1,lv2) = true. This may affect the
     * canonical ordering of lambda variables. *)

    type lvar

    val newLvar : unit -> lvar
    val new_named_lvar : string -> lvar

    val pr_lvar : lvar -> string

    val leq : lvar * lvar -> bool
    val lt : lvar * lvar -> bool
    val eq : lvar * lvar -> bool
    val key : lvar -> int               (* for sorting etc *)
    val is_free : lvar -> bool ref      (* mark for computing sets of free variables *)
    val is_inserted : lvar -> bool ref  (* mark for computing sets of free variables *)

    val reset_use : lvar -> unit
    val incr_use : lvar -> unit
    val decr_use : lvar -> unit
    val zero_use : lvar -> bool
    val one_use : lvar -> bool

    (* Names *)
    type name
    val match : lvar * lvar -> unit
    val name : lvar -> name

    (* ------------------------------------------------------------------
     * Compiler-supported primitives; these are only here to allow for
     * overloading resolution. In fact, I think that we can get rid of
     * them altogether by resolving overloaded identifiers to
     * ``ccalls''. Then, the backend can grab those ``ccalls'' and
     * generate appropriate assembly code instead of calls.
     * ------------------------------------------------------------------ *)

    val plus_int_lvar : lvar           (* integer operations *)
    val minus_int_lvar : lvar
    val mul_int_lvar : lvar
    val negint_lvar : lvar
    val absint_lvar : lvar
    val less_int_lvar : lvar
    val lesseq_int_lvar : lvar
    val greater_int_lvar : lvar
    val greatereq_int_lvar : lvar

    val plus_float_lvar : lvar         (* real operations *)
    val minus_float_lvar : lvar
    val mul_float_lvar : lvar
    val negfloat_lvar : lvar
    val absfloat_lvar : lvar
    val less_float_lvar : lvar
    val greater_float_lvar : lvar
    val lesseq_float_lvar : lvar
    val greatereq_float_lvar : lvar


    (* For pattern-mathing, we declare a datatype for
     * compiler-supported primitives and a function 
     * primitive: lvar -> primitive option *)

    datatype primitive = PLUS_INT | MINUS_INT | MUL_INT | DIV_INT | NEG_INT | ABS_INT
                       | LESS_INT | LESSEQ_INT | GREATER_INT | GREATEREQ_INT
                       | PLUS_FLOAT | MINUS_FLOAT | MUL_FLOAT | DIV_FLOAT | NEG_FLOAT | ABS_FLOAT
                       | LESS_FLOAT | LESSEQ_FLOAT | GREATER_FLOAT | GREATEREQ_FLOAT

    val primitive : lvar -> primitive option

  end;


(*$LVARSET*)

(***********************************************************************
  Applicative representation of finite sets of naturals, 1993-01-03
  sestoft@dina.kvl.dk
***********************************************************************)

signature LVARSET = 
    sig 
	type lvar			(* = Lvars.lvar *)
	type lvarset			(* set of lvar  *)
	val empty        : lvarset
	val singleton    : lvar -> lvarset
	val union        : lvarset * lvarset -> lvarset
	val add          : lvarset * lvar -> lvarset
	val intersection : lvarset * lvarset -> lvarset
	val member       : lvar * lvarset -> bool
	val difference   : lvarset * lvarset -> lvarset
	val delete       : lvarset * lvar -> lvarset
	val disjoint     : lvarset * lvarset -> bool
	val lvarsetof    : lvar list -> lvarset

	val members      : lvarset -> lvar list		
	val foldset      : ('a * lvar -> 'a) -> 'a * lvarset -> 'a
	val mapset       : (lvar -> 'a) -> lvarset -> 'a list
        val findLvar     : (lvar -> '_a option) -> lvarset -> (lvar * '_a)option
    end
