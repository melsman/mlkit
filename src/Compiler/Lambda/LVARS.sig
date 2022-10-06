(* Lambda variables *)

signature LVARS =
  sig

    (* Lambda variables are based on names which may be `matched'. In
     * particular, if two lambda variables, lv1 and lv2, are
     * successfully matched, eq(lv1,lv2) = true. This may affect the
     * canonical ordering of lambda variables. *)

    type lvar

    val newLvar : unit -> lvar
    val new_named_lvar : string -> lvar
    val renew : lvar -> lvar

    val pr_lvar : lvar -> string
    val pr_lvar' : lvar -> string       (* with key *)

    val leq : lvar * lvar -> bool
    val lt : lvar * lvar -> bool
    val eq : lvar * lvar -> bool
    val key : lvar -> int * string      (* for sorting etc ; the string is the base name (i.e., program unit name) *)
    val is_free : lvar -> bool ref      (* mark for computing sets of free variables *)
    val is_inserted : lvar -> bool ref  (* mark for computing sets of free variables *)

    val reset_use : lvar -> unit
    val incr_use : lvar -> unit
    val decr_use : lvar -> unit
    val zero_use : lvar -> bool
    val one_use : lvar -> bool

    val set_ubf64 : lvar -> unit
    val get_ubf64 : lvar -> bool

    val str : lvar -> string           (* access to internal string representation *)

    (* Names *)
    type name
    val match : lvar * lvar -> unit
    val name : lvar -> name

    val pu : lvar Pickle.pu

    structure Map : MONO_FINMAP where type dom = lvar

  end


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
	val appset       : (lvar -> unit) -> lvarset -> unit
        val findLvar     : (lvar -> '_a option) -> lvarset -> (lvar * '_a)option
        val one_in_difference : lvarset * lvarset -> lvar option
    end
