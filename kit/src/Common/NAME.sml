
signature NAME =
  sig

    type name

    (* A name is either flexible or rigid. A flexible name, n, may be
     * matched against another name, n0, which may be either flexible
     * or rigid, and if n0 is marked generative, then n0 and n will
     * thereafter be considered identical (e.g., it holds that key n =
     * key n0). 
     *)

    val key : name -> int * string     (* allow equality only through use of key. *)
    val new : unit -> name
    val eq : name * name -> bool
    val lt : name * name -> bool

    val mk_rigid : name -> unit  (* the key of a rigid name can never change *)
    val rigid : name -> bool

    val mark_gen : name -> unit
    val unmark_gen : name -> unit
    val match : name * name -> unit

    (* match(n,n') matches n to n' if n is flexible and if n' is rigid
     * and is marked generative.  After a succeding match, n is
     * identical with n' and it is unmarked. *)

    val is_gen : name -> bool


    (* New names (introduced with new) are inserted in the bucket. *)

    val bucket : name list ref

      
    (* To support efficient maps and sets based on ordering of name
     * keys, there is a so-called match counter, which increases when
     * a flexible name is successfully matched against another name. *)

    type matchcount                                     
    val current_matchcount : unit -> matchcount          
    val matchcount_lt : matchcount * matchcount -> bool
    val matchcount_invalid : matchcount  (* ~1 for forcing ensurance check of pickled bases *)

    (* Because the runtime system needs to know about the labels
     * of certain kinds of symbols, we predefine some names here *)

    val reg_top : name             (* name 0 *)
    val reg_bot : name             (* name 1 *)
    val reg_string : name          (* name 2 *)
    val reg_pair : name            (* name 3 *)
    val reg_array : name           (* name 4 *)
    val reg_ref : name             (* name 5 *)
    val reg_triple : name          (* name 6 *)

    val exn_DIV : name             (* name 7 *)
    val exn_MATCH : name           (* name 8 *)
    val exn_BIND : name            (* name 9 *)
    val exn_OVERFLOW : name        (* name 10 *)
    val exn_INTERRUPT : name       (* name 11 *)

    val pu : name Pickle.pu
    val pu_matchcount : matchcount Pickle.pu

    val baseSet : string -> unit
    val baseGet : unit -> string
    val assignKey : name * int -> unit (* used by Manager to alpha-rename export bases *)
    val rematching : bool ref
  end
