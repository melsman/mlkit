
signature NAME =
  sig

    type name

    (* A name is either flexible or rigid. A flexible name, n, may be
     * matched against another name, n0, which may be either flexible
     * or rigid, and if n0 is marked generative, then n0 and n will
     * thereafter be considered identical (e.g., it holds that key n =
     * key n0). 
     *)

    val key : name -> int     (* allow equality only through use of key. *)
    val new : unit -> name

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

    val reset : unit -> unit
    val commit : unit -> unit

    (* Operations on lists of elements that have names as components *) 
(*
    val union : ('a -> name) -> 'a list * 'a list -> 'a list
    val diff : ('a -> name) -> 'a list * 'a list -> 'a list
    val disjoint : ('a -> name) -> 'a list * 'a list -> bool
    val elim_dubs : ('a -> name) -> 'a list -> 'a list
*)

  end