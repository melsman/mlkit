
signature NAME =
  sig

    (* Names may be marked generative. If two names are marked
     * generative and then matched, they are considered equal. When
     * two names are successfully matched, they are unmarked.
     * 
     * New names (introduced with new) are inserted in the bucket.
     *)

    type name
      (* only allow equality through use of key. *)

    val key : name -> int
    val new : unit -> name

    val mark_gen : name -> unit
    val unmark_gen : name -> unit
    val match : name * name -> unit
      (* match(n,n')  matches n to n' if both n and n' are
       * marked generative. After a succeding match, n and n' 
       * are unmarked. *)

    val is_gen : name -> bool

    val bucket : name list ref

    val reset : unit -> unit
    val commit : unit -> unit
  end