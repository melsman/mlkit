
signature ADDRESS_LABELS =
  sig

    (* Address labels are based on names which may be `matched'. In
     * particular, if two address labels, l1 and l2, are successfully
     * matched, eq(l1,l2) = true. This may affect the canonical
     * ordering of address labels. Address labels are used either as
     * code labels or as data labels. *)

    type label

    val new       : unit -> label
    val new_named : string -> label
    val eq        : label * label -> bool
    val lt        : label * label -> bool (* Used locally by ClosExp *)
    val key       : label -> int
    val pr_label  : label -> string

    type name
    val name  : label -> name
    val match : label * label -> unit

    val reg_top_lab : label             (* label 0 *)
    val reg_bot_lab : label             (* label 1 *)
    val reg_string_lab : label          (* label 2 *)
    val reg_pair_lab : label            (* label 3 *)

    val exn_DIV_lab : label             (* label 4 *)
    val exn_MATCH_lab : label           (* label 5 *)
    val exn_BIND_lab : label            (* label 6 *)
    val exn_OVERFLOW_lab : label        (* label 7 *)
    val exn_INTERRUPT_lab : label       (* label 8 *)

  end


