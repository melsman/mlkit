
functor AddressLabels(structure Name : NAME) : ADDRESS_LABELS =
  struct

    (* Address labels are based on names which may be `matched'. In
     * particular, if two address labels, l1 and l2, are successfully
     * matched, eq(l1,l2) = true. This may affect the canonical
     * ordering of address labels. Address labels are used either as
     * code labels or as data labels. *)

    type name = Name.name
    type label = name * string

    fun new () = (Name.new (),"Lab")
    fun new_named name = (Name.new (), name)
    fun key (n,s) = Name.key n

    fun eq(l1,l2) = key l1 = key l2
    fun lt(l1,l2) = key l1 < key l2
    fun pr_label(l,s) = s ^ Int.toString (Name.key l)

    type name = Name.name
    fun name (l,s) = l
    fun match((l1,_),(l2,_)) = Name.match(l1,l2)

    val reg_top_lab = (Name.reg_top, "reg_top")                   (* label 0 *)
    val reg_bot_lab = (Name.reg_bot, "reg_bot")                   (* label 1 *)
    val reg_string_lab = (Name.reg_string, "reg_string")          (* label 2 *)
    val reg_pair_lab = (Name.reg_pair, "reg_pair")                (* label 3 *)
    val reg_array_lab = (Name.reg_array, "reg_array")             (* label 4 *)
    val reg_ref_lab = (Name.reg_ref, "reg_ref")                   (* label 5 *)
    val reg_triple_lab = (Name.reg_triple, "reg_triple")          (* label 6 *)

    val exn_DIV_lab = (Name.exn_DIV, "exn_DIV")                   (* label 7 *)
    val exn_MATCH_lab = (Name.exn_MATCH, "exn_MATCH")             (* label 8 *)
    val exn_BIND_lab = (Name.exn_BIND, "exn_BIND")                (* label 9 *)
    val exn_OVERFLOW_lab = (Name.exn_OVERFLOW, "exn_OVERFLOW")    (* label 10 *)
    val exn_INTERRUPT_lab = (Name.exn_INTERRUPT, "exn_INTERRUPT") (* label 11 *)

    val pu =
	Pickle.register [reg_top_lab,reg_bot_lab,reg_string_lab,
			 reg_pair_lab,reg_array_lab,reg_ref_lab,
			 reg_triple_lab,exn_DIV_lab,exn_MATCH_lab,
			 exn_BIND_lab,exn_OVERFLOW_lab,exn_INTERRUPT_lab]
	let open Pickle
	in newHash (Name.key o #1)
	    (pairGen0(Name.pu,string))
	end
  end
