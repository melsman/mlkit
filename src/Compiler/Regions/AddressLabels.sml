
structure AddressLabels: ADDRESS_LABELS =
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

    fun renew ((_,s1):label) (s2:string) = (Name.new(), s1 ^ "." ^ s2)

    fun eq (l1,l2) = key l1 = key l2
    fun lt (l1,l2) =
	let val (i1,s1) = key l1
	    val (i2,s2) = key l2
	in i1 < i2 orelse (i1=i2 andalso s1 < s2)
	end

    val xx = "abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789"
    fun pr_label (l,s) =
	let val (i,b) = Name.key l
            val k = "$" ^ Int.toString i ^ "$" ^ b
	in s ^ "_" ^ MD5.fromStringP xx k
	end

    type name = Name.name
    fun name (l,s) = l
    fun match ((l1,_),(l2,_)) = Name.match(l1,l2)

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
    val exn_SUBSCRIPT_lab = (Name.exn_SUBSCRIPT, "exn_SUBSCRIPT") (* label 12 *)
    val exn_SIZE_lab = (Name.exn_SIZE, "exn_SIZE")                (* label 13 *)

    val pu =
	Pickle.hashConsEq eq
	(Pickle.register "AddressLabels"
	 [reg_top_lab,reg_bot_lab,reg_string_lab,
	  reg_pair_lab,reg_array_lab,reg_ref_lab,
	  reg_triple_lab,exn_DIV_lab,exn_MATCH_lab,
	  exn_BIND_lab,exn_OVERFLOW_lab,exn_INTERRUPT_lab,
          exn_SUBSCRIPT_lab,exn_SIZE_lab]
	 (Pickle.newHash (#1 o Name.key o #1)
	  (Pickle.pairGen(Name.pu,Pickle.string))))
  end
