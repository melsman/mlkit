signature BACKEND_INFO =
  sig

    (* Architecture and non architecture backend info *)
    type label
    type lvar
    type lvarset
    type reg
    type offset = int

    val init_clos_offset   : offset     (* First offset in FN closure is 1 and code pointer is at offset 0 *)
    val init_sclos_offset  : offset     (* First offset in shared closure is 0 *)
    val init_regvec_offset : offset     (* First offset in region vector is 0 *)                              

    val ml_true          : int (* The representation of true *)
    val ml_false         : int (* The representation of false *)
    val ml_unit          : int (* The representation of unit *)
    val value_tag_real   : int (* Used for constant reals. *)
    val value_tag_string : int (* Used for constant strings. *)
    val value_tag_con0   : int
    val value_tag_con1   : int
    val value_tag_record : int
    val value_tag_ref    : int

    val inf_bit          : int (* We must add 1 to an address to set the infinite bit. *)
    val atbot_bit        : int (* We must add 2 to an address to set the atbot bit. *)

    val tag_values       : bool ref
    val tag_integers     : bool ref
    val size_of_real     : unit -> int
    val size_of_ref      : unit -> int
    val size_of_record   : 'a list -> int
    val size_of_reg_desc : unit -> int
    val size_of_handle   : unit -> int

    val init_frame_offset : offset

    val exn_DIV_lab       : label       (* Global exceptions are globally allocated. *)
    val exn_MATCH_lab     : label
    val exn_BIND_lab      : label
    val exn_OVERFLOW_lab  : label
    val exn_INTERRUPT_lab : label

    val toplevel_region_withtype_top_lab    : label
    val toplevel_region_withtype_bot_lab    : label
    val toplevel_region_withtype_string_lab : label
    val toplevel_region_withtype_real_lab   : label

    val is_reg     : lvar -> bool
    val lv_to_reg  : lvar -> reg  (* Die if lvar is not a precolored register *)
    val args_phreg : lvar list (* Machine registers containing arguments *)
    val res_phreg  : lvar list (* Machine registers containing results *)

    val args_phreg_ccall : lvar list  (* Machine registers containing arguments in CCALLs *)
    val res_phreg_ccall  : lvar list  (* Machine registers containing results in CCALLs *)

    val callee_save_phregs   : lvar list
    val callee_save_phregset : lvarset
    val is_callee_save       : lvar -> bool

    val caller_save_phregs   : lvar list
    val caller_save_phregset : lvarset
    val is_caller_save       : lvar -> bool

    val pr_reg : reg -> string
    val reg_eq : reg * reg -> bool

    (* Jump Tables *)
    val minCodeInBinSearch : int
    val maxDiff            : int
    val minJumpTabSize     : int

    (* Names For Primitive Functions *)
    val EQUAL_INT      : string
    val MINUS_INT      : string
    val PLUS_INT       : string
    val MUL_INT        : string
    val NEG_INT        : string
    val ABS_INT        : string
    val LESS_INT       : string
    val LESSEQ_INT     : string
    val GREATER_INT    : string
    val GREATEREQ_INT  : string
    val FRESH_EXN_NAME : string
  end








