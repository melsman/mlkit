signature BACKEND_INFO =
  sig

    (* Architecture and non architecture backend info *)
    type label
    type lvar
    type phregset
    type phreg = lvar
    type offset = int

    val init_clos_offset   : offset     (* First offset in FN closure is 1 and code pointer is at offset 0 *)
    val init_sclos_offset  : offset     (* First offset in shared closure is 0 *)
    val init_regvec_offset : offset     (* First offset in region vector is 0 *)                              

    val ml_true  : int  (* The representation of true *)
    val ml_false : int  (* The representation of false *)

    val size_of_real     : unit -> int
    val size_of_ref      : unit -> int
    val size_of_record   : 'a list -> int
    val size_of_reg_desc : unit -> int
    val size_of_handle   : unit -> int

    val exn_DIV_lab      : label       (* Global exceptions are globally allocated. *)
    val exn_MATCH_lab    : label
    val exn_BIND_lab     : label
    val exn_OVERFLOW_lab : label

    val toplevel_region_withtype_top_lab    : label
    val toplevel_region_withtype_bot_lab    : label
    val toplevel_region_withtype_string_lab : label
    val toplevel_region_withtype_real_lab   : label

    val is_phreg : lvar -> bool

    val args_phreg : phreg list (* Machine registers containing arguments *)
    val res_phreg  : phreg list (* Machine registers containing results *)

    val args_phreg_ccall : phreg list  (* Machine registers containing arguments in CCALLs *)
    val res_phreg_ccall  : phreg list  (* Machine registers containing results in CCALLs *)

    val callee_save_phregs   : phreg list
    val callee_save_phregset : phregset
    val is_callee_save       : phreg -> bool

    val caller_save_phregs   : phreg list
    val caller_save_phregset : phregset
    val is_caller_save       : phreg -> bool

    val init_frame_offset : offset

    val pr_phreg : phreg -> string
  end








