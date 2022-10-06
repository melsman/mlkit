signature REGISTER_INFO =
  sig
    eqtype reg
    type lvar

    val is_reg     : lvar -> bool
    val lv_to_reg  : lvar -> reg  (* Die if lvar is not a precolored register *)
    val args_phreg : lvar list    (* Machine registers containing arguments *)
    val res_phreg  : lvar list    (* Machine registers containing results *)

    val args_phfreg : lvar list    (* Machine registers containing float arguments *)

    val all_regs             : lvar list      (* All general-purpose registers *)

    val caller_save_phregs   : lvar list
    val is_caller_save       : lvar -> bool

    val f64_phregs           : lvar list   (* floating point registers available for
                                              register allocation (excluding two tmp
                                              registers) *)

    (* CCALLs *)
    val args_reg_ccall             : reg list   (* Machine registers containing arguments in CCALLs *)
    val args_phreg_ccall           : lvar list  (* Machine registers containing arguments in CCALLs *)
    val res_phreg_ccall            : lvar list  (* Machine registers containing results in CCALLs *)
    val callee_save_ccall_phregs   : lvar list
    val is_callee_save_ccall       : lvar -> bool

    val pr_reg : reg -> string
    val reg_eq : reg * reg -> bool
  end
