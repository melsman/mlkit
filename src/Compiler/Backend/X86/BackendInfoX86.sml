functor BackendInfoX86(structure Labels : ADDRESS_LABELS
		       structure Lvars : LVARS
		       structure Lvarset: LVARSET
		       sharing type Lvarset.lvar = Lvars.lvar
		       structure InstsX86 : INSTS_X86
		       sharing type InstsX86.lvar = Lvars.lvar
		       structure PP : PRETTYPRINT
		       structure Flags : FLAGS
		       structure Report : REPORT
		       sharing type Report.Report = Flags.Report
		       structure Crash : CRASH) : BACKEND_INFO =
  struct

    structure I = InstsX86

    (***********)
    (* Logging *)
    (***********)
    fun log s = TextIO.output(!Flags.log,s ^ "\n")
    fun msg s = TextIO.output(TextIO.stdOut, s)
    fun chat(s: string) = if !Flags.chat then msg (s) else ()
    fun die s  = Crash.impossible ("BackendInfo." ^ s)

    type label = Labels.label
    type lvar = Lvars.lvar
    type reg = I.reg
    type lvarset = Lvarset.lvarset
    type offset = int

    val init_clos_offset = 1     (* First offset in FN closure is 1 and code pointer is at offset 0 *) 
    val init_sclos_offset = 0	 (* First offset in shared closure is 0 *)                             
    val init_regvec_offset = 0	 (* First offset in region vector is 0 *)                              

    val ml_true          = 3     (* The representation of true *)
    val ml_false         = 1     (* The representation of false *)
    val ml_unit          = 1     (* The representation of unit *)
    val value_tag_real   = 0     (* Used for constant reals. *)
    val value_tag_string = 1     (* Used for constant strings. *)
    val value_tag_con0   = 2
    val value_tag_con1   = 3
    val value_tag_record = 4
    val value_tag_ref    = 5

    val inf_bit = 1   (* We must add 1 to an address to set the infinite bit. *)
    val atbot_bit = 2 (* We must add 2 to an address to set the atbot bit. *)

    val tag_values = Flags.lookup_flag_entry "tag_values"
    fun size_of_real ()  = if !tag_values then 4 else 2
    fun size_of_ref ()   = if !tag_values then 2 else 1
    fun size_of_record l = if !tag_values then List.length l + 1 else List.length l
    fun size_of_reg_desc() = 3
    fun size_of_handle() = 3

    val exn_DIV_lab      = Labels.new_named("exnDIV")       (* Global exceptions are globally allocated. *)
    val exn_MATCH_lab    = Labels.new_named("exnMATCH")
    val exn_BIND_lab     = Labels.new_named("exnBIND")
    val exn_OVERFLOW_lab = Labels.new_named("exn_OVERFLOW")

    val toplevel_region_withtype_top_lab    = Labels.new_named("reg_top")
    val toplevel_region_withtype_bot_lab    = Labels.new_named("reg_bot")
    val toplevel_region_withtype_string_lab = Labels.new_named("reg_string")
    val toplevel_region_withtype_real_lab   = Labels.new_named("reg_real")

    (* Physical Registers *) 
    fun is_reg lv = I.is_reg lv
    fun lv_to_reg lv = I.lv_to_reg lv
    val args_phreg = I.reg_args_as_lvs
    val res_phreg = I.reg_res_as_lvs
    val args_phreg_ccall = I.reg_args_ccall_as_lvs
    val res_phreg_ccall = I.reg_res_ccall_as_lvs
    val callee_save_phregs   = I.callee_save_regs_mlkit_as_lvs
    val callee_save_phregset = Lvarset.lvarsetof callee_save_phregs
    fun is_callee_save phreg = Lvarset.member(phreg,callee_save_phregset)
    val caller_save_phregs = I.caller_save_regs_mlkit_as_lvs
    val caller_save_phregset = Lvarset.lvarsetof caller_save_phregs
    fun is_caller_save phreg = Lvarset.member(phreg,caller_save_phregset)
    fun pr_reg phreg = I.pr_reg phreg
    fun reg_eq(reg1,reg2) = reg1 = reg2

    val init_frame_offset = 0

  end
