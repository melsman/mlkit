functor BackendInfo(structure Labels : ADDRESS_LABELS
		    structure NatSet: KIT_MONO_SET where type elt = word
		    structure PP : PRETTYPRINT
		    structure Flags : FLAGS
		    structure Report : REPORT
		    sharing type Report.Report = Flags.Report
		    structure Crash : CRASH) : BACKEND_INFO =
  struct

    type label = Labels.label
    type phreg = word
    type offset = int

    val init_clos_offset = 1     (* First offset in FN closure is 1 and code pointer is at offset 0 *) 
    val init_sclos_offset = 0	 (* First offset in shared closure is 0 *)                             
    val init_regvec_offset = 0	 (* First offset in region vector is 0 *)                              

    val ml_true = 1  (* The representation of true *)
    val ml_false = 0 (* The representation of false *)

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

    val args_phreg = [0w1,0w2,0w3,0w4,0w5,0w6,0w7,0w8,0w9,0w10] (* Machine registers containing arguments *)
    val res_phreg = [0w10,0w9,0w8,0w7,0w6,0w5,0w4,0w3,0w2,0w1]  (* Machine registers containing results *)

    val args_phreg_ccall = [0w1,0w2,0w3,0w4] (* Machine registers containing arguments in CCALLs *)
    val res_phreg_ccall = [0w4,0w3,0w2,0w1]  (* Machine registers containing results in CCALLs *)

    val callee_save_phregs = [0w7,0w8,0w9,0w10]
    val callee_save_phregs_natset = NatSet.fromList callee_save_phregs
    fun is_callee_save phreg = NatSet.member phreg callee_save_phregs_natset

    val caller_save_phregs = [0w1,0w2,0w3,0w4]
    val caller_save_phregs_natset = NatSet.fromList caller_save_phregs
    fun is_caller_save phreg = NatSet.member phreg caller_save_phregs_natset

    val init_frame_offset = 0


  end
