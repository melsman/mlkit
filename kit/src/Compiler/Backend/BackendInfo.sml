functor BackendInfo(structure Labels : ADDRESS_LABELS
		    structure Lvars : LVARS
	            structure Lvarset: LVARSET
		      sharing type Lvarset.lvar = Lvars.lvar
		    structure PP : PRETTYPRINT
		    structure Flags : FLAGS
		    structure Report : REPORT
		    sharing type Report.Report = Flags.Report
		    structure Crash : CRASH) : BACKEND_INFO =
  struct

    type label = Labels.label
    type lvar = Lvars.lvar
    type phreg = lvar
    type phregset = Lvarset.lvarset
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

    val vector_of_phregs = Vector.tabulate(32,fn i => Lvars.new_named_lvar ("phreg"^Int.toString i))
    val phregs = Vector.foldr (fn (phreg,phreg_list) => phreg::phreg_list) [] vector_of_phregs
    val phregset = Lvarset.lvarsetof phregs
    fun is_phreg lvar = Lvarset.member(lvar,phregset)
    fun get_phreg i = Vector.sub(vector_of_phregs,i)
    fun pr_phreg phreg = Lvars.pr_lvar phreg

    val args_phreg = map (fn i => get_phreg i) [1,2,3,4,5,6,7,8,9,10] (* Machine registers containing arguments *)
    val res_phreg  = map (fn i => get_phreg i) [10,9,8,7,6,5,4,3,2,1] (* Machine registers containing results *)

    val args_phreg_ccall = map (fn i => get_phreg i) [1,2,3,4] (* Machine registers containing arguments in CCALLs *)
    val res_phreg_ccall  = map (fn i => get_phreg i) [4,3,2,1] (* Machine registers containing results in CCALLs *)

    val callee_save_phregs = map (fn i => get_phreg i) [7,8,9,10]
    val callee_save_phregset = Lvarset.lvarsetof callee_save_phregs
    fun is_callee_save phreg = Lvarset.member(phreg,callee_save_phregset)

    val caller_save_phregs = map (fn i => get_phreg i) [1,2,3,4]
    val caller_save_phregset = Lvarset.lvarsetof caller_save_phregs
    fun is_caller_save phreg = Lvarset.member(phreg,caller_save_phregset)

    val init_frame_offset = 0
  end
