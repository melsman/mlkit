functor BackendInfo(structure Labels : ADDRESS_LABELS
		    structure Lvars : LVARS
	            structure Lvarset: LVARSET
		      sharing type Lvarset.lvar = Lvars.lvar
		    structure HpPaRisc : HP_PA_RISC
		      sharing type HpPaRisc.lvar = Lvars.lvar
		    structure PP : PRETTYPRINT
		    structure Flags : FLAGS
		    structure Report : REPORT
		    sharing type Report.Report = Flags.Report
		    structure Crash : CRASH) : BACKEND_INFO =
  struct

    (***********)
    (* Logging *)
    (***********)
    fun log s = TextIO.output(!Flags.log,s ^ "\n")
    fun msg s = TextIO.output(TextIO.stdOut, s)
    fun chat(s: string) = if !Flags.chat then msg (s) else ()
    fun die s  = Crash.impossible ("BackendInfo." ^ s)

    type label = Labels.label
    type lvar = Lvars.lvar
    type reg = HpPaRisc.reg
    type lvarset = Lvarset.lvarset
    type offset = int

    val init_clos_offset = 1     (* First offset in FN closure is 1 and code pointer is at offset 0 *) 
    val init_sclos_offset = 0	 (* First offset in shared closure is 0 *)                             
    val init_regvec_offset = 0	 (* First offset in region vector is 0 *)                              

    (******************************)
    (* Runtime System Information *)
    (******************************)
    val pOff  = 0 (* Offset for previous region pointer (p) in a region descriptor. *)
    val aOff  = 1 (* Offset for allocation pointer (a) in a region descriptor. *)
    val bOff  = 2 (* Offset for border pointer (b) in a region descriptor. *)
    val fpOff = 3 (* Offset for first region page pointer (fp) in a region descriptor. *)

    val regionPageTotalSize = 254 (*ALLOCATABLE_WORDS_IN_REGION_PAGE*) + 2 (*HEADER_WORDS_IN_REGION_PAGE*)
    val regionPageHeaderSize = 2 (*HEADER_WORDS_IN_REGION_PAGE*)

    (***********)
    (* Tagging *)
    (***********)

    fun pr_tag_w tag = "0X" ^ (Word32.fmt StringCvt.HEX tag)
    (* For now, some tags are in integers but it should be eliminated; max size is then 2047 only 09/01/1999, Niels *)
    fun pr_tag_i tag = "0X" ^ (Int.fmt StringCvt.HEX tag)

    fun gen_record_tag(s:int,off:int,i:bool,t:int) = 
      let
	fun pw(s,w) = print (s ^ " is " ^ (Word32.fmt StringCvt.BIN w) ^ "\n")
	val w0 = Word32.fromInt 0
	val size = Word32.fromInt s
	val offset = Word32.fromInt off
	val immovable = if i = true then Word32.fromInt 1 else Word32.fromInt 0
	val tag = Word32.fromInt t
	fun or_bits(w1,w2) = Word32.orb(w1,w2)
	fun shift_left(num_bits,w) = Word32.<<(w,Word.fromInt num_bits)
	val w_size = shift_left(19,size)
	val w_offset = or_bits(w_size,shift_left(6,offset))
	val w_immovable = or_bits(w_offset,shift_left(5,immovable))
	val w_tag = or_bits(w_immovable,tag)
      in
	w_tag
      end

    fun gen_string_tag(s:int,i:bool,t:int) = 
      let
	fun pw(s,w) = print (s ^ " is " ^ (Word32.fmt StringCvt.BIN w) ^ "\n")
	val w0 = Word32.fromInt 0
	val size = Word32.fromInt s
	val immovable = if i = true then Word32.fromInt 1 else Word32.fromInt 0
	val tag = Word32.fromInt t
	fun or_bits(w1,w2) = Word32.orb(w1,w2)
	fun shift_left(num_bits,w) = Word32.<<(w,Word.fromInt num_bits)
	val w_size = shift_left(6,size)
	val w_immovable = or_bits(w_size,shift_left(5,immovable))
	val w_tag = or_bits(w_immovable,tag)
      in
	w_tag
      end

    val ml_true          = 3     (* The representation of true *)
    val ml_false         = 1     (* The representation of false *)
    val ml_unit          = 1     (* The representation of unit *)

    fun tag_real(i:bool)              = gen_record_tag(3,3,i,6)
    fun tag_string(i:bool,size)       = gen_string_tag(size,i,1)
    fun tag_record(i:bool,size)       = gen_record_tag(size,0,i,6)
    fun tag_con0(i:bool,c_tag)        = gen_string_tag(c_tag,i,2)
    fun tag_con1(i:bool,c_tag)        = gen_string_tag(c_tag,i,3)
    fun tag_ref(i:bool)               = gen_string_tag(0,i,5)
    fun tag_clos(i:bool,size,n_skip)  = gen_record_tag(size,n_skip,i,6)
    fun tag_sclos(i:bool,size,n_skip) = gen_record_tag(size,n_skip,i,6)
    fun tag_regvec(i:bool,size)       = gen_record_tag(size,size,i,6)
    fun tag_table(i:bool,size)        = gen_string_tag(size,i,7)
    fun tag_exname(i:bool)            = gen_record_tag(2,2,i,6)
    fun tag_excon0(i:bool)            = gen_record_tag(1,0,i,6)
    fun tag_excon1(i:bool)            = gen_record_tag(2,0,i,6)
    val tag_ignore                    = Word32.fromInt 0

    val inf_bit = 1   (* We add 1 to an address to set the infinite bit. *)
    val atbot_bit = 2 (* We add 2 to an address to set the atbot bit. *)

    val tag_values      = Flags.lookup_flag_entry "tag_values"
    val tag_integers    = Flags.lookup_flag_entry "tag_integers"
    val unbox_datatypes = Flags.lookup_flag_entry "unbox_datatypes"

    fun size_of_real ()    = if !tag_values then 4 else 2
    fun size_of_ref ()     = if !tag_values then 2 else 1
    fun size_of_record l   = if !tag_values then List.length l + 1 else List.length l
    fun size_of_reg_desc() = 4
    fun size_of_handle()   = 4

    val exn_DIV_lab       = Labels.new_named("exnDIV_lab")       (* Global exceptions are globally allocated. *)
    val exn_MATCH_lab     = Labels.new_named("exnMATCH_lab")
    val exn_BIND_lab      = Labels.new_named("exnBIND_lab")
    val exn_OVERFLOW_lab  = Labels.new_named("exn_OVERFLOW_lab")
    val exn_INTERRUPT_lab = Labels.new_named("exn_INTERRUPT_lab")

    val toplevel_region_withtype_top_lab    = Labels.new_named("reg_top")
    val toplevel_region_withtype_bot_lab    = Labels.new_named("reg_bot")
    val toplevel_region_withtype_string_lab = Labels.new_named("reg_string")
    val toplevel_region_withtype_real_lab   = Labels.new_named("reg_real")

    (* Physical Registers *) 
    fun is_reg lv = HpPaRisc.is_reg lv
    fun lv_to_reg lv = HpPaRisc.lv_to_reg lv
    val args_phreg = HpPaRisc.reg_args_as_lvs
    val res_phreg = HpPaRisc.reg_res_as_lvs
    val args_phreg_ccall = HpPaRisc.reg_args_ccall_as_lvs
    val res_phreg_ccall = HpPaRisc.reg_res_ccall_as_lvs

    val all_regs = HpPaRisc.all_regs_as_lvs

    val callee_save_ccall_phregs   = HpPaRisc.callee_save_regs_ccall_as_lvs
    val callee_save_ccall_phregset = Lvarset.lvarsetof callee_save_ccall_phregs
    fun is_callee_save_ccall phreg = Lvarset.member(phreg,callee_save_ccall_phregset)

    val caller_save_ccall_phregs   = HpPaRisc.caller_save_regs_ccall_as_lvs
    val caller_save_ccall_phregset = Lvarset.lvarsetof caller_save_ccall_phregs
    fun is_caller_save_ccall phreg = Lvarset.member(phreg,caller_save_ccall_phregset)

    val callee_save_phregs = HpPaRisc.callee_save_regs_mlkit_as_lvs
    val callee_save_phregset = Lvarset.lvarsetof callee_save_phregs
    fun is_callee_save phreg = Lvarset.member(phreg,callee_save_phregset)
    val caller_save_phregs = HpPaRisc.caller_save_regs_mlkit_as_lvs
    val caller_save_phregset = Lvarset.lvarsetof caller_save_phregs
    fun is_caller_save phreg = Lvarset.member(phreg,caller_save_phregset)
    fun pr_reg phreg = HpPaRisc.pr_reg phreg
    fun reg_eq(reg1,reg2) = HpPaRisc.reg_eq(reg1,reg2)

    val init_frame_offset = 0

    (* Jump Tables *)
    val minCodeInBinSearch = 5
    val maxDiff = 10
    val minJumpTabSize = 5

    (* Names For Primitive Functions *)
    val EQUAL_INT       = "__equal_int"
    val MINUS_INT       = "__minus_int"
    val PLUS_INT        = "__plus_int"
    val MUL_INT         = "__mul_int"
    val NEG_INT         = "__neg_int"
    val ABS_INT         = "__abs_int"
    val LESS_INT        = "__less_int"
    val LESSEQ_INT      = "__lesseq_int"
    val GREATER_INT     = "__greater_int"
    val GREATEREQ_INT   = "__greatereq_int"
    val FRESH_EXN_NAME  = "__fresh_exname"
    val PLUS_FLOAT      = "__plus_float"
    val MINUS_FLOAT     = "__minus_float"
    val MUL_FLOAT       = "__mul_float"
    val DIV_FLOAT       = "__div_float"
    val NEG_FLOAT       = "__neg_float"
    val ABS_FLOAT       = "__abs_float"
    val LESS_FLOAT      = "__less_float"
    val LESSEQ_FLOAT    = "__lesseq_float"
    val GREATER_FLOAT   = "__greater_float"
    val GREATEREQ_FLOAT = "__greatereq_float"

    val prims = ["__equal_int", "__minus_int", "__plus_int", (* "__mul_int", *) (* treat millicode calls as C calls (e.g., mul) *)
		 "__neg_int", "__abs_int", "__less_int", "__lesseq_int",        (*  ; for def-use.. *)
		 "__greater_int", "__greatereq_int", "__fresh_exname",
		 "__plus_float", "__minus_float", "__mul_float", (*"__div_float",*) (* calls a C function *)
		 "__neg_float", "__abs_float", "__less_float", "__lesseq_float",
		 "__greater_float", "__greatereq_float", "less_word__", "greater_word__",
		 "lesseq_word__", "greatereq_word__", "plus_word8__", "minus_word8__",
		 (*"mul_word8__",*) "and__", "or__", "xor__", "shift_left__", "shift_right_signed__",
		 "shift_right_unsigned__", "plus_word__", "minus_word__" (*, "mul_word__"*)]

    fun member n [] = false
      | member n (n'::ns) = n=n' orelse member n ns

    fun is_prim name = member name prims

  end

