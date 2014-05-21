functor BackendInfo(val down_growing_stack : bool) : BACKEND_INFO =
  struct
    structure PP = PrettyPrint
    structure Labels = AddressLabels
    fun die s  = Crash.impossible ("BackendInfo." ^ s)

    type label = Labels.label
    type offset = int

    val init_clos_offset = 1     (* First offset in FN closure is 1 and code pointer is at offset 0 *) 
    val init_sclos_offset = 0	 (* First offset in shared closure is 0 *)                             
    val init_regvec_offset = 0	 (* First offset in region vector is 0 *)                              

    (***********)
    (* Tagging *)
    (***********)

    fun pr_tag_w tag = "0X" ^ (Word32.fmt StringCvt.HEX tag)
    (* For now, some tags are in integers but it should be eliminated; max size is then 2047 only 09/01/1999, Niels *)
    fun pr_tag_i tag = "0X" ^ (Int.fmt StringCvt.HEX tag)

    (* off is the offset at which values are traversed *)
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

    fun tag_real(i:bool)              = gen_record_tag(2,2,i,6)
    fun tag_word_boxed(i:bool)        = gen_record_tag(1,1,i,6)
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

    val tag_values       = Flags.is_on0 "tag_values"
    val region_profiling = Flags.is_on0 "region_profiling"
    val gengc_p          = Flags.is_on0 "generational_garbage_collection"

    val size_of_real = RegConst.size_of_real
    val size_of_ref = RegConst.size_of_ref
    val size_of_record = RegConst.size_of_record
    fun size_of_handle()   = 4

    local
      val region_large_objects = true (* upon change, also change src/Runtime/Makefile *)
      val size_gen = 3
      fun size_lobjs() = if region_large_objects then 1 else 0
      fun size_g0() = size_gen
      fun size_prev_ptr() = 1
      fun size_g1() = if gengc_p() then size_gen else 0
      fun size_prof() = if region_profiling() then 3 else 0
    in
      fun size_of_reg_desc() = 
	size_g0() + size_g1() + size_prev_ptr() + size_prof() + size_lobjs()
    end

    val finiteRegionDescSizeP = 2 (* Number of words in a finite region descriptor when profiling is used. *)
    val objectDescSizeP = 2       (* Number of words in an object descriptor when profiling is used. *)

    fun defaultIntPrecision() = if tag_values() then 31 else 32
    fun defaultWordPrecision() = if tag_values() then 31 else 32

    val toplevel_region_withtype_top_lab    = Labels.reg_top_lab
    val toplevel_region_withtype_bot_lab    = Labels.reg_bot_lab
    val toplevel_region_withtype_string_lab = Labels.reg_string_lab
    val toplevel_region_withtype_pair_lab   = Labels.reg_pair_lab
    val toplevel_region_withtype_array_lab  = Labels.reg_array_lab
    val toplevel_region_withtype_ref_lab    = Labels.reg_ref_lab
    val toplevel_region_withtype_triple_lab = Labels.reg_triple_lab

    val exn_DIV_lab       = Labels.exn_DIV_lab       (* Global exceptions are globally allocated. *)
    val exn_MATCH_lab     = Labels.exn_MATCH_lab
    val exn_BIND_lab      = Labels.exn_BIND_lab
    val exn_OVERFLOW_lab  = Labels.exn_OVERFLOW_lab
    val exn_INTERRUPT_lab = Labels.exn_INTERRUPT_lab

    val init_frame_offset = 0

    (* Jump Tables *)
    val minCodeInBinSearch = 5
    val maxDiff = 10
    val minJumpTabSize = 5

    (* Primitives that are inlined by the compiler; in contrast to
     * those primitives that are implemented as C calls *)

    local
      structure S = OrderSet(struct type T = string
				    fun lt (a: T) b = a < b
			     end)

      val S_flow = S.fromList
	["__equal_int31", "__equal_int32ub", "__equal_int32b", 
	 "__equal_word31", "__equal_word32ub", "__equal_word32b", 
	 "__less_int31", "__less_int32ub", "__less_int32b", 
	 "__less_word31", "__less_word32ub", "__less_word32b", 
	 "__lesseq_int31", "__lesseq_int32ub", "__lesseq_int32b", 
	 "__lesseq_word31", "__lesseq_word32ub", "__lesseq_word32b", 
	 "__greater_int31", "__greater_int32ub", "__greater_int32b", 
	 "__greater_word31", "__greater_word32ub", "__greater_word32b", 
	 "__greatereq_int31", "__greatereq_int32ub", "__greatereq_int32b", 
	 "__greatereq_word31", "__greatereq_word32ub", "__greatereq_word32b"
	 ]

      val S = S.fromList
	["__less_real", "__lesseq_real", "__greater_real", "__greatereq_real",
	 "__plus_int31", "__plus_int32ub", "__plus_int32b", 
	 "__plus_word31", "__plus_word32ub", "__plus_word32b", "__plus_real",
	 "__minus_int31", "__minus_int32ub", "__minus_int32b", 
	 "__minus_word31", "__minus_word32ub", "__minus_word32b", "__minus_real", 
	 "__mul_int31", "__mul_int32ub", "__mul_int32b", 
	 "__mul_word31", "__mul_word32ub", "__mul_word32b", "__mul_real",
	 "__div_real",
	 "__neg_int31", "__neg_int32ub", "__neg_int32b", "__neg_real",  
	 "__abs_int31", "__abs_int32ub", "__abs_int32b", "__abs_real",  
	 "__andb_word31", "__andb_word32ub", "__andb_word32b",
	 "__orb_word31", "__orb_word32ub", "__orb_word32b",
	 "__xorb_word31", "__xorb_word32ub", "__xorb_word32b",
	 "__shift_left_word31", "__shift_left_word32ub", "__shift_left_word32b", 
	 "__shift_right_signed_word31", 
	 "__shift_right_signed_word32ub", "__shift_right_signed_word32b", 
	 "__shift_right_unsigned_word31", 
	 "__shift_right_unsigned_word32ub", "__shift_right_unsigned_word32b", 
	 
	 "__int31_to_int32b", "__int31_to_int32ub", "__int32b_to_int31", "__int32b_to_word32b", "__int32ub_to_int31",

	 "__word31_to_word32b", "__word31_to_word32ub", "__word32b_to_word31", "__word32ub_to_word31",

	 "__word31_to_word32ub_X", "__word31_to_word32b_X", 

	 "__word32b_to_int32b", "__word32b_to_int32b_X", "__word32ub_to_int32ub", "__word31_to_int31", 
	 "__word32b_to_int31", "__int32b_to_word31", "__word32b_to_int31_X",

	 "__exn_ptr", "__fresh_exname", 

         "__bytetable_sub", "__bytetable_size", "__bytetable_update",

	 "word_sub0", "word_update0", "table_size",
	 
	 "__is_null",

	 "__serverGetCtx"]
   
    in
      fun is_prim name = S.member name S orelse S.member name S_flow
      fun is_flow_prim name = S.member name S_flow
    end

    val down_growing_stack = down_growing_stack
  end
