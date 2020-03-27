signature BACKEND_INFO =
  sig

    (* Architecture and non architecture backend info *)
    type label
    type offset = int

    val init_clos_offset   : offset     (* First offset in FN closure is 1 and
					 * code pointer is at offset 0 *)
    val init_sclos_offset  : offset     (* First offset in shared closure is 0 *)
    val init_regvec_offset : offset     (* First offset in region vector is 0 *)

    (* Tagging *)
    val ml_true  : int (* The representation of true *)
    val ml_false : int (* The representation of false *)
    val ml_unit  : int (* The representation of unit *)

    val pr_tag_w : Word32.word -> string
    val pr_tag_i : int -> string

    val tag_real       : bool -> Word32.word
    val tag_word_boxed : bool -> Word32.word
    val tag_string     : bool * int -> Word32.word
    val tag_record     : bool * int -> Word32.word
    val tag_blockf64   : bool * int -> Word32.word
    val tag_con0       : bool * int -> Word32.word
    val tag_con1       : bool * int -> Word32.word
    val tag_ref        : bool -> Word32.word
    val tag_clos       : bool * int * int -> Word32.word
    val tag_sclos      : bool * int * int -> Word32.word
    val tag_regvec     : bool * int -> Word32.word
    val tag_table      : bool * int -> Word32.word
    val tag_exname     : bool -> Word32.word
    val tag_excon0     : bool -> Word32.word
    val tag_excon1     : bool -> Word32.word
    val tag_ignore     : Word32.word

    val inf_bit          : int (* We must add 1 to an address to set the infinite bit. *)
    val atbot_bit        : int (* We must add 2 to an address to set the atbot bit. *)

    val tag_values       : unit -> bool
    val size_of_real     : unit -> int
    val size_of_ref      : unit -> int
    val size_of_record   : 'a list -> int
    val size_of_handle   : unit -> int

    val size_of_reg_desc : unit -> int  (* dependent on whether region profiling is enabled *)

    val finiteRegionDescSizeP : int     (* Number of words in a finite region
					 * descriptor when profiling is used. *)
    val objectDescSizeP       : int     (* Number of words in an object descriptor
					 * when profiling is used. *)
    val defaultIntPrecision : unit -> int
    val defaultWordPrecision : unit -> int

    val init_frame_offset : offset

    val exn_DIV_lab       : label       (* Global exceptions are globally allocated. *)
    val exn_MATCH_lab     : label
    val exn_BIND_lab      : label
    val exn_OVERFLOW_lab  : label
    val exn_INTERRUPT_lab : label

    val toplevel_region_withtype_top_lab    : label
    val toplevel_region_withtype_bot_lab    : label
    val toplevel_region_withtype_string_lab : label
    val toplevel_region_withtype_pair_lab   : label
    val toplevel_region_withtype_array_lab  : label
    val toplevel_region_withtype_ref_lab    : label
    val toplevel_region_withtype_triple_lab : label

    (* Jump Tables *)
    val minCodeInBinSearch : int
    val maxDiff            : int
    val minJumpTabSize     : int

    val down_growing_stack : bool         (* true for x86/x64 code generation *)
  end
