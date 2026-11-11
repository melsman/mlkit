structure K =
let structure KC = KitCompiler(ExecutionJS)
    val () = Flags.turn_off "equalelim_opt_unboxed"
    (* JavaScript numbers already represent reals without a separate box. *)
    val () = Flags.turn_off "unbox_reals"
    val () = Flags.turn_off "data_flattening_floats"
    val () = Flags.turn_on "data_flattening_refs"
    val () = Flags.turn_off "unbox_real_funargs"
    val () = List.app Flags.block_entry
                      ["garbage_collection",
                       "generational_garbage_collection",
                       "values_64bit", "unbox_real_funargs", "unbox_reals", "data_flattening_floats", "tag_values", "tag_pairs",
                       "repository", "reml",
                       "preserve_tail_calls", "dangling_pointers", "equalelim_opt_unboxed"
                      ]
in KitMain(KC)
end
