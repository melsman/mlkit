structure K =
let structure KC = KitCompiler(ExecutionJS)
    val () = Flags.turn_off "equalelim_opt_unboxed"
    val () = Flags.turn_off "unbox_reals"
    val () = Flags.turn_off "unbox_real_funargs"
    val () = List.app Flags.block_entry
                      ["garbage_collection",
                       "generational_garbage_collection",
                       "values_64bit", "unbox_reals", "unbox_real_funargs", "tag_values", "tag_pairs",
                       "repository", "reml", "region_profiling", "region_inference",
                       "print_region_flow_graph", "print_all_program_points",
                       "preserve_tail_calls", "dangling_pointers", "equalelim_opt_unboxed"
                      ]
in KitMain(KC)
end
