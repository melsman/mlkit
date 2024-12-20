structure K =
  let structure KC = KitCompiler(ExecutionBarry)
      val () = Flags.turn_off "cross_module_opt"
      val () = Flags.turn_off "unbox_reals"
      val () = Flags.turn_off "unbox_function_arguments"
      val () = Flags.turn_off "eliminate_polymorphic_equality"
      val () = Flags.turn_off "uncurrying"
      val () = List.app Flags.block_entry
                        ["garbage_collection",
                         "generational_garbage_collection",
                         "values_64bit", "unbox_reals", "tag_values", "tag_pairs",
                         "repository", "reml", "region_profiling", "region_inference",
                         "print_region_flow_graph", "print_all_program_points",
                         "preserve_tail_calls", "dangling_pointers", "report_boxities", "high_pointer_tagging"
                      ]

  in KitMain(KC)
  end
