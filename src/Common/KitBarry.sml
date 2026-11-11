structure K =
  let structure KC = KitCompiler(ExecutionBarry)
      val () = Flags.turn_off "cross_module_opt"
      val () = Flags.turn_off "unbox_reals"
      (* Portable SML retains the host reference and real representations. *)
      val () = Flags.turn_off "data_flattening_refs"
      val () = Flags.turn_off "data_flattening_floats"
      val () = Flags.turn_off "unbox_funargs"
      val () = Flags.turn_off "unbox_real_funargs"
      val () = Flags.turn_off "eliminate_polymorphic_equality"
      val () = Flags.turn_off "uncurrying"
      (* Barry emits portable Standard ML, which has no integer shift or
       * mask operations. Keep div/mod for its source-level output. *)
      val () = Flags.turn_off "divmod_pow2"
      val () = Flags.turn_off "equalelim_opt_unboxed"
      val () = List.app Flags.block_entry
                        ["garbage_collection",
                         "generational_garbage_collection",
                         "values_64bit", "unbox_real_funargs", "unbox_reals", "tag_values", "tag_pairs",
                         "repository", "reml",
                         "preserve_tail_calls", "dangling_pointers", "report_boxities", "high_pointer_tagging",
                         "equalelim_opt_unboxed", "divmod_pow2",
                         "data_flattening_refs", "data_flattening_floats"
                      ]

  in KitMain(KC)
  end
