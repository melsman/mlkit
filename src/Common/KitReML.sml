
structure K =
  let structure KC = KitCompiler(ExecutionX64)
      val _ = Flags.turn_on "reml"
      val _ = Flags.turn_on "aggresive_opt"
      val _ = Flags.turn_on "parallelism"
      val _ = Flags.turn_off "tag_values"
      val _ = Flags.turn_on "preserve_tail_calls"
      val () = List.app Flags.block_entry
                        ["garbage_collection",
                         "warn_spurious",
                         "values_64bit",
                         "tag_values",
                         "tag_pairs",
                         "statistics_spurious",
                         "repository",
                         "regionvar",
                         "quotation",
                         "generational_garbage_collection",
                         "extra_gc_checks",
                         "export_basis_js",
                         "disable_spurious_type_variables"
                        ]
  in KitMain(KC)
  end
