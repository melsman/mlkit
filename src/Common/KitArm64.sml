
structure K =
  let structure KC = KitCompiler(ExecutionArm64)
      val _ = Flags.turn_on "garbage_collection"
      val () = List.app Flags.block_entry
                        ["export_basis_js",
                         "equalelim_opt_unboxed"]
  in KitMain(KC)
  end
