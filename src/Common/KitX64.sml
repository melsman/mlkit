
structure K =
  let structure KC = KitCompiler(ExecutionX64)
      val _ = Flags.turn_on "garbage_collection"
      val () = List.app Flags.block_entry
                        ["export_basis_js"]
  in KitMain(KC)
  end
