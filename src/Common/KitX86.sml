
structure K = 
  let structure KC = KitCompiler(ExecutionX86)
      val _ = Flags.turn_on "garbage_collection"
  in KitMain(KC)
  end
	    
