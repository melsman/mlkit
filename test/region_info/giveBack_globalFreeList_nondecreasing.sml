fun test () =
    let
      val name = "giveBack_globalFreeList_nondecreasing"
	  val b = Regions.getFreeListSize ()
	  val _ = Regions.giveThreadFreeListToGlobal ()
	  val after = Regions.getFreeListSize ()
      val ok = (after >= b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
