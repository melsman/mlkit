fun test () =
    let
      val name = "giveBack_globalFreeList_nondecreasing"
	  val b = Region.getFreeListSize ()
	  val _ = Region.giveThreadFreeListToGlobal ()
	  val after = Region.getFreeListSize ()
      val ok = (after >= b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
