fun test () =
    let
      val name = "giveBack_threadFreeList_nonincreasing"
	  val b = Regions.getThreadFreeListSize ()
	  val _ = Regions.giveThreadFreeListToGlobal ()
	  val after = Regions.getThreadFreeListSize ()
      val ok = (after <= b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
