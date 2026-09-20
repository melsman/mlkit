fun test () =
    let
      val name = "giveBack_threadFreeList_nonincreasing"
	  val b = Region.getThreadFreeListSize ()
	  val _ = Region.giveThreadFreeListToGlobal ()
	  val after = Region.getThreadFreeListSize ()
      val ok = (after <= b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
