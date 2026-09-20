fun test () =
    let
      val name = "giveBack_preservesFreeListSum"
	  val b= Region.getFreeListSize () + Region.getThreadFreeListSize ()
	  val _ = Region.giveThreadFreeListToGlobal ()
	  val after = Region.getFreeListSize () + Region.getThreadFreeListSize ()
      val ok = (after = b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
