fun test () =
    let
      val name = "giveBack_preservesFreeListSum"
	  val b= Regions.getFreeListSize () + Regions.getThreadFreeListSize ()
	  val _ = Regions.giveThreadFreeListToGlobal ()
	  val after = Regions.getFreeListSize () + Regions.getThreadFreeListSize ()
      val ok = (after = b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
