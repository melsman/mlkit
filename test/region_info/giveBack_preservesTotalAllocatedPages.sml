fun test () =
    let
      val name = "giveBack_preservesTotalAllocatedPages"
	  val b = Regions.getNumAllocatedPages ()
	  val _ = Regions.giveThreadFreeListToGlobal ()
	  val after = Regions.getNumAllocatedPages ()
      val ok = (after = b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
