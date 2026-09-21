fun test () =
    let
      val name = "giveBack_preservesTotalAllocatedPages"
	  val b = Region.getNumAllocatedPages ()
	  val _ = Region.giveThreadFreeListToGlobal ()
	  val after = Region.getNumAllocatedPages ()
      val ok = (after = b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
