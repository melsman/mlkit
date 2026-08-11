fun test () =
    let
      val name = "giveBack_idempotent_globalFreeList"
	  val _ = Regions.giveThreadFreeListToGlobal ()
	  val g1 = Regions.getFreeListSize ()
	  val _ = Regions.giveThreadFreeListToGlobal ()
	  val g2 = Regions.getFreeListSize ()
      val ok = (g2 = g1)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
