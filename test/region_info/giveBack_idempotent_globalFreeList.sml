fun test () =
    let
      val name = "giveBack_idempotent_globalFreeList"
	  val _ = Region.giveThreadFreeListToGlobal ()
	  val g1 = Region.getFreeListSize ()
	  val _ = Region.giveThreadFreeListToGlobal ()
	  val g2 = Region.getFreeListSize ()
      val ok = (g2 = g1)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
