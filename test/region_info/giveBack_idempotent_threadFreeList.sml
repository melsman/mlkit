fun test () =
    let
      val name = "giveBack_idempotent_threadFreeList"
	  val _ = Regions.giveThreadFreeListToGlobal ()
	  val t1 = Regions.getThreadFreeListSize ()
	  val _ = Regions.giveThreadFreeListToGlobal ()
	  val t2 = Regions.getThreadFreeListSize ()
      val ok = (t2 = t1)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
