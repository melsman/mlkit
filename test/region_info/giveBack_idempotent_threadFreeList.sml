fun test () =
    let
      val name = "giveBack_idempotent_threadFreeList"
	  val _ = Region.giveThreadFreeListToGlobal ()
	  val t1 = Region.getThreadFreeListSize ()
	  val _ = Region.giveThreadFreeListToGlobal ()
	  val t2 = Region.getThreadFreeListSize ()
      val ok = (t2 = t1)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
