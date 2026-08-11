fun test () =
    let
      val name = "threadFree_lTotalAllocated"
      val ok = (Regions.getThreadFreeListSize () < Regions.getNumAllocatedPages ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
