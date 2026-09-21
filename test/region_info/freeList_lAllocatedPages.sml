fun test () =
    let
      val name = "freeList_lAllocatedPages"
      val ok = (Region.getFreeListSize () < Region.getNumAllocatedPages ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
