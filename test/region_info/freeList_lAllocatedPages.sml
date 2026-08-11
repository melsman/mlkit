fun test () =
    let
      val name = "freeList_lAllocatedPages"
      val ok = (Regions.getFreeListSize () < Regions.getNumAllocatedPages ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
