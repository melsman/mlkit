fun test () =
    let
      val name = "getNumAllocatedPages_geFreeList"
      val ok = (Regions.getNumAllocatedPages () >= Regions.getFreeListSize ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
