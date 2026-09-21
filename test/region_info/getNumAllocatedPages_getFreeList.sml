fun test () =
    let
      val name = "getNumAllocatedPages_geFreeList"
      val ok = (Region.getNumAllocatedPages () >= Region.getFreeListSize ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
