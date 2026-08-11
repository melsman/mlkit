fun test () =
    let
      val name = "getNumAllocatedPages_nonnegative"
      val ok = (Regions.getNumAllocatedPages () >= 0)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
