fun test () =
    let
      val name = "getThreadFreeListSize_nonnegative"
      val ok = (Regions.getThreadFreeListSize () > 0)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
