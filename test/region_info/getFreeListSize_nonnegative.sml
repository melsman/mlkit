fun test () =
    let
      val name = "getFreeListSize_nonnegative"
      val ok = (Regions.getFreeListSize () > 0)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
