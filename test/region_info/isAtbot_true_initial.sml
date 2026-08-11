fun test () =
    let with r
      val name = "isAtbot_true_initial"
      val ok = Regions.isAtbot `[r] ()
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
