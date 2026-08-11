fun test () =
    let with r
      val name : string`r = "isAtbot_false_afterAllocation"
	  val ok = not (Regions.isAtbot `[r] ())
    in
      if ok then
		print ("OK: " ^ name ^ "\n")
      else
		print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
