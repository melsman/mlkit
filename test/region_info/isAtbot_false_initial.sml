fun test () =
    let with r
      val name = "isAtbot_false_afterAllocation"
	  val string = "test string"`r
	  val ok = not (Regions.isAtbot `[r] ())
    in
      (if ok then print ("OK: " ^ name ^ "\n")
       else print ("FAIL: " ^ name ^ "\n");
	   string ^ "\n")
    end

val _ = test ()
