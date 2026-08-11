fun test () =
    let with r
      val name = "resetRegion_keepsAtbot_onEmpty"
	  val _ = Regions.resetRegion `[r] ()
      val ok = (Regions.isAtbot `[r] ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
