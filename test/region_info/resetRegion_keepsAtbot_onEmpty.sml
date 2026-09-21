fun test () =
    let with r
      val name = "resetRegion_keepsAtbot_onEmpty"
	  val _ = Region.resetRegion `[r] ()
      val ok = (Region.isAtbot `[r] ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
