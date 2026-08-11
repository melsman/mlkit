fun test () =
    let with r
      val name = "resetRegion_setsAtbot_afterAllocation"
	  val n = (Regions.getPageSizeBytes () div 8) + 1
	  val s : string`r = CharVector.tabulate (n, fn _ => #"h")
	  val _ = Regions.resetRegion `[r] ()
      val ok = (Regions.isAtbot `[r] ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
