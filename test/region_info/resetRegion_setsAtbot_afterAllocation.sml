fun test () =
    let with r
      val name = "resetRegion_setsAtbot_afterAllocation"
	  val n = (Region.getPageSizeBytes () div 8) + 1
	  val s : string`r = CharVector.tabulate (n, fn _ => #"h")
	  val _ = Region.resetRegion `[r] ()
      val ok = (Region.isAtbot `[r] ())
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
