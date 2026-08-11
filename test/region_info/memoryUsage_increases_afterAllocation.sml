fun test () =
    let with r
      val name : string`r = "memoryUsage_increases_afterAllocation"
	  val b = Regions.memoryUsageOfRegion `[r] ()
	  val n = (Regions.getPageSizeBytes () div 2) + 1
	  val s : string`r = CharVector.tabulate (n, fn _ => #"q")
	  val after = Regions.memoryUsageOfRegion `[r] ()
      val ok = (after > b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
