fun test () =
    let with r
      val name : string`r = "memoryUsage_increases_afterAllocation"
	  val b = Region.memoryUsageOfRegion `[r] ()
	  val n = (Region.getPageSizeBytes () div 2) + 1
	  val s : string`r = CharVector.tabulate (n, fn _ => #"q")
	  val after = Region.memoryUsageOfRegion `[r] ()
      val ok = (after > b)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
