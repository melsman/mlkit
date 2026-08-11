fun test () =
    let with r
      val name = "resetRegion_doesNotIncreaseMemoryUsage"
	  val n = (Regions.getPageSizeBytes () div 2) + 1
	  val s : string`r = CharVector.tabulate (n, fn _ => #"z")
	  val beforeReset = Regions.memoryUsageOfRegion `[r] ()
	  val _ = Regions.resetRegion `[r] ()
	  val afterReset = Regions.memoryUsageOfRegion `[r] ()
      val ok = (afterReset < beforeReset)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
