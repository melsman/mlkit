fun test () =
    let with r
      val name = "resetRegion_doesNotIncreaseMemoryUsage"
	  val n = (Region.getPageSizeBytes () div 2) + 1
	  val s : string`r = CharVector.tabulate (n, fn _ => #"z")
	  val beforeReset = Region.memoryUsageOfRegion `[r] ()
	  val _ = Region.resetRegion `[r] ()
	  val afterReset = Region.memoryUsageOfRegion `[r] ()
      val ok = (afterReset < beforeReset)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
