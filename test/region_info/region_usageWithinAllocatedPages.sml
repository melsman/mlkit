fun test () =
    let with r
      val name = "region_usageWithinAllocatedPages"
	  val pages = Region.numPagesOfRegion `[r] ()
	  val usage = Region.memoryUsageOfRegion `[r] ()
	  val ps = Region.getPageSizeBytes ()
	  val bound = (IntInf.fromInt pages) * (IntInf.fromInt ps)
	  val ok = (IntInf.fromInt usage) <= bound
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
