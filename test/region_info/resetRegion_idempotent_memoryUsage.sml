fun test () =
    let with r
      val name = "resetRegion_idempotent_memoryUsage"
	  val n = (Regions.getPageSizeBytes () div 4) + 1
	  val s : string`r = CharVector.tabulate (n, fn _ => #"m")
	  val _ = Regions.resetRegion `[r] ()
	  val u1 = Regions.memoryUsageOfRegion `[r] ()
	  val _ = Regions.resetRegion `[r] ()
	  val u2 = Regions.memoryUsageOfRegion `[r] ()
      val ok = (u1 = u2)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
