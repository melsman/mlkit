fun test () =
	let with r
	  val name : string`r = "memoryUsage_nonnegative_initial"
	  val n = Regions.memoryUsageOfRegion `[r] () 
	  val ok = (n >= 0)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
