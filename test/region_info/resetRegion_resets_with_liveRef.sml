(* Regions.resetRegion resets even when a live reference exists.
   The built-in resetRegions would refuse to reset in this situation.
   We verify the reset happened by checking memory usage decreased,
   even though we use the value after the reset. *)

fun test () =
    let with r
      val name = "resetRegion_resets_with_liveRef"
	  val n = (Regions.getPageSizeBytes () div 2) + 1
	  val s : string`r = CharVector.tabulate (n, fn _ => #"x")
	  val beforeReset = Regions.memoryUsageOfRegion `[r] ()
	  val _ = Regions.resetRegion `[r] ()
	  val afterReset = Regions.memoryUsageOfRegion `[r] ()
      val ok = (afterReset < beforeReset)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n");
	  ignore s
    end

val _ = test ()
