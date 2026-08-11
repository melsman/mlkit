fun test () =
	let with r
	  val name : string`r = "numPages_increasing_afterAllocation"
	  val b = Regions.numPagesOfRegion `[r] ()
	  val n = ((Regions.getPageSizeBytes ()) + 1) div 2
	  val s1 : string`r = CharVector.tabulate (n, fn i => if i = 0 then #"A" else #"B")
	  val s2 : string`r = CharVector.tabulate (n, fn i => if i = 0 then #"C" else #"D")
	  val after = Regions.numPagesOfRegion `[r] ()
	  val ok = (after > b)
	in
	  if ok then print ("OK: " ^ name ^ "\n")
	  else print ("FAIL: " ^ name ^ "\n")
	end

val _ = test ()
