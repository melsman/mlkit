fun test () =
	let
	  val name = "getPageSizeBytes_stable"
	  val p1 = Regions.getPageSizeBytes ()
	  val p2 = Regions.getPageSizeBytes ()
	  val ok = (p1 = p2) 
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
