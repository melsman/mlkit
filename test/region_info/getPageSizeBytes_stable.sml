fun test () =
	let
	  val name = "getPageSizeBytes_stable"
	  val p1 = Region.getPageSizeBytes ()
	  val p2 = Region.getPageSizeBytes ()
	  val ok = (p1 = p2)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
