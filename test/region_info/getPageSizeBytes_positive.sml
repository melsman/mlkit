fun test () =
    let
      val name = "getPageSizeBytes_positive"
	  val p = Region.getPageSizeBytes ()
      val ok = (p > 0)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
