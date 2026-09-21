fun test () =
    let
      val name = "size_int_zero"
      val ok = (Size.size Size.int 42 = 0)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
