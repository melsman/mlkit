fun test () =
    let
      val name = "size_string_positive"
      val ok = (Size.size Size.string "hello" > 0)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
