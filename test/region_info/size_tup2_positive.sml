fun test () =
    let
      val name = "size_tup2_positive"
      val s = Size.size (Size.tup2 Size.int Size.int) (1, 2)
      val ok = (s > 0)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ " (got " ^ Int.toString s ^ ")\n")
    end

val _ = test ()
