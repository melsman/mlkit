fun test () =
    let
      val name = "size_option_some_gt_none"
      val szNone = Size.size (Size.option Size.int) NONE
      val szSome = Size.size (Size.option Size.int) (SOME 42)
      val ok = (szSome > szNone)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name
                   ^ " (NONE=" ^ Int.toString szNone
                   ^ ", SOME=" ^ Int.toString szSome ^ ")\n")
    end

val _ = test ()
