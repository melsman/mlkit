fun test () =
    let
      val name = "size_list_nil_is_word"
      val s = Size.size (Size.list Size.int) ([] : int list)
      val ok = (s = 8)  (* one word for nil tag *)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ " (got " ^ Int.toString s ^ ")\n")
    end

val _ = test ()
