fun test () =
    let
      val name = "size_list_grows"
      val sz = Size.list Size.int
      val s1 = Size.size sz [1]
      val s3 = Size.size sz [1, 2, 3]
      val ok = (s3 > s1)
    in
      if ok then print ("OK: " ^ name ^ "\n")
      else print ("FAIL: " ^ name ^ "\n")
    end

val _ = test ()
