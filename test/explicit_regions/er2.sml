
(* It is an error to have duplicate region names in a with declaration *)

fun f () =
  let with r1 r2 r10 r2 r3
      val a = 3.2`r10
  in #1 (4, 3.2`r2, a, "hi there"`r3)
  end
