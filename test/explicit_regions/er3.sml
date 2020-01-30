
(* A region cannot be used for values that belong to different region types *)

fun f () =
  let region r1
      val a = 3.2`r1
  in #1 (4, a, "hi there"`r1)
  end
