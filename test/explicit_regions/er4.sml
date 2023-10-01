
(* It is an error to refer to a region name that is not in scope *)

fun f () =
  let val a = 3.2`r
      with r2
  in #1 (4,3.2`r2,a)
  end
