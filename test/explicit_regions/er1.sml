
(* It is an error to declare a region with a name that is already in
 * scope *)

fun f () =
  let with r
      val a = 3.2`r
      with r
  in #1 (4,3.2`r,a)
  end
