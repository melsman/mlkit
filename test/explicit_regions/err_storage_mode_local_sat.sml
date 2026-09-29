(* Reject sat allocation where a local region requires atbot. *)
infix +
fun f () = let with r in 5.4`sat r + 1.0 end
