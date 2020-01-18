
(*
fun f [`r2 `r3 `r3] () =
  let region `r
  in (4,3)
  end
  | f [`r2 `r3 `r3] () = (4,8)
*)

(*
fun f [`r2 `r3 `r2] () =
  let region `r
  in (4,3)
  end
*)

fun g `[r] () = 4
fun f `[r2 r3 r1] () =
    let region r r8
        val v = g `[r2] ()
    in (v,v)
    end
