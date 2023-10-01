(* It is an error to call a function with a different non-zero
 * number of region parameters than the function declares *)

infix +
fun f `[rr rp] () : real*real =
  let val x = 4.3`rr
  in (x,x)`rp
  end

fun g () =
    let with r2
        val (_,y) = f `[r2] ()
    in y + 0.0
    end
