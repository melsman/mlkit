infix +
fun f `[rr rp] () : real*real =
  let val x = 4.3`rr
  in (x,x)`rp
  end

fun g `[r] () =
    let region r2
        val (_,y) = f `[r r2] ()
    in y + 0.0
    end
