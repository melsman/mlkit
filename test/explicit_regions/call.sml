infix +
fun f `[r] () : real = 4.3`r

fun g () =
    let region r2
    in 0.0 + f `[r2] ()
    end
