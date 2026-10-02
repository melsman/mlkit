fun make `r n : string`r = prim ("allocStringML", n)
fun sample () : unit = prim ("mlkit_rp_sample", ())
fun run__noinline () =
    let with r
        val _ = make `attop r 17
        val _ = make `attop r 9000
    in sample ()
    end
val _ = run__noinline ()
