fun sample () : unit = prim ("mlkit_rp_sample", ())
fun touch (p:int*int) : int = prim ("ap_pair", p)
fun pair__noinline `r n = (n,n)`attop r
fun run__noinline () =
    let with r s
        val _ = touch (pair__noinline `r 1)
        val _ = touch (pair__noinline `r 2)
        val _ = touch (pair__noinline `r 3)
        val _ = touch (pair__noinline `s 4)
        val _ = touch (pair__noinline `s 5)
        val _ = sample ()
        val _ = forceResetting `[r] ()
        val _ = touch (pair__noinline `r 6)
    in sample ()
    end
val _ = run__noinline ()
