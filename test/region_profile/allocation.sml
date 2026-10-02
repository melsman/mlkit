infix 6 + -
infix 4 =
fun op = (a:int,b:int) : bool = prim ("=", (a,b))
fun print (s:string) : unit = prim ("printStringML", s)
fun sample () : unit = prim ("mlkit_rp_sample", ())
fun touch (p:int*int) : int = prim ("ap_pair", p)
fun pair__noinline `r n = (n,n)`attop r
fun run__noinline () =
    let with r
        fun loop n = if n = 0 then ()
                     else (touch (pair__noinline `r n); loop (n-1))
        val iterations : int = prim ("ap_iterations", ())
        val _ = loop iterations
        val _ = sample ()
        val _ = forceResetting `[r] ()
        val _ = loop 10
    in sample ()
    end
val _ = (run__noinline (); print "allocation ok\n")
