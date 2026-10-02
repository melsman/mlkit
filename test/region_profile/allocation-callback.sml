infix 4 =
fun op = (a:int,b:int) : bool = prim ("=", (a,b))
fun print (s:string) : unit = prim ("printStringML", s)
fun sample () : unit = prim ("mlkit_rp_sample", ())
fun array `r n : int array`r = prim ("word_table0", n)
fun foreign `r n : int array`r = prim ("ap_foreign", n)
exception Escape
fun callback n = (array 3; if n = 0 then raise Escape else n)
val _ = _export ("ap_callback", callback)
fun run__noinline () =
    let with r
        val _ = foreign `attop r 1
        val _ = (foreign `attop r 0; ()) handle Escape => ()
        val _ = array `attop r 4
    in sample ()
    end
val _ = (run__noinline (); print "callback allocation ok\n")
