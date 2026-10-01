(* ReML example: three named regions grow and reset at different phases. *)
infix 6 + -
infix 4 = >
fun op = (a:int,b:int) : bool = prim ("=", (a,b))
fun sample () : unit = prim ("mlkit_rp_sample", ())
fun mark (s:string) : unit = prim ("mlkit_rp_mark", s)
fun print (s:string) : unit = prim ("printStringML", s)
fun alloc `r n : int array`r = prim ("word_table0", n)
fun update (a:int array,n:int) : unit = prim ("word_update0", (a,0,n))
fun sub (a:int array) : int = prim ("word_sub0", (a,0))
fun run__noinline () =
    let with alpha beta gamma
        fun loop__noinline n =
            if n = 0 then (mark "done"; sample (); 0)
            else
              let val a = alloc `attop alpha (n+500)
                  val b = alloc `attop beta (n+800)
                  val c = alloc `attop gamma (n+1200)
                  val _ = update (a,n)
                  val _ = update (b,n)
                  val _ = update (c,n)
                  val v = sub a + sub b + sub c
                  val _ = sample ()
                  val _ = if n = 12 then (mark "reset alpha"; forceResetting `[alpha] ()) else ()
                  val _ = if n = 6 then (mark "reset beta"; forceResetting `[beta] ()) else ()
              in v + loop__noinline (n-1)
              end
    in loop__noinline 24
    end
val _ = (run__noinline (); print "graph example ok\n")
