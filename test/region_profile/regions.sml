infix 6 + -
infix 4 = >
fun op = (a:int,b:int) : bool = prim ("=", (a,b))
fun sample () : unit = prim ("mlkit_rp_sample", ())
fun mark (s:string) : unit = prim ("mlkit_rp_mark", s)
fun print (s:string) : unit = prim ("printStringML", s)
fun touch (p:int*int) : int = prim ("rp_pair", p)
fun alloc `r n : int array`r = prim ("word_table0", n)
fun update (a:int array,n:int) : unit = prim ("word_update0", (a,0,n))
fun sub (a:int array) : int = prim ("word_sub0", (a,0))
fun inner__noinline n =
    let with r
        val p = (n,n)`r
        val _ = (mark "inner"; sample ())
    in touch p
    end
fun outer__noinline n =
    let with rf ri
        val p = (n,n)`rf
        val a = alloc `ri n
        val _ = update (a,n)
        val _ = inner__noinline n
        val _ = (mark "outer"; sample ())
        val answer = touch p + sub a
        val _ = forceResetting `[ri] ()
        val _ = (mark "reset"; sample ())
        val b = alloc `ri 3
        val _ = update (b,n)
        val _ = (mark "small"; sample ())
    in answer + sub b
    end
val n : int = prim ("rp_number", ())
val _ = (outer__noinline n; mark "after"; sample (); print "regions ok\n")
fun recursive__noinline (depth,n) =
    if depth = 0 then (mark "recursive"; sample (); 0)
    else
      let with r
          val p = (n,n)`r
          val a = recursive__noinline (depth-1,n)
      in a + touch p
      end
val _ = recursive__noinline (3,n)
exception Done
fun raise__noinline () = raise Done
fun handler__noinline n =
    let with r
        val p = (n,n)`r
        val _ = (raise__noinline () handle Done => (mark "handler"; sample ()))
    in touch p
    end
val _ = handler__noinline n
fun pages__noinline n =
    let with ri
        val a = alloc `attop ri (n-2400)
        val _ = update (a,n)
        val b = alloc `attop ri (n-2400)
        val _ = update (b,n)
        val _ = (mark "pages"; sample ())
    in sub a + sub b
    end
val _ = pages__noinline n
fun spilled__noinline (a,b,c,d,e,f,g,h,i,j,k,l) =
    (mark "spilled"; sample (); (a,b,c,d,e,f,g,h,i,j,k,l))
fun caller__noinline n =
    let with r
        val p = (n,n)`r
        val (a,b,c,d,e,f,g,h,i,j,k,l) =
            spilled__noinline (n,n+1,n+2,n+3,n+4,n+5,n+6,n+7,n+8,n+9,n+10,n+11)
    in touch p + a+b+c+d+e+f+g+h+i+j+k+l
    end
val _ = caller__noinline n
