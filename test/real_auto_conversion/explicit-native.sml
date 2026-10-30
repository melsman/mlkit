fun identity__noinline `r (x:real) : real`r =
    prim `[r] ("@real_auto_identity", x)
val x = identity__noinline 1.25
val ok = (prim ("__lesseq_real", (x,1.25)) : bool)
         andalso (prim ("__greatereq_real", (x,1.25)) : bool)
val half : real16 = prim ("@real_auto_half_identity", (~0.0:real16))
val halfbits : word = prim ("id", half)
val ok = ok andalso (prim ("=", (halfbits,0wx8000:word)) : bool)
val () : unit = if ok then prim ("printStringML", "real auto-conversion passed\n")
               else prim ("@exit", (1:int))
