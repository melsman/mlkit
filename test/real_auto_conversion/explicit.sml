fun explicit__noinline `r (x:real) : real`r =
    prim `[r] ("@m2_explicit", x)
val x = explicit__noinline 1.25
val () : unit = prim ("printReal", x)
