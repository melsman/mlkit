infix 6 +
fun flip__noinline(w:word):word = prim("__xorb_word64ub",(w,0wxFEDCBA9876543210))
val w:word = prim("getchar",())
val _:unit = prim("putchar",flip__noinline(flip__noinline w)+0w1)
val _:unit = prim("putchar",0w10)
