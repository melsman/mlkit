infix 6 + -
fun !(x:'a ref):'a = prim("!",x)
fun eq(a:word,b:word):bool = prim("__equal_word64ub",(a,b))
fun go__noinline(n:word,w:word) =
  if eq(n,0w0) then w
  else
    let with r
        val a = ref`r w
        val answer = !a
        val _ = forceResetting `[r] ()
        val b = ref`r 0w1
        val sum = answer + !b
    in go__noinline(n-0w1,sum)
    end
val w:word = prim("getchar",())
val _:unit = prim("putchar",go__noinline(0w1,w))
val _:unit = prim("putchar",0w10)
