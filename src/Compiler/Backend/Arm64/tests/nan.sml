infix 7 /
infix 4 < <= > >=
fun op / (a:real,b:real):real = prim("__div_real",(a,b))
fun unordered__noinline(x:real,y:real) =
  let val z = x/y
  in if z < x then 0w88 else if z <= x then 0w88 else
     if z > x then 0w88 else if z >= x then 0w88 else 0w65
  end
val _:unit = prim("putchar",unordered__noinline(0.0,0.0))
val _:unit = prim("putchar",0w10)
