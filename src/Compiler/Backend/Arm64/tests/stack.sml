infix 6 + -
infix 4 =
fun sum__noinline(a:word,b,c,d,e,f,g,h,i,j,k) = a+b+c+d+e+f+g+h+i+j+k
fun recur__noinline(n:word,a,b,c,d,e,f,g,h,i,j) =
  if prim("__equal_word64ub",(n,0w0)) then sum__noinline(a,b,c,d,e,f,g,h,i,j,0w0)
  else recur__noinline(n-0w1,b,c,d,e,f,g,h,i,j,a)
val w:word = prim("getchar",())
val x = recur__noinline(0w1000001,w,0w1,0w0,0w0,0w0,0w0,0w0,0w0,0w0,0w0)
val _:unit = prim("putchar",x)
val _:unit = prim("putchar",0w10)
