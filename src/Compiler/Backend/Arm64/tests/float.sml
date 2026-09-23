infix 6 + -
infix 7 * /
infix 4 <
fun mix__noinline(a:real,b:real,c:real,d:real,e:real,f:real,g:real,h:real,i:real,j:real,w:word) =
  if a+b+c+d+e+f+g+h+i+j < 56.0 then w+0w1 else w
val w:word = prim("getchar",())
val _:unit = prim("putchar",mix__noinline(1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,w))
val _:unit = prim("putchar",0w10)
