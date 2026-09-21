infix 6 + -
infix 4 <
infixr 5 ::
fun build__noinline(n:int,xs) = if n < 1 then xs else build__noinline(n-1,n::xs)
fun sum__noinline(nil,n:int)=n | sum__noinline(x::xs,n)=sum__noinline(xs,n+x)
fun mixed__noinline(a,b,c,d,e,f,g,h,i,j,k,l,
                   x0:real,x1:real,x2:real,x3:real,x4:real,x5:real,
                   x6:real,x7:real,x8:real,x9:real) =
  if x0+x1+x2+x3+x4+x5+x6+x7+x8+x9 < 56.0 then
    sum__noinline(a,0)+sum__noinline(b,0)+sum__noinline(c,0)+sum__noinline(d,0)+
    sum__noinline(e,0)+sum__noinline(f,0)+sum__noinline(g,0)+sum__noinline(h,0)+
    sum__noinline(i,0)+sum__noinline(j,0)+sum__noinline(k,0)+sum__noinline(l,0)
  else 0
fun raise__noinline(n:int) =
  let val xs=build__noinline(n,nil)
      val n=sum__noinline(xs,0)
  in if n < 1 then 0 else raise Overflow end
fun handler__noinline(n:int) =
  let val xs=build__noinline(100,nil)
  in raise__noinline n handle _ => sum__noinline(xs,0) end
val xs=build__noinline(10,nil)
val n=mixed__noinline(xs,xs,xs,xs,xs,xs,xs,xs,xs,xs,xs,xs,
                     1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0)
val h=handler__noinline 20
val _:unit=prim("printStringML",if 659 < n andalso n < 661 andalso 5049 < h andalso h < 5051 then "OK\n" else "BAD\n")
