infix 6 + -
infix 4 = <
fun op = (x: ''a,y: ''a): bool = prim("=",(x,y))
exception Wrong
fun check b = if b then () else raise Wrong
(* Runtime input and noinline parameters prevent constant folding/sinking of
 * values across the call. More live integers than the preserved palette also
 * exercise spills. The assembly callee destroys every caller-save register. *)
fun manual__noinline(a:int,b:int,c:int,d:int,e:int,f:int,g:int,h:int) =
  let val r:int = prim("preservation_identity",a)
  in check(r = a); a+b+c+d+e+f+g+h end
fun automatic__noinline(a:int,b:int,c:int,d:int,e:int,f:int,g:int,h:int) =
  let val r:int = prim("@preservation_identity",a)
  in check(r = a); a+b+c+d+e+f+g+h end
fun floating__noinline(a:real,b:real,c:real,d:real,x:int) =
  let val r:int = prim("preservation_identity",x)
  in check(r = x); check(a+b+c+d < 11.0); check(9.0 < a+b+c+d) end
fun callback__noinline(a:int,b:int,c:int,d:int,e:int) =
  let val r:int = prim("preservation_callback",a)
  in check(r = a+1); a+b+c+d+e end
fun hook__noinline(x:int) =
  let val r:int = prim("preservation_identity",x)
  in r+1 end
val () = _export("preservation_hook",hook__noinline)
val x:int = prim("@preservation_seed",())
val () = check(manual__noinline(x,x+1,x+2,x+3,x+4,x+5,x+6,x+7) = 116)
val () = check(automatic__noinline(x,x+1,x+2,x+3,x+4,x+5,x+6,x+7) = 116)
val () = floating__noinline(1.0,2.0,3.0,4.0,x)
val () = check(callback__noinline(x,x+1,x+2,x+3,x+4) = 65)
exception Expected
fun caught__noinline(a:int,b:int) =
  ((check((prim("preservation_identity",a):int) = a); raise Expected)
   handle Expected => a+b)
val () = check(caught__noinline(x,x+1) = 23)
val () = prim("printStringML","C preservation passed\n"):unit
