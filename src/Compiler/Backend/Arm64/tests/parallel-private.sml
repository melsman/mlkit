(* -par0 is only safe when concurrent allocations target disjoint regions. *)
fun build__noinline(n:word,acc) =
  if eq(n,0w0) then acc else build__noinline(n-0w1,n::acc)
fun sum__noinline(nil,acc:word) = acc
  | sum__noinline(x::xs,acc) = sum__noinline(xs,acc+x)
fun work () = sum__noinline(build__noinline(0w8192,nil),0w0)
val () = Thread.spawn work (fn a => Thread.spawn work (fn b =>
  (check(eq(Thread.get a,0w33558528)); check(eq(Thread.get b,0w33558528)))))
val () = prim("printStringML","parallel ML passed\n")
