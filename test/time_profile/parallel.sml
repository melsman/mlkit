fun spin__noinline (0, acc) = acc
  | spin__noinline (n, acc) = spin__noinline(n-1,(acc+n) mod 1000003)
fun work__noinline () = spin__noinline(40000000,0)
fun flushWork 0 = ()
  | flushWork n = (spin__noinline(1000000,0); TimeProfile.flush (); flushWork(n-1))
fun launch 0 = 0
  | launch n = Thread.spawn work__noinline (fn t =>
      let
        val rest = launch(n-1)
        val () = flushWork 4
      in rest+Thread.get t
      end)
val () = TimeProfile.pause ()
val () = TimeProfile.start ()
val result = launch 4
val () = TimeProfile.flush ()
val () = TimeProfile.pause ()
val () = TimeProfile.start ()
val again = launch 4
val () = TimeProfile.pause ()
val () = if result = again then print "parallel sampling ok\n" else raise Fail "parallel result"
