fun wait (n:int) : unit = prim("tp_wait", n)
fun masked () : unit = prim("tp_masked_wait", ())
val () = TimeProfile.start ()
val () = wait 100
val () = TimeProfile.flush ()
val () = TimeProfile.pause ()
val () = wait 50
val () = TimeProfile.start ()
val () = masked ()
val () = wait 50
val () = TimeProfile.flush ()
val () = print "recording done\n"
val () = List.app (fn _ => (wait (if CommandLine.arguments () = ["slow"] then 1200 else 20); TimeProfile.flush ())) [1,2,3,4,5]
