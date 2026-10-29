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
(* One full buffer suffices for the blocked-drain check; normal lifecycle
 * scenarios still exercise repeated flushes. *)
val slow = CommandLine.arguments () = ["slow"]
val () = List.app (fn _ => (wait (if slow then 1200 else 20); TimeProfile.flush ()))
                  (if slow then [1] else [1,2,3,4,5])
