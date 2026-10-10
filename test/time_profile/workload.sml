fun start () : int = prim("tp_start", ())
fun stop () : int = prim("tp_stop", ())
fun busy () : int = prim("tp_busy", ())
fun sleep () : int = prim("tp_sleep", ())
fun loop (0, a) = a
  | loop (n, a) = loop (n-1, (a+1) mod 1000000)
fun churn 0 = ()
  | churn n =
    let val xs = List.tabulate (10000, fn i => i+n)
        val total = List.foldl (fn (i, a) => (a+i) mod 1000000) 0 xs
    in if total >= 0 then churn (n-1) else raise Fail "churn"
    end
val keep = List.tabulate (200000, fn i => i)
val _ = start ()
val _ =
  case CommandLine.arguments () of
      ["ml"] => print (Int.toString (loop (100000000, 0)) ^ "\n")
    | ["gc"] => churn 3000
    | ["c"] => ignore (busy ())
    | ["sleep"] => ignore (sleep ())
    | _ => raise Fail "expected ml, gc, c, or sleep"
val _ = stop ()
val _ = if List.length keep = 200000 then () else raise Fail "lost live data"
