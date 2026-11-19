(* Exercise bounded arithmetic and floating-point calls shared by native backends. *)
fun boundedModulo__noinline (c:char) = Char.ord c mod 7
fun trig__noinline x = Math.sin x + Math.cos x
val () = List.app (fn i =>
    if boundedModulo__noinline (Char.chr i) = i mod 7 then ()
    else raise Fail "bounded modulo") (List.tabulate (256, fn i => i))
val () = if Real.abs (trig__noinline 0.0 - 1.0) < 0.000001 then ()
         else raise Fail "native trigonometry"
val () = print "native optimisations passed\n"
