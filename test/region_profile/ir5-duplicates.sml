(* One exception binding lowers to two allocations with the same IR site. *)
fun make__noinline () = let exception E in E end
val exceptions = List.tabulate (100, fn _ => make__noinline ())
fun sample () : unit = prim ("mlkit_rp_sample", ())
val () = sample ()
val () = if length exceptions = 100 then print "duplicated site ok\n"
         else raise Fail "exceptions"
