val saved = ref (List.tabulate (2000, fn i => (Int.toString i, i)));
val closure = let val xs = !saved in fn () => List.length xs end;
val constant = "static text in an earlier REPL image";
exception Earlier of string;
fun collect__noinline () = List.length (List.tabulate (3000, fn i => i));
fun hook (_:int) =
  let val () = prim ("repl_gc_in_callback", ())
      val n = collect__noinline ()
      val () = prim ("repl_gc_in_callback", ())
  in n
  end;
val () = _export ("repl_gc_hook", hook);
val () = prim ("repl_gc_request", ());
val _ = collect__noinline ();
val () = prim ("repl_gc_check", ());
val () = if closure () = 2000 andalso #1 (List.nth (!saved, 1234)) = "1234" then () else raise Fail "lost earlier roots";
val () = saved := [(constant, 42)];
val () = prim ("repl_gc_request", ());
val _ = collect__noinline ();
val () = prim ("repl_gc_check", ());
val () = if closure () = 2000 andalso #1 (hd (!saved)) = constant then () else raise Fail "lost updated roots";
val () = ((raise Earlier constant) handle Earlier s => if s = constant then () else raise Fail "lost exception");
val () = prim ("repl_gc_request", ());
val shown = List.tabulate (100, fn i => (Int.toString i, i));
val _ = collect__noinline ();
val () = prim ("repl_gc_check", ());
val () = prim ("repl_gc_callback", ());
val _ = collect__noinline ();
val () = prim ("repl_gc_check", ());
val () = if List.length shown = 100 then print "REPL GC images: OK\n" else raise Fail "lost printed value";
:quit;
