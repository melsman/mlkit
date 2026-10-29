fun compute__noinline (0, acc:int) = acc
  | compute__noinline (n, acc) = compute__noinline (n-1, (acc+n) mod 1000003)
fun allocate__noinline 0 = 0
  | allocate__noinline n =
    let
      val xs = List.tabulate (10000, fn i => i mod 1024)
      val sum = List.foldl (op +) 0 xs
    in
      (sum mod 1024)+allocate__noinline (n-1)
    end
fun busy__noinline () : int = prim ("tp_measure_busy", 1)
fun sleep__noinline () : int = prim ("tp_measure_sleep", 0)
fun phase (name, work) =
    let
      val () = TimeProfile.start ()
      val result = work ()
      val () = TimeProfile.pause ()
    in
      print (name ^ " " ^ Int.toString result ^ "\n")
    end
val keep = List.tabulate (200000, fn i => i)
(* Enough work for timer samples and multiple collections, without making the
 * attribution matrix a benchmark. Each phase is checked for actual samples. *)
val () = phase ("compute", fn () => compute__noinline (5000000, 0))
val () = phase ("allocate", fn () => allocate__noinline 150)
val () = phase ("c", busy__noinline)
val () = phase ("sleep", sleep__noinline)
val () = if List.length keep = 200000 then () else raise Fail "lost live data"
