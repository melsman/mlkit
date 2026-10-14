fun compute__noinline (0, acc:int) = acc
  | compute__noinline (n, acc) = compute__noinline (n-1, (acc+n) mod 1000003)
exception CallbackFailure
fun leaf__noinline (x:int) =
  let val n = compute__noinline (30000000, x)
  in (prim ("tp_throw", CallbackFailure) : int) handle CallbackFailure => n
  end
val () = _export ("tp_leaf", leaf__noinline)
fun hook__noinline (x:int) =
  let val n = compute__noinline (30000000, x)
      val result:int = prim ("tp_inner", n)
  in result+1
  end
val () = _export ("tp_hook", hook__noinline)
fun outer__noinline (x:int) : int = prim ("tp_outer", x)
val result = outer__noinline 1
val result = compute__noinline (30000000, result)
val keep = List.tabulate (200000, fn i => i)
fun allocate__noinline n =
  if n = 0 then result
  else
    let val xs = List.tabulate (10000, fn i => i mod 1024)
        val sum = List.foldl (op +) 0 xs
    in allocate__noinline (n-1)+sum
    end
val result = (prim ("tp_throw", CallbackFailure) : int) handle CallbackFailure => result
val result = compute__noinline (30000000, result)
val result = allocate__noinline 3000
val () = if List.length keep = 200000 then () else raise Fail "lost GC roots"
val () = TimeProfile.pause ()
val () = print ("attribution done " ^ Int.toString result ^ "\n")
