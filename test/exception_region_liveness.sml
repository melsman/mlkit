(* Values live across normal and exceptional exits must survive region helpers.
 * In particular, handler returns fetch pre-existing integer and real values. *)
exception Inner of int
exception Outer of int
fun build__noinline n = List.tabulate (n, fn i => i)
fun raise__noinline n =
    let val xs = build__noinline n
    in raise Inner (List.length xs)
    end
fun exercise__noinline (n, fail) =
    let
        val integer = n + 17
        val realValue = Real.fromInt n + 0.25
        val observed = ref 0
        val result =
            ((let val xs = build__noinline n
                  val () = observed := List.length xs
              in if fail then raise__noinline n else List.length xs + 1
              end
              handle Inner k =>
                  let val ys = build__noinline (k + 1)
                  in raise Outer (List.length ys)
                  end)
             handle Outer k => k)
        val after = build__noinline (n + 2)
    in
        if result = n + 1 andalso !observed = n andalso
           integer = n + 17 andalso
           Real.== (realValue, Real.fromInt n + 0.25) andalso
           List.length after = n + 2 then ()
        else raise Fail "exception region liveness"
    end
fun realResult__noinline (n, fail) =
    let
        val keep = Real.fromInt n + 0.5
        val result =
            (let val xs = build__noinline n
             in if fail then raise Inner (List.length xs) else keep + 0.25
             end
             handle Inner k => Real.fromInt k + 0.75)
    in
        if Real.== (result + keep, 2.0 * Real.fromInt n + 1.25) then ()
        else raise Fail "real handler result"
    end
val n = 10000 + List.length (CommandLine.arguments ())
val () = List.app (fn fail => (exercise__noinline (n, fail);
                              realResult__noinline (n, fail))) [false, true]
val () = print "exception region liveness: OK\n"
