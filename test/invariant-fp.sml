(* Immutable coordinates may be lifted; mutable fields and failed matches may not. *)
datatype node = Empty | Node of {x: real, y: real, count: int ref}
fun check true = ()
  | check false = raise Fail "invariant FP arguments"
fun sum (n, iterations) =
  case n of
      Empty => 0.0
    | Node {count,...} =>
      let
        val start = !count
        fun loop (i, total) =
          if i = 0 then total
          else
            case n of
                Empty => raise Fail "node changed"
              | Node {x,y,count} =>
                let val c = !count
                    val () = count := c + 1
                in loop (i-1,total+x+y+Real.fromInt c)
                end
      in loop (iterations,Real.fromInt start)
      end
val n = Node {x = 1.5,y = 2.5,count = ref 0}
val () = check (Real.==(sum(n,4),22.0))
val () = check (Real.==(sum(n,2),21.0))
val () = check (Real.==(sum(Empty,3),0.0))
fun guarded (n,k) =
  let fun loop (i,total) =
        if i = 0 then total
        else case n of Empty => raise Fail "empty"
                     | Node {x,...} => loop(i-1,total+x)
  in loop(k,0.0)
  end
val () = check (Real.==(guarded(Empty,0),0.0))
val () = check ((guarded(Empty,1); false) handle Fail "empty" => true | _ => false)
val () = check (Real.==(guarded(n,3),4.5))
val () = print "Invariant FP arguments passed\n"
