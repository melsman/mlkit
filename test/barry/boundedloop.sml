(* A proven-safe integer increment must still barify as integer arithmetic. *)
fun indices n =
    let
      fun loop i =
          if i < n then i :: loop (i + 1)
          else []
    in
      loop 0
    end

val () =
    if indices 4 = [0,1,2,3] andalso indices 0 = []
    then print "Bounded loop passed\n"
    else raise Fail "Bounded loop"
