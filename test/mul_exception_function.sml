(* Keep the real arrow effects inside an exception's argument type. *)
exception E of unit -> int
fun walk (n:int) : unit =
    if n = 0 then
        let val xs = List.tabulate (20, fn i => i+1)
        in raise E (fn () => List.foldl op+ 0 xs)
        end
    else List.app walk [n-1]
val callback = (walk 5; fn () => 0) handle E f => f
val () = print (Int.toString (callback ()) ^ "\n")
