fun tabulate (n, f) =
    let fun h (i, acc) =
            if i<n then h (i+1, f i :: acc) else acc
    in if n<0 then raise Size else List.rev(h(0, nil))
    end

fun println s = print (s ^ "</br>")

val store : (int -> int) ref = ref (fn x => x)

fun tailr nil acc = acc
  | tailr (x::xs) acc =
    ( if acc < 100 then store := (fn y => y+acc) else ()
    ; tailr xs (acc+x))

val xs = tabulate (10000, fn x => x div 3 + x div 100)

fun prl s l = println (s ^ ": [" ^ String.concatWith "," (map Int.toString l) ^ "]")

(*val () = prl "xs" xs*)
val x = tailr xs 0
val y = !store 0

val () = println ("x= " ^ Int.toString x)
val () = println ("y= " ^ Int.toString y)

val () = println(if y < 100 then "OK" else "ERR")
