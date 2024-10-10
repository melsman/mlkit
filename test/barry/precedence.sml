
fun f x =
    if !(ref false) then f x
    else let val a = ref 4
         in fn y => x + y * 2 - ((2 - x) + !a)
         end

val () = print (Int.toString (f 8 9) ^ "\n")
