infix :: < <= div + - =

fun iota acc n =
    if n <= 0 then acc else iota ((n-1)::acc) (n-1)

fun len nil = 0
  | len (x::xs) = 1 + len xs

fun app (n:int) (acc:int) =
    if n <= 0 then acc else
    let val g = if (n div 2) = 0 then fn x => len(iota nil (x+2))
		else fn x => len(iota nil (x+1))
	val () = _export("appML", g)
        val acc :int = prim("appML", acc)
    in app (n-1) acc
    end

val _ = print ("Result: "
               ^ Int.toString(app 8 100)
               ^ "\n")
