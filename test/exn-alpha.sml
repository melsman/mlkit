infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))

fun nil @ ys     = ys
  | (x::xs) @ ys = x :: (xs @ ys)

fun print (s:string) : unit = prim("printStringML", s)

fun f (x: 'a) : exn =
    let exception E of 'a
    in E x
    end

val e : exn =
    let val y = "hi" ^ "there"
    in f y
    end

fun length xs =
    let fun acc [] k = k
	  | acc (x::xr) k = acc xr (k+1)
    in acc xs 0
    end

fun work n =
    if n <= 3 then [n]
    else
    let val xs = work (n-1)
        val ys = work (n-2)
        val zs = work (n-3)
        val zs = ((xs @ ys) @ zs) @ []
        val n = length zs
    in zs @ [n]
    end

val () = print "now working\n"
val _ = work 20
val () = print "done working\n"
