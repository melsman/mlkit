infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  o

fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun !(x: 'a ref): 'a = prim ("!", x)

fun nil @ ys     = ys
  | (x::xs) @ ys = x :: (xs @ ys)

fun print (s:string) : unit = prim("printStringML", s)

fun (f o g) x = f(g x)

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

fun go () =
    let
      fun g f = op o let val x = f()
                     in (fn x => () (*print(#a x)*), fn () => x)
                     end
      val h = g (fn () => {a=op ^ ("uggh","boo\n")})
      val () = print "now working\n"
      val _ = work 20
      val () = print "done working\n"
    in h ()
    end

val () = go ()

val () = print "done\n"
