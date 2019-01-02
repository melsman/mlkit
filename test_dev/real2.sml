infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun printReal (n:real):unit = prim("printReal",n)
fun print (s:string) : unit = prim("printStringML",s)

infix ==
val epsilon = 0.000666
fun r1 == r2 =
    let val _ = printReal r1
        val _ = printReal r2
	val r = (r1 - r2)
	val _ = printReal r
	val r_abs = abs r
	val _ = printReal r_abs
    in r_abs < epsilon (*no perfect world*)
    end
fun error b s = print ((if b then "Ok - " else "Error - ") ^ s ^ "...\n")
val b = (4.0 + 3.0 == 7.0)
val _ = if b then print "True\n" else print "False\n"
val _ = error (4.0 + 3.0 == 7.0) "+"

val b = 4.0 < 3.0

val _ = if b then print "ERROR\n" else print "OK\n"
