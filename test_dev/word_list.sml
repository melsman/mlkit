infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun print (s:string) : unit = prim("printStringML", s)
fun printNum (n:int):unit = prim("printNum", n)

fun len nil = 0
  | len (x::xs) = 1 + len xs

val () = print "counting\n"

val x = [0w1,0w2,0w5,0wxFF,0wxFFFF,0wxFFFFAAAA]

val () = printNum (len x)

val ws = [0w0,0w1,0w2,0w254,0w255,0w256,0w257,
	  0w0,0w0,0w0,0w0,0w0,
	  0w1,0w0,0w2,0w0,0w254,0w0,0w255,0w0,0w256,0w0,0w257,
	  0wxFFFFFFFC,0wxFFFFFFFD,0wxFFFFFFFE,0wxFFFFFFFF,
	  0w0,0wxFFFFFFFC,0w0,0wxFFFFFFFD,0w0,0wxFFFFFFFE,0w0,0wxFFFFFFFF,
	  0w0,0wxFFFFFFF0,0w0,0wxFFFFFFF1,0w0,0wxFFFFFFF2,0w0,0wxFFFFFFF3,
	  0w0,0wxFFFFFF00,0w0,0wxFFFFFF01,0w0,0wxFFFFFF02,0w0,0wxFFFFFF03,
	  0w0,0wxFF00FF00,0w0,0wxFF01FF01,0w0,0wxFF02FF02,0w0,0wxFF03FF03,
	  0wxFFFBFFFC,0wxFFFCFFFD,0wxFFFDFFFE,0wxFFFEFFFF]

val () = printNum (len ws)

val y = [1,2,5,256,~32768,32767,65535,~2147483648,2147483647]

val () = printNum (len y)
