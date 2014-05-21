infix * - + :: := before o

fun fac 0 = 1
  | fac n = n * fac(n-1);

val a = fac 3

fun sum (0,a) = a
  | sum (n,a) = sum(n-1,n+a)

val b = sum (10,0)

val c = [1,2,3,4]

fun len nil = 0
  | len (x::xs) = 1 + len xs

val d = len c

val ch_A : char = #"A"
val ch_a : char = #"a"

fun !(x: 'a ref): 'a = prim ("!", x) 
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y)) 
fun (f o g) x = f(g x)
fun a before () = a
fun ignore (a) = ()

val r = ref "hej"

val (rc1, rc2) = (!r, (r := "hejsa"; !r))

exception Hej of string * int

fun f():int = raise Hej ("elle belle", 4)

val i =
    f() handle Hej (_,i) => i


val b : int = raise Overflow
