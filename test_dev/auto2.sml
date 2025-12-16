local
infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

fun !(x: 'a ref): 'a = prim ("!", x)
fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))
fun not true = false | not false = true
fun a <> b = not (a = b)
fun print (s:string) : unit = prim("printStringML", s)
fun printNum (n:int) : unit = prim("printNum", n)
fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))
fun printCharArray (s:chararray) : unit = prim("printStringML", s)
fun chr (i:int) : char = if i>=0 andalso i<256 then prim ("id", i)
			 else raise Div
in

(* Auto-conversion with many arguments *)
local
  fun runtime_test1auto (a1:int,a2:int,a3:int,a4:int,a5:int,
                         a6:int,a7:int,a8:int,a9:int,a10:int) : int =
      prim("@runtime_test1auto", (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10))
  fun runtime_test2auto (a1:int,a2:int,a3:int,a4:int,a5:int,
                         a6:int,a7:int,a8:int,a9:int,a10:int,a11:int) : int =
      prim("@runtime_test2auto", (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11))
in
  fun x1 n = runtime_test1auto (1,2+n,3,4,5,6,7,8,9+n,10)
  fun x2 n = runtime_test2auto (1,2+n,3,4,5,6,7,8,9+n,10,11+n)

  fun ok1 n = 1096 + 3*n + 29*n
  fun ok2 n = ok1 n + 37*(11+n)

  fun test (x,ok) s n =
      let val r = ok n
      in if x n = r then print ("OK " ^ s ^ "\n")
         else print ("ERR " ^ s ^ ": result <> expected\n")
      end

  val () = test (x1,ok1) "1:0" 0
  val () = test (x1,ok1) "1:2" 2
  val () = test (x1,ok1) "1:23" 23
  val () = test (x2,ok2) "2:0" 0
  val () = test (x2,ok2) "2:12" 12
  val () = test (x2,ok2) "2:123" 123
end

end
