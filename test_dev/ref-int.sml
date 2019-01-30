

  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  := o
  type 'a ref = 'a ref

  fun !(x: 'a ref): 'a = prim ("!", x)
  fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))

  fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

  fun not true = false
    | not false = true

  fun a <> b = not (a = b)
  fun (f o g) x = f(g x)
  fun print (s:string) : unit = prim("printStringML", s)
  fun printNum (n:int):unit = prim("printNum",n)

local
  val counter = ref 0
in
  fun inc() = (counter := !counter + 1; !counter)
  fun succ() = (counter := !counter -1; !counter)
end

fun loop 0 f = f ()
  | loop n f = (f ();
		loop (n-1) f)

val res = (loop 10 (fn () => printNum(inc()));
	   loop 10 (fn () => printNum(succ())))


val counter = ref 0
fun inc () = (counter := (!counter + 1);
	      printNum(!counter);
	      !counter)

val res = (loop 1000 inc;
	   loop 1000 inc;
	   loop 1000 inc)
