
  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  := o
  type 'a ref = 'a ref

  fun op = (x: ''a, y: ''a): bool = prim ("=", (x, y))

  fun print (s:string) : unit = prim("printStringML", s)
  fun printNum (n:int):unit = prim("printNum",n)

val x =
 let
   exception E
   val z = (if (2=2) then (raise E) else print "E not raised, error\n")
     handle E => print "E raised, ok\n"
(*
   exception F of int
   val y = (if (2=2) then (raise F 3) else print "F not raised\n")
     handle F _ => print "F raised\n"

   exception G of int
   exception G' of int

   val x = 2
   val y = 3

   val q =
     (if (2=2) then raise G'(x + y) else y - x)
	handle G' x => (print "G' Raised\n"; x) | G x => (print "G Raised\n"; x+2)

   val _ = printNum q*)

in
  () (*(z,y)*)
end;
