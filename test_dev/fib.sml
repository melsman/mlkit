infix +
infix -
infix <
infix >


local
  fun print (s:string) : unit = prim("printStringML", s)
  fun neq (x,y) = if x<y then false else if x>y then false else true
  fun printNum (i:int) : unit = prim("printNum", i)
  fun fib x =
    let
      val _ = print "In FIB\n"
      val _ = printNum x
    in
      if neq(x,0) orelse neq(x,1) then
	1
      else
	fib(x-2) + fib(x-1)
    end

(*  fun fib x = if eq(x,0) orelse eq(x,1) then 1 else fib(x-2)+fib(x-1)*)
in
  val _ = print "Before fib\n"
  val _ = printNum(fib 10)
  val _ = print "After fib\n"
end
