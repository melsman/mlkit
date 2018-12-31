let
  infix -
  infix  3  := o
  fun !(x: 'a ref): 'a = prim ("!", x)
  fun print (s:string) : unit = prim("printStringML", s)

  fun printNum (i:int) : unit = prim("printNum", i)
  val r = ref 42
  val free = !r
  fun f 0 = printNum free
    | f n = (printNum n;f (n-1))
in
  f 10
end
