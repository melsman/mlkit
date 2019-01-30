val () =
let
(*  infix -
  infix  3  := o

  fun printNum (i:int) : unit = prim("printNum", "printNum", i)
  fun f 0 = printNum free
    | f n = (printNum n;f (n-1))
*)
  fun print (s:string) : unit = prim("printStringML", s)
in
  print "Hello\n"
end
