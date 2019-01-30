let
  fun print (s:string) : unit = prim("printStringML", s)
  fun !(x: 'a ref): 'a = prim ("!", x)
  infix  3  := o
  fun (x: 'a ref) := (y: 'a): unit = prim (":=", (x, y))
  val r = ref "Hej med dig"
in
  print (!r);
  r := "Hej igen";
  print (!r)
end
