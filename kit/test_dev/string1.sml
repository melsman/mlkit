let
  infix  7  * / div mod
  infix  6  + - ^
  infixr 5  :: @
  infix  4  = <> > >= < <=
  infix  3  := o
  infix  0  before

  fun print (s:string) : unit = prim("printString", "printString", s)
  
  fun (s : string) ^ (s' : string) : string = prim ("concatString", "concatStringProfiling", (s, s'))

  val _ = print ("Hej" ^ " " ^ "Igen" ^ "\n")
in
  ()
end
