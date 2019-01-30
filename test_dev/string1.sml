let
  infix  7  * / div mod
  infix  6  + - ^
  infixr 5  :: @
  infix  4  = <> > >= < <=
  infix  3  := o
  infix  0  before

  fun print (s:string) : unit = prim("printStringML", s)

  fun (s : string) ^ (s' : string) : string = prim ("concatStringML", (s, s'))

  val _ = print ("Hej" ^ " " ^ "Igen" ^ "\n")
in
  ()
end
