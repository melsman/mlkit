let
  infix -
  infix  3  := o
  infixr 5  :: @
  fun !(x: 'a ref): 'a = prim ("!", "!", x) 
  fun print (s:string) : unit = prim("printStringML", "printStringML", s)

  fun printNum (i:int) : unit = prim("printNum", "printNum", i)

  fun app f [] = ()
    | app f (x::xs) = (f x; app f xs)
in
  app (if false then (fn i => printNum (i-1)) else (fn i => printNum (i-2))) [1,2,3]
end
