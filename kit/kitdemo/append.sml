   infixr 5 @
   fun [] @ ys = ys
     | (x::xs) @ ys = x :: (xs @ ys)
   val l = [1] @ [2,3]
