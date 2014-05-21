   fun foldl f b xs = 
     let fun loop([], b) = b
           | loop(x::xs, b) = loop(xs,f x b)
     in
         loop(xs,b)
     end
