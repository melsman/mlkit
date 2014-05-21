   fun foldl f b xs = 
     let fun loop(p as ([], b)) = p
           | loop(x::xs, b) = loop(xs,f x b)
     in
         #2(loop(xs,b))
     end
