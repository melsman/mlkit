     fun map f [] = []
       | map f (x::xs) = f(x) :: map f xs
    
     val x = map (fn x => x+1) [7,11]
