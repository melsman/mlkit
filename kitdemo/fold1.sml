       fun foldl f b xs = 
         case xs of 
           [] => b
         | x::xs' => foldl f (f x b) xs'
