
    fun f [] = []
      | f (x::xs) = x+1 :: f xs

    val n = length(let val l1 = [1,2,3]
                   in if true then f l1 else l1
	           end)



