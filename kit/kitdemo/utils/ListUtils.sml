structure ListUtils =
  struct
    fun pr_list (l,d,r) pr list =
      let fun loop [] = r
	    | loop [x] = pr x ^ r
	    | loop (x::xs) = pr x ^ d ^ loop xs
      in l ^ loop list
      end
  end