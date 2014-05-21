structure ListUtils =
  struct
    fun pr_list pr list =
      let fun loop [] = "}"
	    | loop [x] = pr x ^ "}"
	    | loop (x::xs) = pr x ^ "," ^ loop xs
      in "{" ^ loop list
      end
  end