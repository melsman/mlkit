functor Set (eqtype elem (*total order*)
             val lt : elem * elem -> bool
	     val pr : elem -> string) : SET where type elem = elem =
  struct
    type elem = elem
    type set = elem list
    val empty : set = []
    fun singleton e = [e]
    fun mem x [] = false
      | mem x (y::ys) = if lt(y,x) then mem x ys
			else (not o lt)(x,y)
    fun union(s1,s2) =
      let fun U ([], s, acc) = rev acc @ s
	    | U (s, [], acc) = rev acc @ s
	    | U (s1 as x::xs, s2 as y::ys, acc) =
	        U(if lt(x,y) then (xs, s2, x::acc)
		  else if lt(y,x) then (s1, ys, y::acc)
	          else (xs, ys, y::acc))
      in U(s1, s2, [])
      end
    val pr = ListUtils.pr_list ("{",",","}") pr
  end