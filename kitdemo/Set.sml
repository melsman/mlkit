functor Set (eqtype elem (*total order*)
             val lt : elem * elem -> bool
	     val pr : elem -> string) 
     : SET where type elem = elem =
  struct
    type elem = elem
    type set = elem list
    val empty : set = []
    fun singleton e = [e]
    fun mem x l =
      let fun mem' [] = false
	    | mem' (y::ys) = if lt(y,x) then mem' ys
			     else not(lt(x,y))
      in mem' l
      end
    fun union(s1,s2) =
      let fun U (t as ([], [], acc)) = t
	    | U ([], y::ys, acc) = U([], ys, y::acc)
	    | U (x::xs, [], acc) = U(xs, [], x::acc)
	    | U (s1 as x::xs, s2 as y::ys, acc) =
	        U(if lt(x,y) then (xs, s2, x::acc)
		  else if lt(y,x) then (s1, ys, y::acc)
	          else (xs, ys, y::acc))
      in rev(#3(U(s1, s2, [])))
      end
    val pr = fn s => ListUtils.pr_list pr s
  end