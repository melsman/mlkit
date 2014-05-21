structure PolySet =
  struct
    type 'a set = 'a list
    val empty = []
    fun singleton x = [x]
    fun mem(x,[]) = false
      | mem(x,y::ys) = x=y orelse mem(x,ys)
    fun union(s1,[]) = s1
      | union(s1,x::s2) = if mem(x,s1) then union(s1,s2)
			  else x::union(s1,s2)
  end
