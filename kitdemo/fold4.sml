  local
    fun foldl f e [] = e
      | foldl f e (x::xs) = foldl f (f(x,e)) xs
  in
    val a = foldl (op +) 0 [0,1,2]
  end
