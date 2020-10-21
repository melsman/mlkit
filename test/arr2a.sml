
local
  val a = Array2.array (100,10,23)
  fun set(r,c,v) = Array2.update(a,r,c,v)
  fun get(r,c) = Array2.sub(a,r,c)
  val () = set(5,8,24)
  val () = print (Int.toString (get (50,7)) ^ "\n")
  val () = print (Int.toString (get (5,8)) ^ "\n")
in
end
