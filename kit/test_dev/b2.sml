
local
  fun print (s:string) : unit = prim("printString", "printString", s)
  val _ = print a
  val _ = (print (#1 b); print " "; print (#2 b))
in

end

