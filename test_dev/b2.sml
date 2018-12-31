
local
  fun print (s:string) : unit = prim("printStringML", s)
  val () = print "Hi\n"
  val () = print a
  val () = (print (#1 b); print " "; print (#2 b))
in

end
