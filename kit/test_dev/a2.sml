local 
  fun print (s:string) : unit = prim("printString", "printString", s)
  val _ = print "world\n"
in

end
