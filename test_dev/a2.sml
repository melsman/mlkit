local 
(*  fun print (s:string) : unit = prim("printString", "printString", s) *)
  val _ = print "world\n"
  val a = "This is also a string\n"
  val _ = print a
in

end
