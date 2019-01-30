local
  val b = ("Hej", "Med dig")
fun print (s:string) : unit = prim("printStringML", s)
in
val () = print (#1 b)
val () = print " "
val () = print (#2 b)
end
