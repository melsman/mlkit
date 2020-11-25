local
fun print (s:string) : unit = prim("printStringML", s)
datatype t = A of real | B
fun pp p = case p of
               A _ => "OK\n"
             | B => "ERR\n"
in
val () = print "test: "
val b = A 1.0
val () = print (pp b)
end
