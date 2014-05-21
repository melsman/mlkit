infix <

fun print (s:string) : unit = prim("printStringML", "printStringML", s)
val _ = if ~1 < 0 then print "OK"
	else print "ERROR"
