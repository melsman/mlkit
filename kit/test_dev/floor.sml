    fun print (s:string) : unit = prim("printStringML", "printStringML", s)
    fun floor (x : real) : int = prim ("floorFloat", "floorFloat", x)    (* may raise Overflow *)
    val _ = floor (23.0E23) handle Overflow => 4 
    val _ = print "End\n";
