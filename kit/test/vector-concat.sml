(* Written by Stephen Weeks (sweeks@acm.org). *)

structure Main =
   struct
      fun doit () =
	 let
	    val len = 100
	    val sum = 2 * len * (len - 1)
	    fun loop n =
	       if n < 0
		  then ()
	       else
		   let val v = Vector.tabulate (len, fn i => (i,i))
		   in
		       if sum = 			   
			   Vector.foldl (fn ((x,y),a) => x+y+a) 0 (Vector.concat [v, v])
			   then loop (n - 1)
		       else raise Fail "bug"
		   end
	    val numTrials = 10000
	 in loop numTrials
	 end
   end

val _ = Main.doit()