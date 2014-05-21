(* Written by Stephen Weeks (sweeks@acm.org). *)

structure Main =
   struct
      open Vector
	 
      fun rev v =
	 let
	    val n = length v
	 in
	    tabulate (n, fn i => sub (v, n - 1 - i))
	 end

      fun doit () =
	 let	    
	    fun loop n =
	       if n < 0
		  then ()
	       else
		   let val v = tabulate (10000, fn i => (i,i))
		   in
		       if 0 = #1 (sub (rev (rev v), 0))
			   then loop (n - 1)
		       else raise Fail "bug"
		   end
	 in loop 10000
	 end
   end

val _ = Main.doit()