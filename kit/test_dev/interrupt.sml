val n = ref 0
fun loop () =
  (n := ((!n + 1) handle Overflow => 0);
    loop ())
    
fun f () = (loop () 
	    handle Interrupt => (print (Int.toString (!n) ^ "\n"); f ()))
  
val _ = f ()
  