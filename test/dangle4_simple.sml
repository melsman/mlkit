fun f a = ()               (* f makes no use of its argument *)
fun g v = (fn () => f v)   (* g returns a closure containing v, 
			    * which is a pointer if v is boxed. *)
val h = g (2,3)            (* boxed tuple in finite region; h is now bound 
			    * to a closure containing a dangling pointer, 
			    * because region inference puts a letregion 
			    * around the call to g to hold the real. *)

(*
       fun f at r1 [] (a)= ()
       fun g at r1 [r7] (v)= (fn () => f[] v)at r7 
       val h = letregion r8 
               in g[r1] (2,3)at r8 
               end
       val h' = g[r1] (2,3)at r1


*)