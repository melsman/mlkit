structure S :> sig type ('a,'b) t 
		   val f : ('a,'b) t -> ('b,'a) t
		   val mk : 'a * 'b -> ('a,'b) t 
	       end
	     =
	   struct
	     type ('a,'b) t = 'b * 'a
	     fun f (x,y) = (y,x)
	     fun mk (a,b) = (b,a)
	   end

val a = S.mk (5, "hello")

val b = S.f a


(* An example that fails to compile under the ML Kit Version 3;
 * reported by Mads. Problem fixed ME 1999-02-19. *)

signature SIG = sig type key 
		    val a : key
                end

functor j(type key') =
  struct
    type key = key'
    val a : key = raise Match
  end :> SIG; 

structure s = j(type key' = int):SIG
