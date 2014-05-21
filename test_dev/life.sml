(* implode, explode virker ikke i copy string *)
(*kitlife35u.sml*)

(*based on kitlifeopt.sml, but with copying
  to avoid many generations in the same region*)


local

infix  6  +
infixr 5  ::

fun print (x:string):unit = prim("printStringML","printStringML",x)
  

val bail = [0,1]
val _ = print "Before genB\n"

in
val genB = let val _ = print "1"
	       fun map [] = []
		 | map (a::x) = (print "."; a :: map x)
	   in map bail
	   end
end

