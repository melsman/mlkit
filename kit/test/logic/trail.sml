(* trail.sml *)

structure Trail = 
struct
  local
      open Term
      val global_trail = ref (nil : term option ref list)
      val trail_counter = ref 0
  in
      fun unwind_trail (0, tr) = tr
	| unwind_trail (n, r::tr) =
	  ( r := NONE ; unwind_trail (n-1, tr) )
	| unwind_trail (_, nil) =
	  raise BadArg "unwind_trail"

      fun reset_trail () = ( global_trail := nil )

      fun trail func =
	  let 
	      val tc0 = !trail_counter
	  in
	      ( func () ;
	       global_trail := 
	         unwind_trail (!trail_counter-tc0, !global_trail) ;
	       trail_counter := tc0 )
	  end
	
      fun bind (r, t) =
	  ( r := SOME t ;
	   global_trail := r::(!global_trail) ;
	   trail_counter := !trail_counter+1 )
  end (* local *)
end; (* Trail *)	

