(* Susp -- support for lazy evaluation in Moscow ML 1995-05-22 *)
(*      -- modified for the ML Kit, 2001-06-07                 *)

structure Susp : SUSP =
  struct
    datatype 'a thunk = VAL of 'a | THUNK of unit -> 'a

    type 'a susp = 'a thunk ref

    fun delay (f : unit -> 'a) : 'a susp = 
      ref (THUNK f) 

    fun force (su : 'a susp) : 'a = 
	case !su of
	    VAL v   => v 
	  | THUNK f => let val v = f () 
		       in su := VAL v; v 
		       end
  end
