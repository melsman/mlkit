
(* Modified version of the MONO_SET signature given in the Edinburgh
   library. *)

signature MONO_SET_PP =
sig
    include MONO_SET

    val layoutSet : {start: string, sep: string, finish: string} 
		    -> (elt -> PrettyPrint.StringTree) 
		    -> Set 
		    -> PrettyPrint.StringTree
end

signature MONO_SET_EXT =
sig
    include MONO_SET_PP
    		       
    val pu : Set Pickle.pu
end

functor MonoSetPP (S : MONO_SET) : MONO_SET_PP =
struct 
  open S
  local structure PP = PrettyPrint
  in fun layoutSet {start, sep, finish} layoutItem s =
	 PP.NODE {start=start,
		  finish=finish,
		  indent=3,
		  childsep=PP.RIGHT sep,
		  children=List.map layoutItem (list s)}
  end      
end
