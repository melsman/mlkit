functor count (F : sig val c : int Obj.obj end) : SCRIPTLET =
    struct
	open Scripts infix && ++

        fun action s c =
	    td (count.form (p(inputHidden count.c (Obj.fromInt c)
			      && inputSubmit s)))
	val response = 
	    case Obj.valOf F.c of
		NONE => Page.page "Error" (p($"I expect an integer"))
	      | SOME c => 
		    Page.page ("Count: " ^ Int.toString c)
		    (table (tr (action "Up"   (c+1) ++ 
				action "Down" (c-1))
			    )
		     )
    end
