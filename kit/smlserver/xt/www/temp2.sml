functor temp2 (F : sig val tempC : int Obj.obj end) : SCRIPTLET =
    struct
	open Scripts infix && ++

        fun calculate c =
	    $(Int.toString c ^ " degrees Celcius equals " 
	      ^ Int.toString ( Int.div(9*c,5) + 32 )
	      ^ " degrees Fahrenheit.")

        val response = 
	    case Obj.valOf F.tempC of
		NONE => 
		    Page.page "Error" 
		    (p($"Go back and enter an integer!"))
	      | SOME tempC => 
		    Page.page "Temperature Conversion (result)" 
		    (   p ( calculate tempC ) 
		     && p ( $"Go " && temp.link ($"again") && $"?"))
    end
