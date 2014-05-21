functor temp2 (F : sig val tempC : int Form.var 
		   end) : SCRIPTLET =
    struct
	open Scripts infix &

        fun calculate c =
	    $(Int.toString c ^ " degrees Celcius equals " 
	      ^ Int.toString ( Int.div(9*c,5) + 32 )
	      ^ " degrees Fahrenheit.")

        val tempC = Page.get "Temperature" F.tempC

        val response = 
	    Page.page "Temperature Conversion (result)" 
	    (  p ( calculate tempC ) 
	     & p ( $"Go " & temp.link ($"again") & $"?"))
    end
