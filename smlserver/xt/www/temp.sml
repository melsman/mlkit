functor temp () : SCRIPTLET =
    struct
	open Scripts infix & % attr

	val response = 
	    Page.page "Temperature Conversion" 
	    (temp2.form 
	     (p(  $ "Enter a temperature in degrees Celcius:"
		& br()
		& inputTexta (A.size 5) temp2.tempC NONE
		& inputSubmit "Compute Fahrenheit Temperature")))
    end
