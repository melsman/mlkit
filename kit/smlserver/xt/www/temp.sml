functor temp () : SCRIPTLET =
    struct
	open Scripts infix & % attr

	val response = 
	    Page.page "Temperature Conversion" 
	    (temp2.form 
	     (p(  $ "Enter a temperature in degrees Celcius:"
		& br()
		& (inputText attr (A.size 5)) (temp2.tempC, SOME (Form.Int 34))
		& inputSubmit "Compute Fahrenheit Temperature")))
    end
