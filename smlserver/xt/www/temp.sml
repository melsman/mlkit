functor temp () : SCRIPTLET =
    struct
	open Scripts infix && ++

	val response = 
	    Page.page "Temperature Conversion" 
	    (temp2.form 
	     (p( $ "Enter a temperature in degrees Celcius:"
		&& br()
		&& inputText temp2.tempC NONE
		&& inputSubmit "Compute Fahrenheit Temperature")))
    end
