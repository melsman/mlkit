functor bmi () : SCRIPTLET =
    struct
	open Scripts infix &&

	val response = 
	    Page.page "Body Mass Index Form" 
	    (bmi2.form 
	     (p($"Enter your height (in cm) " &&
		inputText bmi2.h NONE &&
		br() &&
		$"Enter your weight (in kg) " &&
		inputText bmi2.w NONE &&
		br() &&
		inputSubmit "Compute Index")))
    end
