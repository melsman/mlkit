functor bmiform () : SCRIPTLET =
    struct
	open Scripts infix & % attr

	val response = 
	    Page.page "BMI Form" 
	    (bmi.form 
	     (p($"Enter height (in cm) " &
		inputTexta (A.size 5) bmi.h NONE & br() &
		$"Enter weight (in kg) " &
		inputTexta (A.size 5) bmi.w NONE & 
		inputSubmit "Compute BMI")))
    end
