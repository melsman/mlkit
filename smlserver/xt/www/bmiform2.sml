functor bmiform2 () : SCRIPTLET =
    struct
	open Scripts infix & % attr

	val response = 
	    Page.page "BMI Form 2" 
	    (bmi.form 
             (swap (One())
	     (p($"Enter weight (in kg) " &
		inputTexta (A.size 5) bmi.w NONE & br() &
		$"Enter height (in cm) " &
		inputTexta (A.size 5) bmi.h NONE & 
		inputSubmit "Compute BMI"))))
    end
