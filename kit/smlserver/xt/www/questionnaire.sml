functor questionnaire () : SCRIPTLET =
    struct
	open Scripts infix &&

	fun head s = tha (A.align A.left) ($s)
	val radioGroup = radioDrop
	    (tr (head "Male:"  && td(inputRadio' questionnaire2.male (Obj.fromBool true))) &&
	     tr (head "Female:" && td(inputRadio questionnaire2.male (Obj.fromBool false))))

        val response = 
	    Page.page "Please answer the following form" 
	    (questionnaire2.form
	     (table (swap(tr (head "Name:" && td(inputText questionnaire2.name NONE)) &&
			  swap(tr (head "Email:" && td(inputText questionnaire2.email NONE)) &&
			       radioGroup)) &&
		     tr (tda (A.colspan 2) (inputSubmit "Submit information")))))
    end
