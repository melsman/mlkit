functor questionnaire2 (F : sig val male : bool Obj.obj
				val name : string Obj.obj
				val email: string Obj.obj
			    end) : SCRIPTLET =
    struct
	open Scripts infix &&

        fun sexFromMale true = "Male"
	  | sexFromMale false = "Female"

	val response = 
	    case (Obj.valOf F.male, Obj.valOf F.name, Obj.valOf F.email) of
		(SOME male, SOME name, SOME email) =>
		    Page.page "Your Answer" 
		    (table (tr (th ($"Sex:") && td ($(sexFromMale male))) &&
			    tr (th ($"Name:") && td ($name)) &&
			    tr (th ($"Email:") && td ($email)))
		     )
	      | _ => Page.page "Error" (p($"Wrong form variables"))
    end
