functor questionnaire2 (F : sig val male : bool option Form.var
				val name : string Form.var
				val email: string Form.var
			    end) : SCRIPTLET =
    struct
	open Scripts infix &

        fun sexFromMale true = "Male"
	  | sexFromMale false = "Female"

	val male = Page.get "Male" F.male
	val name = Page.get "Name" F.name
	val email = Page.get "Email" F.email

	val response = 
	    case male of
		SOME male =>
		    Page.page "Your Answer" 
		    (table (tr (th ($"Sex:") & td ($(sexFromMale male))) &
			    tr (th ($"Name:") & td ($name)) &
			    tr (th ($"Email:") & td ($email)))
		     )
	      | NONE =>
		    Page.page "Make a choice!" (p($"You must be either a male or a female"))
    end
