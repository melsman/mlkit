functor count (F : sig val c : int Form.var 
		   end) : SCRIPTLET =
    struct
	open Scripts infix &

        fun action s c =
	    td (count.form (p(inputHidden count.c (Form.Int c)
			      & inputSubmit s)))

	val c = Page.get "Count" F.c

	val response = 
	    Page.page ("Count: " ^ Int.toString c)
	    (table (tr (action "Up"   (c+1) &
			action "Down" (c-1))
		    )
	     )
    end
