functor time_of_day () : SCRIPTLET =
    struct
	open Scripts infix &

	val time_of_day = 
	    Date.fmt "%H.%M.%S" (Date.fromTimeLocal(Time.now()))

	val response = Http.returnHtml
	    (html(head ("Time of day",nil),
		  body (  h1 ($"Time of day") 
			& p($("The time of day is " ^ time_of_day)) 
			& hr() 
			& address ($"Served by SMLserver")
			)
		  )
	     )
    end