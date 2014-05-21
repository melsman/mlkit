functor data () : SCRIPTLET =
    struct
	open Scripts infix &

	val response = Http.returnHtml
	    (html(head ("Data persistence test",nil),
		  body (  h1 ($"Data persistence test") 
			& table (  tr(th($"Test") & th($"Lib") & th($"Script"))
				 & tr(td($"v1") & td($Data0.v1) & td($(Data0.eval Data0.d1))) 
				 & tr(td($"v2") & td($Data0.v2) & td($(Data0.eval Data0.d2))))
			& hr() 
			& address ($"Served by SMLserver")
			)
		  )
	     )
	    
	val a = ( (* Data0.f(); *) Data0.r := Data0.Int 200000)
    end