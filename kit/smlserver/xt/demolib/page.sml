structure Page =
    struct
	local 
	    open Scripts infix &&
	    val smlserver_link = Unsafe.ahref {src="http://www.smlserver.org"} ($"SMLserver")
	in
	    fun pageWithCookies cookies t bdy =		
		Http.returnHtml' cookies 
		(html(head (t,nil),
		      body (h1 ($t) 
			    && bdy 
			    && hr() 
			    && address ($"Served by " && smlserver_link))))
	    fun page t bdy = pageWithCookies nil t bdy
	end
    end