structure Page =
    struct
	local 
	    open Scripts infix &&
	    val smlserver_link = Unsafe.ahref {src="http://www.smlserver.org"} ($"SMLserver")
	in
	    fun page t bdy =
		Http.returnHtml(html(head (t,nil),
				     body (h1 ($t) 
					   && bdy 
					   && hr() 
					   && address ($"Served by " && smlserver_link))))
	end
    end