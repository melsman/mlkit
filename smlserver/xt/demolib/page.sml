
local open Scripts
in
structure Page : 
    sig
	type 'attrs blockElt = (nil,nil,aclosed,formclosed,
				preclosed,block flow,'attrs) elt

	val pageWithCookies : SMLserver.Cookie.cookiedata list ->
	    string -> 'attrs blockElt -> Http.response
	val page : string -> 'attrs blockElt -> Http.response

	val get : string -> 't SMLserver.Form.var -> 't
    end
=
    struct
	type 'attrs blockElt = (nil,nil,aclosed,formclosed,
				preclosed,block flow,'attrs) elt
	local 
	    open Scripts infix &
	    val smlserver_link = Unsafe.ahref {src="http://www.smlserver.org"} ($"SMLserver")
	in
	    fun pageWithCookies cookies t bdy =		
		Http.returnHtml' cookies 
		(html(head (t,nil),
		      body (h1 ($t) 
			    & bdy 
			    & hr() 
			    & address ($"Served by " & smlserver_link))))
	    fun page t bdy = pageWithCookies nil t bdy

	    fun error t bdy = Http.respondExit (page t bdy)

	    fun get (s:string) (fv:'t SMLserver.Form.var) : 't =
		case SMLserver.Form.get fv of
		    SMLserver.Form.Ok v => v
		  | SMLserver.Form.Wrong => 
			error "Form Field error" 
			(p($"Error in the field '" & b ($s) & 
			   $"'. Go back using your browser's back button and enter a correct value for this field."))
		  | SMLserver.Form.Missing => 
			error "Missing form variable error" 
			(p($"Error: Missing form variable - someone is tampering with the system!"))
	end
    end
end