signature HTTP =
    sig
	type response
	type html
	val returnHtml : html -> response
    end

signature HTTP_EXTRA =
    sig
	include HTTP
        structure Unsafe :
	    sig		
		val toString : response -> string
		val redirect : string -> SMLserver.Cookie.cookiedata list -> response
	    end
    end

