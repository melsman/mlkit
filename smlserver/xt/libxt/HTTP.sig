signature HTTP =
    sig
	type response
	type html
	val returnHtml  : html -> response
	val returnHtml' : SMLserver.Cookie.cookiedata list 
                          -> html -> response
	val respondExit : response -> 'a
    end

(* 
 [respondExit r] sends a response to the client and exits; this
 function never returns.
*)

signature HTTP_EXTRA =
    sig
	include HTTP
        structure Unsafe :
	    sig		
		val toString : response -> string
		val redirect : string -> SMLserver.Cookie.cookiedata list -> response
	    end
    end

