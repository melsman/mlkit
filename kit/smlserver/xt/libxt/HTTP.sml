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
		val redirect : string -> string option -> response
	    end
    end

