structure Http : HTTP_EXTRA =
    struct
	type html = XHtml.html
	type response = string
	fun returnHtml h =
	    ("HTTP/1.0 200 OK\n\
	     \MIME-Version: 1.0\n\
	     \Content-type: text/html\n\n" ^ 
	     XHtml.Unsafe.toString h)

	structure Unsafe =
	    struct
		fun toString x = x

		fun redirect link cookieOpt =
		    case cookieOpt of
			SOME c =>
			    ("HTTP/1.0 302 Found\n\
			     \Location: " ^ link ^ "\n\
			     \MIME-Version: 1.0\n" ^ c ^ "\n\
			     \\n\
			     \You should not be seeing this!")
		      | NONE => 
			     ("HTTP/1.0 302 Found\n\
			      \Location: " ^ link ^ "\n\
			      \MIME-Version: 1.0\n\
			      \\n\
			      \\n\
			      \You should not be seeing this!")
	    end
    end
