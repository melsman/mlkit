functor countreload () : SCRIPTLET =
    struct
	open Scripts infix &&

	fun page i = 
	    let val c = {name="counter", value=Int.toString i,
			 expiry=NONE,domain=NONE,
			 path=NONE, secure=false} 
	    in Page.pageWithCookies [c] "Count Reloads"
		(p($("Count: " ^ Int.toString i)))
	    end

        val response =
	    case SMLserver.Cookie.getCookieValue "counter" of
		NONE => page 0
	      | SOME v => 
		    case Int.fromString v of
			SOME n => page (n+1)
		      | NONE => page 0
    end
