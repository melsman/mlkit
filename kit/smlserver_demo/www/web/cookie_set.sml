structure FV = FormVar

val cv = case FV.wrapOpt FV.getStringErr "cookie_value" 
	   of NONE => "No Cookie Value Specified"
	    | SOME cv => cv

val cn = case FV.wrapOpt FV.getStringErr "cookie_name" 
	   of NONE => "CookieName"
	    | SOME cn => cn

val clt = case FV.wrapOpt FV.getIntErr "cookie_lt" 
	    of NONE => 60
	     | SOME clt => clt

val cs = case FV.wrapOpt FV.getStringErr "cookie_secure" 
	   of SOME "Yes" => true
	    | _  => false

val expiry = let open Time Date
	     in fromTimeUniv(now() + fromSeconds clt)
	     end

val cookie = Web.Cookie.setCookie 
              {name=cn, value=cv, expiry=SOME expiry,
	       domain=NONE, path=SOME "/", secure=cs}

val _ = Web.Conn.returnRedirectWithCode(302, "cookie.sml")
