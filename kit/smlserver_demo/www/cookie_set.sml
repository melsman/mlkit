val cv =
  case FormVar.wrapOpt FormVar.getStringErr "cookie_value" of
    NONE => "No Cookie Value Specified"
  | SOME cv => cv

val cn =
  case FormVar.wrapOpt FormVar.getStringErr "cookie_name" of
    NONE => "CookieName"
  | SOME cn => cn

val clt =
  case FormVar.wrapOpt FormVar.getIntErr "cookie_lt" of
    NONE => 60
  | SOME clt => clt

val cs =
  case FormVar.wrapOpt FormVar.getStringErr "cookie_secure" of
    SOME "Yes" => true
  | _  => false

val _ = Ns.write
`HTTP/1.0 302 Found
Location: http://localhost:8005/cookie.sml
MIME-Version: 1.0
^(Ns.Cookie.setCookie{name=cn, 
		      value= cv,
		      expiry=SOME(Date.fromTimeUniv(Time.fromSeconds(Time.toSeconds(Time.now())+clt))),
		      domain=NONE,
		      path=SOME "/",
		      secure=cs})

You should not be seeing this!`

