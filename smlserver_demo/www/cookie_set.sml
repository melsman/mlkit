val cv =
  case FormVar.getString "cookie_value" of
    NONE => "No Cookie Value Specified"
  | SOME cv => cv

val cn =
  case FormVar.getString "cookie_name" of
    NONE => "CookieName"
  | SOME cn => cn

val clt =
  case FormVar.getInt "cookie_lt" of
    NONE => 60
  | SOME clt => clt

val cs =
  case FormVar.getString "cookie_secure" of
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

