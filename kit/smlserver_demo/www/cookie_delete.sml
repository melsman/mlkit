val cn =
  case FormVar.getString "cookie_name" of
    NONE => "CookieName"
  | SOME cn => cn

val _ = Ns.write
`HTTP/1.0 302 Found
Location: http://localhost:8005/cookie.sml
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name=cn,path=SOME "/"})

You should not be seeing this!`

