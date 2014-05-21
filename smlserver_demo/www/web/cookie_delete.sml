val cn =
  case FormVar.wrapOpt FormVar.getStringErr "cookie_name" 
    of NONE => "CookieName"
     | SOME cn => cn

val _ = Web.Cookie.deleteCookie{name=cn,path=SOME "/"}

val _ = Web.Conn.returnRedirectWithCode(302, "cookie.sml")
