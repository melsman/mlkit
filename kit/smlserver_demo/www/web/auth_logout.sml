val target = "/web/link/index.sml"

val _ = Web.Cookie.deleteCookie{name="auth_password",path=SOME "/"}
val _ = Web.Cookie.deleteCookie{name="auth_person_id",path=SOME "/"}

val _ = Web.returnRedirect (Web.Conn.location() ^ target)
