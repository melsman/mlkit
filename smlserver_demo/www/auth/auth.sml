fun redirect() = 
  (Ns.returnRedirect "http://localhost:8005/auth/auth_form.sml"; Ns.exit())

val target =
  case FormVar.getString "target" of
    NONE => redirect()
  | SOME t => t

val al =
  case FormVar.getString "auth_login" of
    NONE => redirect()
  | SOME al => al

val ap =
  case FormVar.getString "auth_password" of
    NONE => redirect()
  | SOME ap => ap

val user_id =
  case Db.zeroOrOneField `select user_id from auth_user where login = '^(Db.qq al)'` of
    NONE => redirect()
  | SOME user_id => user_id

val _ = Ns.log(Ns.Notice, "auth.sml: " ^ target)
val _ = Ns.write
`HTTP/1.0 302 Found
Location: ^target
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="auth_user_id",path=SOME "/auth"})
^(Ns.Cookie.setCookie{name="auth_user_id", value=user_id,expiry=NONE,
		      domain=NONE,path=SOME "/auth",secure=false})
^(Ns.Cookie.deleteCookie{name="auth_password",path=SOME "/auth"})
^(Ns.Cookie.setCookie{name="auth_password", value=ap,expiry=NONE,
		      domain=NONE,path=SOME "/auth",secure=false})


You should not be seeing this!`