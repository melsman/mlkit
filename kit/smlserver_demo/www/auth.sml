val _ = Ns.log (Ns.Notice,"Entering auth.sml")

fun redirect() = 
  (Ns.log (Ns.Notice,"Redirecting from auth");
   Ns.returnRedirect (Ns.Conn.location() ^ "/auth_form.sml"); Ns.exit())

val target =
  case FormVar.wrapOpt FormVar.getStringErr "target" of
    NONE => redirect()
  | SOME t => t

val al =
  case FormVar.wrapOpt FormVar.getStringErr "auth_login" of
    NONE => redirect()
  | SOME al => al

val ap =
  case FormVar.wrapOpt FormVar.getStringErr "auth_password" of
    NONE => redirect()
  | SOME ap => ap

val user_id =
  case Db.zeroOrOneField `select user_id from auth_user where login = '^(Db.qq al)'` of
    NONE => "0"
  | SOME user_id => user_id

val _ = Ns.log(Ns.Notice, "auth.sml: " ^ target ^ " and user id = " ^ user_id)
val _ = Ns.write
`HTTP/1.0 302 Found
Location: ^target
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="auth_user_id",path=SOME "/"})
^(Ns.Cookie.setCookie{name="auth_user_id", value=user_id,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false})
^(Ns.Cookie.deleteCookie{name="auth_password",path=SOME "/"})
^(Ns.Cookie.setCookie{name="auth_password", value=ap,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false})


You should not be seeing this!`

