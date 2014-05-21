structure FV = FormVar

fun redirect() = 
  (Web.log (Web.Notice,"Redirecting from auth");
   Web.returnRedirect (Web.Conn.location() ^ Auth.loginPage); 
   Web.exit())

val target = case FV.wrapOpt FV.getStringErr "target" 
	       of NONE => redirect()
		| SOME t => t

val email = case FV.wrapOpt FV.getStringErr "email" 
	      of NONE => redirect()
	       | SOME e => e

val passwd = case FV.wrapOpt FV.getStringErr "passwd" 
	       of NONE => redirect()
		| SOME p => p

val pid =
  case Db.zeroOrOneField `select person_id 
                          from person 
                          where email = ^(Db.qqq email)` 
    of NONE => "0"
     | SOME pid => pid

val _ = Web.Cookie.deleteCookie{name="auth_person_id",path=SOME "/"}
val _ = Web.Cookie.deleteCookie{name="auth_person_id",path=SOME "/"}
val _ = Web.Cookie.setCookie{name="auth_person_id", value=pid,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false}
val _ = Web.Cookie.deleteCookie{name="auth_password",path=SOME "/"}
val _ = Web.Cookie.setCookie{name="auth_password", value=passwd,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false}

val _ = Web.returnRedirect target

