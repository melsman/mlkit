structure FV = FormVar

fun redirect() = 
  (Ns.log (Ns.Notice,"Redirecting from auth");
   Ns.returnRedirect "/demo/auth_form.sml"; 
   Ns.exit())

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
                          where email = ^(Db.qq' email)` 
    of NONE => "0"
     | SOME pid => pid

val _ = Ns.write
`HTTP/1.0 302 Found
Location: ^target
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="auth_person_id",path=SOME "/"})
^(Ns.Cookie.setCookie{name="auth_person_id", value=pid,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false})
^(Ns.Cookie.deleteCookie{name="auth_password",path=SOME "/"})
^(Ns.Cookie.setCookie{name="auth_password", value=passwd,expiry=NONE,
		      domain=NONE,path=SOME "/",secure=false})


You should not be seeing this!`

