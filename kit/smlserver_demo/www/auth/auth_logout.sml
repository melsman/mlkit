val _ = Ns.log (Ns.Notice, "auth_logout: " ^ ("http://localhost:8005/index.msp"))
val _ = Ns.write 
`HTTP/1.0 302 Found
Location: http://localhost:8005/index.msp
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="auth_password",path=SOME "/auth"})
^(Ns.Cookie.deleteCookie{name="auth_user_id",path=SOME "/auth"})


You should not be seeing this!`
