val target = "/demo/link/index.sml"

val _ = Ns.write 
`HTTP/1.0 302 Found
Location: ^(Ns.Conn.location())^target
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="auth_password",path=SOME "/"})
^(Ns.Cookie.deleteCookie{name="auth_person_id",path=SOME "/"})


You should not be seeing this!`
