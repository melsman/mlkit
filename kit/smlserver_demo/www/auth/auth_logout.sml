val _ = Ns.write 
`HTTP/1.0 302 Found
Location: ^(Ns.Conn.location())/auth/auth_form.sml
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="auth_password",path=SOME "/auth"})
^(Ns.Cookie.deleteCookie{name="auth_user_id",path=SOME "/auth"})


You should not be seeing this!`
