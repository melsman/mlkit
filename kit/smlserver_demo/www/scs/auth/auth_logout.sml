val target = ScsConfig.scs_site_index_page()

val _ = Ns.write 
`HTTP/1.0 302 Found
Location: ^(Ns.Conn.location())^target
MIME-Version: 1.0
^(Ns.Cookie.deleteCookie{name="session_id",path=SOME "/"})
^(Ns.Cookie.deleteCookie{name="auth_user_id",path=SOME "/"})


You should not be seeing this!`
