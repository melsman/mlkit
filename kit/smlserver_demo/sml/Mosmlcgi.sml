structure Mosmlcgi : MOSML_CGI =
  struct
    fun cgi_field_string s =
      case Ns.Conn.getQuery(Ns.getConn())
	of SOME formvars => Ns.Set.get(formvars, s)
	 | NONE => NONE
    val cgi_remote_addr = NONE
  end