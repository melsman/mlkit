val target = 
  case FormVar.wrapOpt FormVar.getStringErr "target" of
    SOME t => t
  | NONE => Ns.Conn.location() ^ "/auth/admin/show_cookies.sml" (* Default target url *)

val _ = Ns.return (`
<html>
<head>
<title>SMLserver Authentication</title>
</head>
<body bgcolor=white>
<form action=auth.sml method=post>
  <table>
   <input type=hidden name=target value="^target">
    <tr><td><b>Login</b></td>
    <td><input type=text name=auth_login size=20></td>
    </tr>
    <tr><td><b>Password</b></td>
    <td><input type=password name=auth_password size=20></td>
    </tr>
    <tr><td colspan=2 align=center><input type=submit value=Login></td>
    </tr>
  </table>
</form>

Back to the <a href="/auth_example.sml">authentication example page</a>.<p>

</body>
</html>`)
