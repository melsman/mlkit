val _ = Ns.Quot.return `
<html>
<head>
<title>SMLserver Authentication</title>
</head>
<body>
<form action=auth.sml method=post>
  <table>
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
</body>
</html>`
