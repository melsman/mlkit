val _ = Ns.return `
<html>
<body bgcolor=white>
This is a password protected page.<p>

You are logged in with user_id: ^(Int.toString (auth_verify_user()))<p>

<hr>

<form method=post action="../auth_logout.sml">
  <input type=submit value="Logout">
</form><p>

</body>
</html>`