fun error s = (Ns.return ("Missing formvariable " ^ s);
	       raise Fail "Missing formvar")

fun formVar (k:string) : string =
  case Ns.Conn.getQuery()
    of SOME s => (case Ns.Set.get(s,k)
		    of SOME v => v
		     | NONE => error k)
     | NONE => error k

val to = formVar "to"
val from = formVar "from"
val subject = formVar "subject"
val body = formVar "body"

val _ = Ns.mail {to=to, from=from,subject=subject,body=body}

val _ =
Ns.returnQuot `
<html>
<body bgcolor=lightgreen>
<h2>Email has been sent</h2>
Email with subject "^subject" has been sent to ^to.<p>
<a href=mail_form.sml>Send another?</a>
</body>
</html>`
