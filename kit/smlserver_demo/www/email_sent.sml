val to = "nh@it-c.dk"
val (from,errs) = FormVar.getEmailErr ("from","your email",FormVar.emptyErr)
val (subject,errs) = FormVar.getStringErr ("subject","subject",errs)
val (body,errs) = FormVar.getStringErr ("body","body",errs)
val _ = FormVar.anyErrors errs

val _ = Ns.Mail.send{to=to,from=from,subject=subject,body=body}

val _ = Ns.return `
<html>
<head>
<title>^(SmlsDict.d "Email Sent")</title>
</head>
<body bgcolor=white>
<h1>^(SmlsDict.d "Email Sent")</h1>
^(SmlsDict.d "Your email has been sent")<p>

^(SmlsDict.d "Thank You").
</body>
</html>`