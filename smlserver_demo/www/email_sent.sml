val % = ScsDict.d ScsLang.English

val to = "nh@it-c.dk"
val (from,errs) = ScsFormVar.getEmailErr ("from","your email",ScsFormVar.emptyErr)
val (subject,errs) = ScsFormVar.getStringErr ("subject","subject",errs)
val (body,errs) = ScsFormVar.getStringErr ("body","body",errs)
val _ = ScsFormVar.anyErrors errs

val _ = Ns.Mail.send{to=to,from=from,subject=subject,body=body}

val _ = Ns.return `
<html>
<head>
<title>^(%"Email Sent")</title>
</head>
<body bgcolor=white>
<h1>^(%"Email Sent")</h1>
^(%"Your email has been sent")<p>

^(%"Thank You").
</body>
</html>`