val getString = FormVar.wrapFail FormVar.getStringErr
val getEmail = FormVar.wrapFail FormVar.getEmailErr

val to      = getEmail ("to", "To")
val from    = getEmail ("from", "From")
val subject = getString ("subject", "Subject")
val body    = getString ("body", "Body")

val _ = Ns.Mail.send {to=to,from=from,subject=subject,body=body}

val _ = Page.return "Email has been sent"
  `Email with subject "^subject" has been sent to ^to.<p>
  <a href=mail_form.sml>Send another?</a>`
