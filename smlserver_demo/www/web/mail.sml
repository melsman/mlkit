  structure FV = FormVar

  val (to,errs)   = FV.getEmailErr ("to", "To", FV.emptyErr)
  val (from,errs) = FV.getEmailErr ("from", "From", errs)
  val (subj,errs) = FV.getStringErr ("subject", "Subject", errs)
  val (body,errs) = FV.getStringErr ("body", "Body", errs)
  val () = FV.anyErrors errs

  val _ = Web.Mail.send {to=to, from=from,
			subject=subj, body=body}

  val _ = Page.return "Email has been sent"
    `Email with subject "^subj" has been sent to ^to.<p>
    <a href=mail_form.sml>Send another?</a>`
