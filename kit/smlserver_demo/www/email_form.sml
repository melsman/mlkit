fun genForm () = `
<form method=post action=email_sent.sml>
Type your email <input type=text name=from><p>
Type subject <input type=text name=subject><p>
Type body 
<textarea name=body cols=80 rows=5></textarea>
<center><input type=submit value="Send Message">
</center>
</form>`

val _ = Page.return "Mail Me"
    (`With <a href="http://www.smlserver.com">SMLserver</a> you can mail from
     within your SML-scripts. You use the SMLserver function
     <code>send</code>. You can, for instance, make a web-services that
     sends emails til you or others. In the simplest form, the function takes 
     four arguments
     <pre>
     Ns.Mail.send to from subject body
     </pre>
     The first argument must contain the receivers email address, the
     second argument must contain the senders email address. The email
     addresses must be properly formatted (i.e.,
     <i>name</i>@<i>domain</i>); otherwise the function raises the
     exception <code>Fail</code> rejses). The last two arguments are
     strings containing the subject and body of the message to
     send.<p>
     Below, you can mail the <a href="mailto:mlkit@it.edu">ML Kit team</a>, 
     and we are pleased to receive your comments, ideas, bug reports etc.<p>` 
     ^^ (genForm()) ^^ 
     `Back to the <a href="../auth_example.sml">authentication example</a> 
     page<p>`)
