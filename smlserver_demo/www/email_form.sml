fun genForm () = `
<form method=post action=email_sent.sml>
^(SmlsDict.d "Type your email") <input type=text name=from><p>
^(SmlsDict.d "Type subject") <input type=text name=subject><p>
^(SmlsDict.d "Type body")
  <textarea name=body cols=80 rows=5>
  </textarea>
<center>
<input type=submit value="^(SmlsDict.d "Send Message")">
</center>
</form>`

val _ = case SmlsLogin.user_lang of
  SmlsLang.English => Ns.return 
    (`<html>
     <head>
     <title>Mail Me</title>
     </head>
     <body bgcolor=white>

     <h1>Mail Me</h1>

     With <a href="http://www.smlserver.com">SMLserver</a> you can mail from
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
     and we are pleased to receive your comments, ideas, bug reports etc.<p>

       ` ^^ (genForm()) ^^ `

       Back to the <a href="../auth_example.sml">authentication example</a> page<p>
       <hr>
       <a href="http://www.smlserver.org/">SMLserver Home Page</a> 
       (<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-09-23
       
       </body>
       </html>`)
  |  SmlsLang.Danish => Ns.return 
       (`
	<html>
	<head>
	<title>Send Mig Mail</title>
	</head>
	<body bgcolor=white>

	<h1>Send Mig Mail</h1>

	Med <a href="http://www.smlserver.com">SMLserver</a> kan du sende
	emails fra dine SML-programmer ved brug af SMLservers indbyggede
	<code>send</code> kommando. Du kan således lave web-services som
	sender emails til dig eller andre. På sin simple form tager funktionen
	<code>Ns.Mail.send</code> fire argumenter:
	<pre>
	Ns.Mail.send to from subject body
	</pre>

	Det første argument skal indeholde modtagerens email-adresse, det
	andet argument skal indeholde afsenderens email-adresse. For at
	functionen ikke skal fejle (exception <code>Fail</code> rejses) er det
	vigtigt at de to første argumenter er på formen
	<i>name</i>@<i>domain</i>. De to sidste argumenter er strenge, som
	skal indeholde henholdsvis beskedens emne (subject) og den egentlige
	besked.<p>

	Nedenfor kan du sende en mail til <b>mlkit@it.edu</b>, og sende dine
	kommentarer, ideer, fejlrapporter m.m.<p>

	` ^^ (genForm()) ^^ `
	
	Link til <a href="../auth_example.sml">eksempelsiden</a> med validering.<p>
	<hr>
	<a href="http://www.smlserver.org/">SMLserver Home Page</a> 
	(<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-09-23
	
	</body>
	</html>`)
