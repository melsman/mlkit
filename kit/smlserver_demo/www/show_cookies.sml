fun datefmt date = Date.fmt "%a, %d-%b-%Y %H:%M:%S GMT" date
val % = SmlsDict.d SmlsLang.English

val _ = Ns.return 
  (`
   <html>
   <body bgcolor=white>` ^^
   (case SmlsLogin.user_lang of
      SmlsLang.Danish => `
	Dette er en kodeordsbeskyttet side.<p>

	Du er logget på med bruger id: ^(Int.toString SmlsLogin.user_id)<p>

	<h1>Vis Cookies</h1>

	Det nuværende tidsstempel er (GMT): <b>^(datefmt(Date.fromTimeUniv(Time.fromSeconds(Time.toSeconds(Time.now())+60))))</b><p>

	Nedenfor kan du se de to cookier som anvendes ved validering.<p>` 
    | SmlsLang.English => `
	This is a password protected page.<p>

	You are logged in with user_id: ^(Int.toString SmlsLogin.user_id)<p>

	<h1>Show Cookies</h1>

	Current GMT date is: <b>^(datefmt(Date.fromTimeUniv(Time.fromSeconds(Time.toSeconds(Time.now())+60))))</b><p>
	
	Here, you can see the two cookies used for authentication.<p>`) ^^

      `
      <blockquote>
      <pre>`
      ^^ (List.foldl (fn ((n,v),a) => `^n == ^v <br>` ^^ a) `` Ns.Cookie.allCookies) ^^ `
      </pre>
      </blockquote>

      <form method=post action="../auth_logout.sml">
      <input type=submit value="^(% "Logout")">
      </form><p>

      <p>
      Tilbage til <a href="auth_example.sml">^(% "authentication example")</a> siden.<p>
      <hr>
      <a href="http://www.smlserver.org/">^(% "SMLserver Home Page")</a> 
      (<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-09-23
      
      </body>
      </html>`)
