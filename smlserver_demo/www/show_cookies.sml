fun datefmt date = Date.fmt "%a, %d-%b-%Y %H:%M:%S GMT" date

(* What is going on here Niels??? mael 2001-12-19 *)
val time_s = Time.toSeconds (Time.now()) + 60
val date = (datefmt o Date.fromTimeUniv o Time.fromSeconds) time_s

fun layoutCookie ((n,v),acc) = `^n == ^v <br>` ^^ acc

val _ = Page.return "Show Cookies"
   (`This is a password protected page.<p> You are logged 
     in with person_id: ^(Int.toString Login.person_id)<p>
     Current GMT date is: <b>^date</b><p> Here, you can see 
     the two cookies used for authentication.<p>
     <blockquote>
     <pre>` ^^ 
     (List.foldl layoutCookie `` Ns.Cookie.allCookies) ^^ 
    `</pre>
     </blockquote>
     <form method=post action="../auth_logout.sml">
     <input type=submit value="Logout">
     </form>
     <p><p>
     Back to <a href="auth_example.sml">authentication 
     example</a> page.`)
