fun datefmt date = Date.fmt "%a, %d-%b-%Y %H:%M:%S GMT" date

val _ = Ns.Quot.return (`
<html>
<body bgcolor=white>

<h1>Show Cookies</h1>

Current GMT date is: <b>^(datefmt(Date.fromTimeUniv(Time.fromSeconds(Time.toSeconds(Time.now())+60))))</b><p>

Here, you can see the two cookies used for authentication.<p>

<hr>

<h3>The list of all cookies</h3>
<blockquote>
<pre>`
^^ (List.foldl (fn ((n,v),a) => `^n == ^v <br>` ^^ a) `` Ns.Cookie.allCookies) ^^ `
</pre>
</blockquote>
</body>
</html>
`)

