val _ = Ns.return `
<html>
<head>
<title>Checking Form Variables</title>
</head>
<body bgcolor=white>
<h2>Checking Form Variables</h2><p>
<form method=post action=formvar_chk.sml>
Type an integer: <input type=text name=int><p>
Type a positive integer: <input type=text name=nat><p>
Type a real: <input type=text name=real><p>
Type a numeral: <input type=text name=num><p>
Type a string: <input type=text name=str><p>
Type a positive integer in the range [2,...,10]: <input type=text name=range><p>
<input type=submit value="Submit Entry">
</form>
<hr>
<a href="http://www.smlserver.org/">SMLserver Home Page</a> 
(<a href="mailto:mlkit@it.edu">mlkit@it.edu</a>) 2001-09-20
</body>
</html>`
