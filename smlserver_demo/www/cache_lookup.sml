
val cache = Ns.Cache.findTm ("people", 20)

fun returnPage s = Ns.Quot.return `
<html>
<body bgcolor=white>
<h2>^s</h2>
Go back to <a href=cache.sml>Cache Demo Home Page</a>.
<hr><i>Served by SMLserver</i>
</body>
</html>`

val _ = (* b true if new value is added to *)
  case Ns.Conn.formvar "login"
    of NONE => Ns.returnRedirect "cache.sml"
     | SOME login => 
      case Ns.Cache.get(cache,login)
	of SOME p => 
	  returnPage ("Password for " ^ login ^ " is: " ^ p)
	 | NONE => 
	  returnPage ("No password in cache for " ^ login)



