
val cache = 
  case Ns.Cache.find "people"
    of SOME c => c
     | NONE => Ns.Cache.create ("people", 20)

val b = (* b true if new value is added to *)
  case (Ns.Conn.formvar "login", Ns.Conn.formvar "passwd")
    of (SOME login, SOME passwd) => 
      Ns.Cache.set(cache,login,passwd)
     | _ => false

fun returnPage s = Ns.Quot.return `
<html>
<body bgcolor=white>
<h2>^s</h2>
Go back to <a href=cache.sml>Cache Demo Home Page</a>.
<hr><i>Served by SMLserver</i>
</body>
</html>`

val _ = returnPage 
  (if b then "New Value added" else "Key already in Cache")



