
val cache = Ns.Cache.findTm ("people", 20)

val b = (* b true if new value is added to *)
  case (Ns.Conn.formvar "login", Ns.Conn.formvar "passwd")
    of (SOME login, SOME passwd) => 
      Ns.Cache.set(cache,login,passwd)
     | _ => false

val _ = Page.return "Caching Demonstration"  `
<h2>^(if b then "New Value added" else "Key already in Cache")</h2>
Go back to <a href=cache.sml>Cache Demo Home Page</a>.`
  



