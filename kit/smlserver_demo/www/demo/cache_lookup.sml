  val cache = Ns.Cache.findTm ("people", 20)

  fun returnPage s = Page.return "Caching Demonstration"  
    `^s <p> 
     Go back to <a href=cache.sml>Cache Demo Home Page</a>.`

  val _ = (* new_p is true if new value added *)
    case Ns.Conn.formvar "email"
      of NONE => Ns.returnRedirect "cache.sml"
       | SOME email => 
    returnPage
    (case Ns.Cache.get(cache,email)
       of SOME n => "Name for " ^ email ^ " is: " ^ n
        | NONE => "No name in cache for " ^ email)



