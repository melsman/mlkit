  val cache = Ns.Cache.findTm ("people", 20)

  val new_p = (* new_p true if new value added *)
    case (Ns.Conn.formvar "email", Ns.Conn.formvar "name")
      of (SOME email, SOME name) => 
        Ns.Cache.set(cache,email,name)
       | _ => false

  val head = if new_p then "New Value added" 
             else "Key already in Cache"

  val _ = Page.return "Caching Demonstration"  
    `^head <p>
     Go back to <a href=cache.sml>Cache Demo Home Page</a>.`
  



