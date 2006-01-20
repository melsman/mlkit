val kind = Option.valOf (Web.Conn.formvar "kind") handle _ => "Size"

val cache = 
  let
    val (k,name) =
      case kind of
         "WhileUsed" => (Web.Cache.WhileUsed (SOME(Time.fromSeconds 20),SOME(10000)),"users1")
       | "TimeOut" => (Web.Cache.TimeOut (SOME(Time.fromSeconds 20), SOME(10000)),"users2")
       | "Size" => (Web.Cache.WhileUsed (NONE, SOME(10000)),"users3")
  in
    Web.Cache.get (Web.Cache.String, 
		  Web.Cache.Pair Web.Cache.Int Web.Cache.String,
		  name, k)
  end

val new_p = (* new_p true if new value added *)
  case (Web.Conn.formvar "email", Web.Conn.formvar "name", Web.Conn.formvar "uid", 
        Web.Conn.formvar "timeout") of
    (SOME email, SOME name, SOME uid, SOME timeout) => 
      Web.Cache.insert(cache,email,(Option.getOpt(Int.fromString uid,0) ,name), 
      Option.map Time.fromSeconds (LargeInt.fromString timeout))
  | _ => false

val head = if new_p then "New Value added" 
	   else "Key already in Cache"

val _ = Page.return ("Caching Demonstration" ^ ": cache_add.sml")
  (`^head <p>

` (*^^  `Pretty printing the cache: 
  <pre>
  ^(Web.Cache.pp_cache cache)
  </pre><p> `*) ^^ `

  Go back to <a href=cache.sml?kind=^kind>Cache Demo Home Page</a>.`)
  



