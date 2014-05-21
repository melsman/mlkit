val kind = FormVar.wrapExn (FormVar.getEnumErr ["WhileUsed","TimeOut","Size"]) "kind"
  handle _ => "Size"

val cache = 
  let
    val k =
      case kind of
	       "WhileUsed" => Web.Cache.WhileUsed (SOME(Time.fromSeconds 20), SOME(10000))
       | "TimeOut" => Web.Cache.TimeOut (SOME(Time.fromSeconds 20), SOME(10000))
       | "Size" => Web.Cache.WhileUsed (NONE, SOME(10000))
  in
    Web.Cache.get (Web.Cache.String, 
		  Web.Cache.Triple Web.Cache.String Web.Cache.String Web.Cache.Int,
		  "triple",
		  k)
  end

val new_p = (* new_p true if new value added *)
  case (Web.Conn.formvar "email", Web.Conn.formvar "firstnames", Web.Conn.formvar "lastname",Web.Conn.formvar "uid") of
    (SOME email, SOME firstnames, SOME lastname,SOME uid) => 
      Web.Cache.insert(cache,email,(lastname,firstnames,Option.getOpt(Int.fromString uid,0)),NONE)
  | _ => false

val head = if new_p then "New Value added" 
	   else "Key already in Cache"

val _ = Page.return "Caching Demonstration"  
  (`^head <p>

 ` ^^ (*` Pretty printing the cache: 
  <pre>
  ^(Web.Cache.pp_cache cache)
  </pre><p> ` ^^*) `

  Go back to <a href=cache.sml?kind=^kind>Cache Demo Home Page</a>.`)
  



