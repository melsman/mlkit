val kind = FormVar.wrapExn (FormVar.getEnumErr ["WhileUsed","TimeOut","Size"]) "kind"
  handle _ => "Size"

val cache = 
  let
    val k =
      case kind of
	"WhileUsed" => Ns.Cache.WhileUsed 20
       | "TimeOut" => Ns.Cache.TimeOut 20
       | "Size" => Ns.Cache.Size 100
  in
    Ns.Cache.get (Ns.Cache.String, 
		  Ns.Cache.List Ns.Cache.String,
		  "userlist",
		  k)
  end

val new_p = (* new_p true if new value added *)
  case (Ns.Conn.formvar "email", Ns.Conn.formvar "firstnames", Ns.Conn.formvar "lastname") of
    (SOME email, SOME firstnames, SOME lastname) => 
      Ns.Cache.insert(cache,email,[lastname,firstnames])
  | _ => false

val head = if new_p then "New Value added" 
	   else "Key already in Cache"

val _ = Page.return "Caching Demonstration"  
  (`^head <p>

` ^^(* `  Pretty printing the cache: 
  <pre>
  ^(Ns.Cache.pp_cache cache)
  </pre><p> ` ^^*) `

  Go back to <a href=cache.sml?kind=^kind>Cache Demo Home Page</a>.`)
  



