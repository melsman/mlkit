val kind = FormVar.wrapExn (FormVar.getEnumErr ["WhileUsed","TimeOut","Size"]) "kind"
  handle _ => "Size"

val cache = 
  let
    val k =
      case kind of
	"WhileUsed" => NsCacheV2.WhileUsed 20
       | "TimeOut" => NsCacheV2.TimeOut 20
       | "Size" => NsCacheV2.Size 100
  in
    NsCacheV2.get ("userlist",
		   k,
		   NsCacheV2.String, 
		   NsCacheV2.List NsCacheV2.String)
  end

val new_p = (* new_p true if new value added *)
  case (Ns.Conn.formvar "email", Ns.Conn.formvar "firstnames", Ns.Conn.formvar "lastname") of
    (SOME email, SOME firstnames, SOME lastname) => 
      NsCacheV2.insert(cache,email,[lastname,firstnames])
  | _ => false

val head = if new_p then "New Value added" 
	   else "Key already in Cache"

val _ = Page.return "Caching Demonstration V2"  
  `^head <p>

  Pretty printing the cache: 
  <pre>
  ^(NsCacheV2.pp_cache cache)
  </pre><p>

  Go back to <a href=cache_v2.sml?kind=^kind>Cache Demo Home Page</a>.`
  



