val kind = FormVar.wrapExn (FormVar.getEnumErr ["WhileUsed","ForAWhile","Size"]) "kind"
  handle _ => "Size"

val cache = 
  let
    val k =
      case kind of
	"WhileUsed" => NsCacheV2.WhileUsed 20
       | "ForAWhile" => NsCacheV2.ForAWhile 20
       | "Size" => NsCacheV2.Size 100
  in
    NsCacheV2.get ("users",
		   k,
		   NsCacheV2.String, 
		   NsCacheV2.pair NsCacheV2.Int NsCacheV2.String)
  end

val new_p = (* new_p true if new value added *)
  case (Ns.Conn.formvar "email", Ns.Conn.formvar "name", Ns.Conn.formvar "uid") of
    (SOME email, SOME name, SOME uid) => 
      NsCacheV2.insert(cache,email,(Option.valOf(Int.fromString uid),name))
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
  



