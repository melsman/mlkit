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
		  Ns.Cache.Triple Ns.Cache.String Ns.Cache.String Ns.Cache.Int,
		  "triple",
		  k)
  end

fun pp_kind kind =
  case kind of
    "Size" => `<b>^kind</b> of size 100`
  | _ => `<b>^kind</b>. Entries live in the cache in
    approximately 20 seconds.<p>`

fun returnPage s = Page.return "Caching Demonstration"  
  (`^s <p>

  Using cache kind: ` ^^ (pp_kind kind) ^^ `<p>

  Pretty printing the cache: 
  <pre>
  ^(Ns.Cache.pp_cache cache)
  </pre><p>
 
  Go back to <a href=cache.sml?kind=^kind>Cache Demo Home Page</a>.`)

val _ = (* new_p is true if new value added *)
  case Ns.Conn.formvar "email"
      of NONE => Ns.returnRedirect "cache.sml"
       | SOME email => 
	returnPage
	(case Ns.Cache.lookup cache email
	   of SOME (lastname,firstnames,uid) => "Name for " ^ email ^ 
	     " is: (" ^ firstnames ^ "," ^ lastname ^ "," ^ (Int.toString uid) ^ ")"
	 | NONE => "No name in cache for " ^ email)
	   


